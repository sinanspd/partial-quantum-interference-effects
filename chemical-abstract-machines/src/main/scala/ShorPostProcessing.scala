package com.sinanspd

import scala.collection.mutable

/** A reduced non-negative rational used during phase reconstruction. */
private[sinanspd] final case class ShorRational private (
    numerator: Int,
    denominator: Int
) {
  def render: String = s"$numerator/$denominator"
}

private[sinanspd] object ShorRational {
  def apply(numerator: Int, denominator: Int): ShorRational = {
    require(denominator != 0, "A rational denominator cannot be zero")
    val sign = if (denominator < 0) -1 else 1
    val divisor = ShorPostProcessing.gcd(math.abs(numerator), math.abs(denominator))
    new ShorRational(
      sign * numerator / divisor,
      sign * denominator / divisor
    )
  }
}

final case class ShorPostProcessingResult(
    measuredValue: Int,
    phaseDenominator: Int,
    rationalCandidates: Vector[ShorRational],
    testedExponents: Vector[Int],
    successfulFraction: Option[ShorRational],
    successfulExponent: Option[Int],
    recoveredOrder: Option[Int],
    factors: Option[(Int, Int)],
    success: Boolean,
    message: String
) {
  def phaseEstimate: String = s"$measuredValue/$phaseDenominator"

  def candidateSummary: String =
    if (rationalCandidates.isEmpty) "none"
    else rationalCandidates.map(_.render).mkString(",")

  def testedExponentSummary: String =
    if (testedExponents.isEmpty) "none" else testedExponents.mkString(",")

  def factorSummary: String =
    factors.map { case (left, right) => s"$left x $right" }.getOrElse("none")

  def metadata: Vector[(String, String)] =
    Vector(
      "shorPhaseEstimate" -> phaseEstimate,
      "shorRationalCandidates" -> candidateSummary,
      "shorTestedExponents" -> testedExponentSummary,
      "shorSuccessfulFraction" -> successfulFraction.map(_.render).getOrElse("none"),
      "shorSuccessfulExponent" -> successfulExponent.map(_.toString).getOrElse("none"),
      "shorRecoveredOrder" -> recoveredOrder.map(_.toString).getOrElse("none"),
      "shorFactors" -> factorSummary,
      "shorPostProcessingSuccess" -> success.toString,
      "shorPostProcessingMessage" -> message
    )

  def printReport(modulus: Int, base: Int): Unit =
    println(
      s"""
         |=============== SHOR POST-PROCESSING ===============
         |problem:              factor N=$modulus with base a=$base
         |measured phase:       $phaseEstimate
         |rational candidates:  $candidateSummary
         |tested exponents:     $testedExponentSummary
         |successful fraction:  ${successfulFraction.map(_.render).getOrElse("none")}
         |successful exponent:  ${successfulExponent.map(_.toString).getOrElse("none")}
         |recovered order:       ${recoveredOrder.map(_.toString).getOrElse("none")}
         |factors:               $factorSummary
         |result:                ${if (success) "SUCCESS" else "FAILURE"}
         |message:               $message
         |======================================================""".stripMargin
    )
}

/**
  * Classical factor-recovery stage for one sampled phase-estimation outcome.
  *
  * Besides ordinary continued-fraction convergents, this includes intermediate
  * convergents that are still within half a measurement bin of the sampled
  * phase. It then tests bounded multiples of each reconstructed denominator.
  * Testing multiples recovers an order when the eigenphase numerator and order
  * have a common divisor, which is a standard source of otherwise discarded
  * Shor samples.
  */
final case class ShorPostProcessing(
    modulus: Int,
    base: Int,
    countingQubits: Int,
    maxIntermediateConvergents: Int = 4,
    maxDenominatorMultiple: Int = 8
) {
  require(modulus > 2, "Shor's modulus must be greater than two")
  require(base > 1 && base < modulus, "Shor's base must be between one and N")
  require(countingQubits > 0 && countingQubits < 31, "Invalid counting-register width")
  require(maxIntermediateConvergents > 0, "The intermediate-convergent bound must be positive")
  require(maxDenominatorMultiple > 0, "The denominator-multiple bound must be positive")

  val correctnessDescription: String =
    s"classical Shor post-processing must construct non-trivial factors of $modulus"

  val targetDescription: String = s"non-trivial factors p and q with p * q = $modulus"

  def process(observedBits: Vector[Boolean]): ShorPostProcessingResult = {
    require(
      observedBits.length == countingQubits,
      s"Expected $countingQubits Shor counting bits, received ${observedBits.length}"
    )

    val measuredValue =
      observedBits.foldLeft(0)((value, bit) => (value << 1) | (if (bit) 1 else 0))
    val phaseDenominator = 1 << countingQubits
    val immediateFactor = ShorPostProcessing.gcd(base, modulus)

    if (immediateFactor > 1 && immediateFactor < modulus) {
      val factors = ShorPostProcessing.orderedFactors(immediateFactor, modulus / immediateFactor)
      return ShorPostProcessingResult(
        measuredValue = measuredValue,
        phaseDenominator = phaseDenominator,
        rationalCandidates = Vector.empty,
        testedExponents = Vector.empty,
        successfulFraction = None,
        successfulExponent = None,
        recoveredOrder = None,
        factors = Some(factors),
        success = true,
        message = s"gcd($base, $modulus) produced a factor before order finding"
      )
    }

    if (measuredValue == 0) {
      return ShorPostProcessingResult(
        measuredValue = measuredValue,
        phaseDenominator = phaseDenominator,
        rationalCandidates = Vector.empty,
        testedExponents = Vector.empty,
        successfulFraction = None,
        successfulExponent = None,
        recoveredOrder = None,
        factors = None,
        success = false,
        message = "the zero phase contains no period information"
      )
    }

    val candidates =
      ShorPostProcessing.rationalCandidates(
        measuredValue,
        phaseDenominator,
        modulus,
        maxIntermediateConvergents
      )
    val attempts =
      ShorPostProcessing.exponentAttempts(candidates, modulus, maxDenominatorMultiple)
    val tested = mutable.ArrayBuffer.empty[Int]
    var success: Option[(ShorRational, Int, Int, (Int, Int))] = None
    val iterator = attempts.iterator

    while (iterator.hasNext && success.isEmpty) {
      val (fraction, exponent) = iterator.next()
      tested += exponent
      ShorPostProcessing.tryFactors(base, modulus, exponent).foreach { factors =>
        val order = ShorPostProcessing.smallestVerifiedOrder(base, modulus, exponent)
        success = Some((fraction, exponent, order, factors))
      }
    }

    success match {
      case Some((fraction, exponent, order, factors)) =>
        ShorPostProcessingResult(
          measuredValue = measuredValue,
          phaseDenominator = phaseDenominator,
          rationalCandidates = candidates,
          testedExponents = tested.toVector,
          successfulFraction = Some(fraction),
          successfulExponent = Some(exponent),
          recoveredOrder = Some(order),
          factors = Some(factors),
          success = factors._1 > 1 &&
            factors._2 > 1 &&
            factors._1 * factors._2 == modulus,
          message =
            s"verified a^$exponent mod N = 1 and recovered ${factors._1} x ${factors._2}"
        )

      case None =>
        ShorPostProcessingResult(
          measuredValue = measuredValue,
          phaseDenominator = phaseDenominator,
          rationalCandidates = candidates,
          testedExponents = tested.toVector,
          successfulFraction = None,
          successfulExponent = None,
          recoveredOrder = None,
          factors = None,
          success = false,
          message =
            if (candidates.isEmpty)
              "continued-fraction reconstruction found no plausible period denominator"
            else
              "no reconstructed denominator or bounded multiple produced non-trivial factors"
        )
    }
  }
}

private[sinanspd] object ShorPostProcessing {
  def gcd(left: Int, right: Int): Int = {
    @annotation.tailrec
    def loop(a: Int, b: Int): Int =
      if (b == 0) math.abs(a) else loop(b, a % b)

    loop(left, right)
  }

  def orderedFactors(left: Int, right: Int): (Int, Int) =
    if (left <= right) (left, right) else (right, left)

  private def continuedFraction(numerator: Int, denominator: Int): Vector[Int] = {
    val coefficients = mutable.ArrayBuffer.empty[Int]
    var currentNumerator = numerator
    var currentDenominator = denominator

    while (currentDenominator != 0) {
      coefficients += currentNumerator / currentDenominator
      val remainder = currentNumerator % currentDenominator
      currentNumerator = currentDenominator
      currentDenominator = remainder
    }

    coefficients.toVector
  }

  private def intermediateConvergents(
      coefficients: Vector[Int],
      maxIntermediateConvergents: Int
  ): Vector[ShorRational] = {
    val approximants = mutable.ArrayBuffer.empty[ShorRational]
    var numeratorMinusTwo = 0
    var numeratorMinusOne = 1
    var denominatorMinusTwo = 1
    var denominatorMinusOne = 0

    coefficients.zipWithIndex.foreach {
      case (coefficient, index) =>
        if (index > 0) {
          val multipliers =
            if (coefficient <= maxIntermediateConvergents) {
              1 to coefficient
            } else {
              (1 to maxIntermediateConvergents) :+ coefficient
            }
          multipliers.foreach { multiplier =>
            val numerator =
              multiplier * numeratorMinusOne + numeratorMinusTwo
            val denominator =
              multiplier * denominatorMinusOne + denominatorMinusTwo
            approximants += ShorRational(numerator, denominator)
          }
        }

        val numerator =
          coefficient * numeratorMinusOne + numeratorMinusTwo
        val denominator =
          coefficient * denominatorMinusOne + denominatorMinusTwo
        numeratorMinusTwo = numeratorMinusOne
        numeratorMinusOne = numerator
        denominatorMinusTwo = denominatorMinusOne
        denominatorMinusOne = denominator
    }

    approximants.toVector
  }

  private def withinHalfMeasurementBin(
      measuredValue: Int,
      phaseDenominator: Int,
      rational: ShorRational
  ): Boolean = {
    val difference = math.abs(
      measuredValue.toLong * rational.denominator.toLong -
        rational.numerator.toLong * phaseDenominator.toLong
    )
    2L * difference <= rational.denominator.toLong
  }

  def rationalCandidates(
      measuredValue: Int,
      phaseDenominator: Int,
      modulus: Int,
      maxIntermediateConvergents: Int
  ): Vector[ShorRational] = {
    val coefficients = continuedFraction(measuredValue, phaseDenominator)
    val candidates = intermediateConvergents(coefficients, maxIntermediateConvergents)
      .filter { rational =>
        rational.numerator > 0 &&
        rational.numerator < rational.denominator &&
        rational.denominator < modulus &&
        withinHalfMeasurementBin(measuredValue, phaseDenominator, rational)
      }
      .distinct

    candidates.sortBy { rational =>
      val error = math.abs(
        measuredValue.toDouble / phaseDenominator.toDouble -
          rational.numerator.toDouble / rational.denominator.toDouble
      )
      (error, rational.denominator)
    }
  }

  def exponentAttempts(
      candidates: Vector[ShorRational],
      modulus: Int,
      maxDenominatorMultiple: Int
  ): Vector[(ShorRational, Int)] = {
    val seenExponents = mutable.Set.empty[Int]

    candidates.flatMap { candidate =>
      (1 to maxDenominatorMultiple)
        .map(_ * candidate.denominator)
        .filter(_ < modulus)
        .collect {
        case exponent if seenExponents.add(exponent) => candidate -> exponent
      }
    }
  }

  private def modPow(base: Int, exponent: Int, modulus: Int): Int =
    BigInt(base).modPow(BigInt(exponent), BigInt(modulus)).toInt

  def tryFactors(
      base: Int,
      modulus: Int,
      exponent: Int
  ): Option[(Int, Int)] = {
    if (exponent <= 0 || exponent % 2 != 0 || modPow(base, exponent, modulus) != 1) {
      None
    } else {
      val halfPower = modPow(base, exponent / 2, modulus)
      val left = gcd(halfPower - 1, modulus)
      val right = gcd(halfPower + 1, modulus)

      Vector(left, right)
        .find(factor => factor > 1 && factor < modulus && modulus % factor == 0)
        .map(factor => orderedFactors(factor, modulus / factor))
    }
  }

  def smallestVerifiedOrder(
      base: Int,
      modulus: Int,
      successfulExponent: Int
  ): Int =
    (1 to successfulExponent)
      .find(exponent =>
        successfulExponent % exponent == 0 &&
          modPow(base, exponent, modulus) == 1
      )
      .getOrElse(successfulExponent)
}
