package com.sinanspd

import scala.collection.mutable

final case class NumericSummary(
    count: Int,
    mean: Double,
    standardDeviation: Double,
    confidenceLow95: Double,
    confidenceHigh95: Double,
    confidenceMethod: String
)

final case class HypothesisTestResult(
    statistic: Double,
    pValue: Double,
    effectEstimate: Double,
    sampleCount: Int
)

object TrialStatistics {
  def summarize(values: Vector[Double], proportion: Boolean = false): NumericSummary = {
    require(values.nonEmpty, "Cannot summarize an empty sample")
    val mean = values.sum / values.length.toDouble
    val standardDeviation =
      if (values.length == 1) 0d
      else {
        val squared = values.map(value => math.pow(value - mean, 2d)).sum
        math.sqrt(squared / (values.length - 1).toDouble)
      }

    if (proportion) {
      val z = 1.959963984540054
      val n = values.length.toDouble
      val denominator = 1d + z * z / n
      val center = (mean + z * z / (2d * n)) / denominator
      val halfWidth =
        z * math.sqrt(mean * (1d - mean) / n + z * z / (4d * n * n)) / denominator
      NumericSummary(
        values.length,
        mean,
        standardDeviation,
        math.max(0d, center - halfWidth),
        math.min(1d, center + halfWidth),
        "Wilson score"
      )
    } else {
      val critical =
        if (values.length == 1) 0d else studentTCritical95(values.length - 1)
      val halfWidth =
        if (values.length == 1) 0d
        else critical * standardDeviation / math.sqrt(values.length.toDouble)
      NumericSummary(
        values.length,
        mean,
        standardDeviation,
        mean - halfWidth,
        mean + halfWidth,
        "Student t"
      )
    }
  }

  def pairedTTest(left: Vector[Double], right: Vector[Double]): HypothesisTestResult = {
    require(left.length == right.length && left.nonEmpty, "Paired samples must be non-empty")
    val differences = left.zip(right).map { case (a, b) => a - b }
    val summary = summarize(differences)
    val standardError = summary.standardDeviation / math.sqrt(differences.length.toDouble)
    val statistic =
      if (standardError == 0d) {
        if (summary.mean == 0d) 0d
        else java.lang.Math.copySign(Double.PositiveInfinity, summary.mean)
      } else {
        summary.mean / standardError
      }
    val pValue =
      if (statistic.isInfinite) 0d
      else studentTTwoSidedP(statistic, differences.length - 1)

    HypothesisTestResult(statistic, pValue, summary.mean, differences.length)
  }

  def wilcoxonSignedRank(
      left: Vector[Double],
      right: Vector[Double]
  ): HypothesisTestResult = {
    require(left.length == right.length && left.nonEmpty, "Paired samples must be non-empty")
    val differences = left.zip(right).map { case (a, b) => a - b }.filter(_ != 0d)
    if (differences.isEmpty) {
      return HypothesisTestResult(0d, 1d, 0d, left.length)
    }

    val sorted = differences.zipWithIndex.sortBy { case (difference, _) => math.abs(difference) }
    val ranks = Array.fill(differences.length)(0d)
    val tieSizes = mutable.ArrayBuffer.empty[Int]
    var start = 0
    while (start < sorted.length) {
      var end = start + 1
      while (
        end < sorted.length &&
        math.abs(sorted(end)._1) == math.abs(sorted(start)._1)
      ) {
        end += 1
      }
      val averageRank = ((start + 1) + end).toDouble / 2d
      (start until end).foreach { index =>
        ranks(sorted(index)._2) = averageRank
      }
      tieSizes += (end - start)
      start = end
    }

    val positiveRank = differences.indices.collect {
      case index if differences(index) > 0d => ranks(index)
    }.sum
    val negativeRank = differences.indices.collect {
      case index if differences(index) < 0d => ranks(index)
    }.sum
    val n = differences.length.toDouble
    val mean = n * (n + 1d) / 4d
    val tieCorrection = tieSizes.map(size => size.toDouble * (size * size - 1d)).sum / 48d
    val variance = n * (n + 1d) * (2d * n + 1d) / 24d - tieCorrection
    val centered = positiveRank - mean
    val continuityCorrected =
      if (centered > 0d) centered - 0.5d
      else if (centered < 0d) centered + 0.5d
      else 0d
    val z = if (variance == 0d) 0d else continuityCorrected / math.sqrt(variance)
    val pValue = math.min(1d, 2d * (1d - normalCdf(math.abs(z))))
    val medianDifference = median(differences)

    HypothesisTestResult(
      statistic = math.min(positiveRank, negativeRank),
      pValue = pValue,
      effectEstimate = medianDifference,
      sampleCount = differences.length
    )
  }

  /** Exact two-sided McNemar test for paired binary outcomes. */
  def mcnemarExact(
      left: Vector[Boolean],
      right: Vector[Boolean]
  ): HypothesisTestResult = {
    require(left.length == right.length && left.nonEmpty, "Paired samples must be non-empty")
    val leftOnly = left.zip(right).count { case (a, b) => a && !b }
    val rightOnly = left.zip(right).count { case (a, b) => !a && b }
    val discordant = leftOnly + rightOnly
    val pValue =
      if (discordant == 0) 1d
      else {
        val tail = binomialHalfCdf(discordant, math.min(leftOnly, rightOnly))
        math.min(1d, 2d * tail)
      }
    val statistic =
      if (discordant == 0) 0d
      else math.pow(math.abs(leftOnly - rightOnly) - 1d, 2d) / discordant.toDouble
    val effect =
      left.count(identity).toDouble / left.length.toDouble -
        right.count(identity).toDouble / right.length.toDouble

    HypothesisTestResult(statistic, pValue, effect, left.length)
  }

  private def median(values: Vector[Double]): Double = {
    val sorted = values.sorted
    val middle = sorted.length / 2
    if (sorted.length % 2 == 1) sorted(middle)
    else (sorted(middle - 1) + sorted(middle)) / 2d
  }

  private def binomialHalfCdf(trials: Int, successes: Int): Double = {
    var probability = math.pow(0.5d, trials.toDouble)
    var cumulative = probability
    var k = 0
    while (k < successes) {
      probability *= (trials - k).toDouble / (k + 1).toDouble
      cumulative += probability
      k += 1
    }
    cumulative
  }

  private def studentTCritical95(degreesOfFreedom: Int): Double = {
    var low = 0d
    var high = 20d
    (0 until 80).foreach { _ =>
      val middle = (low + high) / 2d
      if (studentTTwoSidedP(middle, degreesOfFreedom) > 0.05d) low = middle
      else high = middle
    }
    (low + high) / 2d
  }

  private def studentTTwoSidedP(statistic: Double, degreesOfFreedom: Int): Double = {
    require(degreesOfFreedom > 0, "Student t requires positive degrees of freedom")
    val t = math.abs(statistic)
    val x = degreesOfFreedom.toDouble / (degreesOfFreedom.toDouble + t * t)
    regularizedBeta(x, degreesOfFreedom.toDouble / 2d, 0.5d)
  }

  private def regularizedBeta(x: Double, a: Double, b: Double): Double = {
    if (x <= 0d) 0d
    else if (x >= 1d) 1d
    else {
      val logarithm =
        logGamma(a + b) - logGamma(a) - logGamma(b) +
          a * math.log(x) + b * math.log1p(-x)
      val front = math.exp(logarithm)
      if (x < (a + 1d) / (a + b + 2d)) {
        front * betaContinuedFraction(x, a, b) / a
      } else {
        1d - front * betaContinuedFraction(1d - x, b, a) / b
      }
    }
  }

  private def betaContinuedFraction(x: Double, a: Double, b: Double): Double = {
    val maximumIterations = 200
    val epsilon = 3e-14
    val minimum = 1e-300
    val qab = a + b
    val qap = a + 1d
    val qam = a - 1d
    var c = 1d
    var d = 1d - qab * x / qap
    if (math.abs(d) < minimum) d = minimum
    d = 1d / d
    var result = d
    var iteration = 1
    var converged = false

    while (iteration <= maximumIterations && !converged) {
      val even = 2d * iteration
      var coefficient =
        iteration.toDouble * (b - iteration.toDouble) * x /
          ((qam + even) * (a + even))
      d = 1d + coefficient * d
      if (math.abs(d) < minimum) d = minimum
      c = 1d + coefficient / c
      if (math.abs(c) < minimum) c = minimum
      d = 1d / d
      result *= d * c

      coefficient =
        -(a + iteration.toDouble) * (qab + iteration.toDouble) * x /
          ((a + even) * (qap + even))
      d = 1d + coefficient * d
      if (math.abs(d) < minimum) d = minimum
      c = 1d + coefficient / c
      if (math.abs(c) < minimum) c = minimum
      d = 1d / d
      val delta = d * c
      result *= delta
      converged = math.abs(delta - 1d) < epsilon
      iteration += 1
    }
    result
  }

  private def logGamma(value: Double): Double = {
    val coefficients = Vector(
      676.5203681218851,
      -1259.1392167224028,
      771.32342877765313,
      -176.61502916214059,
      12.507343278686905,
      -0.13857109526572012,
      9.9843695780195716e-6,
      1.5056327351493116e-7
    )
    if (value < 0.5d) {
      math.log(math.Pi) - math.log(math.sin(math.Pi * value)) - logGamma(1d - value)
    } else {
      val shifted = value - 1d
      val series = coefficients.zipWithIndex.foldLeft(0.99999999999980993) {
        case (sum, (coefficient, index)) =>
          sum + coefficient / (shifted + index.toDouble + 1d)
      }
      val t = shifted + coefficients.length.toDouble - 0.5d
      0.5d * math.log(2d * math.Pi) +
        (shifted + 0.5d) * math.log(t) - t + math.log(series)
    }
  }

  private def normalCdf(value: Double): Double = {
    val absolute = math.abs(value)
    val t = 1d / (1d + 0.2316419d * absolute)
    val polynomial =
      t * (0.319381530d +
        t * (-0.356563782d +
          t * (1.781477937d +
            t * (-1.821255978d + t * 1.330274429d))))
    val density = math.exp(-0.5d * absolute * absolute) / math.sqrt(2d * math.Pi)
    val upper = 1d - density * polynomial
    if (value >= 0d) upper else 1d - upper
  }
}
