package com.sinanspd

/**
  * Dependency-free verification executable:
  *
  *   sbt "Test / runMain com.sinanspd.ShorPostProcessingVerification"
  */
object ShorPostProcessingVerification extends App {
  private def bits(value: String): Vector[Boolean] =
    value.map {
      case '0' => false
      case '1' => true
      case other => throw new IllegalArgumentException(s"Not a bit: $other")
    }.toVector

  private def assertFactors(
      postProcessing: ShorPostProcessing,
      sampledBits: String,
      expectedFactors: (Int, Int),
      expectedOrder: Int
  ): Unit = {
    val result = postProcessing.process(bits(sampledBits))
    assert(result.success, s"$sampledBits did not produce factors: ${result.message}")
    assert(
      result.factors.contains(expectedFactors),
      s"$sampledBits produced ${result.factors}, expected $expectedFactors"
    )
    assert(
      result.recoveredOrder.contains(expectedOrder),
      s"$sampledBits recovered ${result.recoveredOrder}, expected order $expectedOrder"
    )
    assert(
      result.factors.exists { case (left, right) =>
        left > 1 && right > 1 && left * right == postProcessing.modulus
      },
      s"$sampledBits did not produce non-trivial factors of ${postProcessing.modulus}"
    )
  }

  private def postProcessableBins(postProcessing: ShorPostProcessing): Set[Int] =
    (0 until (1 << postProcessing.countingQubits)).filter { measuredValue =>
      val sampledBits = Vector.tabulate(postProcessing.countingQubits) { index =>
        ((measuredValue >> (postProcessing.countingQubits - index - 1)) & 1) == 1
      }
      postProcessing.process(sampledBits).success
    }.toSet

  val shor15 = ShorPostProcessing(modulus = 15, base = 2, countingQubits = 4)
  assertFactors(shor15, sampledBits = "0100", expectedFactors = (3, 5), expectedOrder = 4)
  assertFactors(shor15, sampledBits = "1000", expectedFactors = (3, 5), expectedOrder = 4)
  assertFactors(shor15, sampledBits = "1100", expectedFactors = (3, 5), expectedOrder = 4)
  assert(!shor15.process(bits("0000")).success, "zero phase unexpectedly factored 15")
  val referenceBins15 = Set(4, 8, 12)
  val postProcessable15 = postProcessableBins(shor15)
  assert(
    referenceBins15.subsetOf(postProcessable15),
    s"post-processing lost reference Shor-15 bins: $postProcessable15"
  )

  val shor21 = ShorPostProcessing(modulus = 21, base = 2, countingQubits = 4)
  Vector("0011", "0101", "1000", "1011", "1101").foreach { sampledBits =>
    assertFactors(shor21, sampledBits, expectedFactors = (3, 7), expectedOrder = 6)
  }
  assert(!shor21.process(bits("0000")).success, "zero phase unexpectedly factored 21")
  assert(!shor21.process(bits("0001")).success, "an unsupported nonzero phase factored 21")

  val referenceBins21 = Set(3, 5, 8, 11, 13)
  val postProcessable21 = postProcessableBins(shor21)

  assert(
    referenceBins21.subsetOf(postProcessable21),
    s"post-processing lost reference Shor-21 bins: $postProcessable21"
  )
  assert(
    postProcessable21.size > referenceBins21.size,
    s"enhanced post-processing did not recover additional Shor-21 outcomes: $postProcessable21"
  )

  println(
    s"Shor post-processing verification passed; recoverable bins: " +
      s"N=15 [${postProcessable15.toVector.sorted.mkString(",")}], " +
      s"N=21 [${postProcessable21.toVector.sorted.mkString(",")}]"
  )
}
