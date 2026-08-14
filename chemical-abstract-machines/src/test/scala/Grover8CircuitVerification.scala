package com.sinanspd

/** Verification of the five-search-plus-three-ancilla Grover circuits. */
object Grover8CircuitVerification extends App {
  private def approximately(left: Double, right: Double): Boolean =
    math.abs(left - right) <= 1e-10

  private val variants = Vector(
    (ExperimentCatalog.grover3Sat, 1, BigInt(32768), BigInt(1024), 0.25830078125d),
    (ExperimentCatalog.grover5Iteration1, 1, BigInt(32768), BigInt(1024), 0.25830078125d),
    (
      ExperimentCatalog.grover5Iteration2,
      2,
      BigInt(33554432),
      BigInt(1048576),
      0.6024246215820311d
    ),
    (
      ExperimentCatalog.grover5Iteration3,
      3,
      BigInt("34359738368"),
      BigInt(1073741824),
      0.896936535835266d
    )
  )
  private val marked = Vector(false, true, true, true, true)

  assert(ExperimentCatalog("grover3Sat") eq ExperimentCatalog.grover3Sat)
  assert(ExperimentCatalog("GROVER_3SAT") eq ExperimentCatalog.grover3Sat)

  variants.foreach {
    case (experiment, iterations, expectedTotal, expectedCorrect, expectedProbability) =>
      assert(experiment.qubitCount == 8)
      assert(experiment.resultQubits.contains((0 until 5).toVector))
      assert(experiment.hadamardCount == 5 + 10 * iterations)
      assert(
        experiment.leafMetrics ==
          CircuitLeafMetrics(expectedTotal, expectedCorrect)
      )

      val reference = CircuitReferenceMetrics.calculate(
        experiment,
        selectedPostSelectionOutcome = None,
        instanceCount = 1
      )
      assert(reference.fullInterferenceContributions == expectedTotal)
      assert(reference.fullInterferenceEndpointStates == 32)
      assert(
        approximately(reference.idealProbability(marked), expectedProbability),
        s"${experiment.alias} marked probability was " +
          s"${reference.idealProbability(marked)}, expected $expectedProbability"
      )
      assert(approximately(reference.idealOutputProbabilities.values.sum, 1d))
  }

  println("Eight-qubit Grover circuit verification passed.")
}
