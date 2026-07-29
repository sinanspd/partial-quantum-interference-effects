package com.sinanspd

object ReferenceMetricsVerification extends App {
  private def approximately(left: Double, right: Double): Boolean =
    math.abs(left - right) < 1e-10

  val grover = CircuitReferenceMetrics.calculate(
    ExperimentCatalog.grover4Tagged,
    selectedSimonOutput = None,
    instanceCount = 1
  )
  assert(grover.fullInterferenceContributions == 4096)
  assert(grover.fullInterferenceEndpointStates == 16)
  assert(grover.fullInterferenceReactions == 4080)
  assert(approximately(grover.idealOutputProbabilities.values.sum, 1d))
  assert(
    approximately(
      grover.idealProbability(Vector(false, false, false, false)),
      0.47265625d
    )
  )

  val simonOutput = Vector(false, false, false)
  val simon = CircuitReferenceMetrics.calculate(
    ExperimentCatalog.simonN3,
    selectedSimonOutput = Some(simonOutput),
    instanceCount = 3
  )
  assert(simon.fullInterferenceContributions == 48)
  assert(simon.fullInterferenceEndpointStates == 8)
  assert(simon.fullInterferenceReactions == 40)
  assert(approximately(simon.idealOutputProbabilities.values.sum, 1d))
  ExperimentCatalog.simonN3.correctOutcomes.states.foreach { state =>
    assert(approximately(simon.idealProbability(state), 0.25d))
  }
  val invalidSimonStates =
    simon.idealOutputProbabilities.keySet -- ExperimentCatalog.simonN3.correctOutcomes.states
  invalidSimonStates.foreach { state =>
    assert(approximately(simon.idealProbability(state), 0d))
  }

  ExperimentCatalog.all.foreach { experiment =>
    val selectedOutput =
      experiment.simonPostSelection.map(_.possibleOracleOutputs.head)
    val metrics =
      CircuitReferenceMetrics.calculate(experiment, selectedOutput, instanceCount = 1)
    assert(approximately(metrics.idealOutputProbabilities.values.sum, 1d))
    assert(metrics.fullInterferenceReactions >= 0)
    if (experiment.simonPostSelection.isEmpty) {
      assert(metrics.fullInterferenceContributions == experiment.leafMetrics.total)
    }
  }

  val rendered = grover.renderedIdealDistribution
  val parsed = OutputDistributionMetrics.parse(rendered)
  assert(
    parsed.keySet == grover.idealOutputProbabilities.keys
      .map(state => state.map(if (_) '1' else '0').mkString)
      .toSet
  )

  println("Circuit reference metrics verification passed.")
}
