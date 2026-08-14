package com.sinanspd

/** Dependency-free verification of exact pre-interference leaf counts. */
object LeafMetricsVerification extends App {
  private val expected = Map(
    "deutsch-jozsa" -> CircuitLeafMetrics(total = 8, correct = 4),
    "grover-4-tagged" -> CircuitLeafMetrics(total = 4096, correct = 256),
    "grover-3sat" -> CircuitLeafMetrics(total = 32768, correct = 1024),
    "grover-3sat-3q-approx" -> CircuitLeafMetrics(total = 32768, correct = 4096),
    "grover-3sat-30" -> CircuitLeafMetrics(total = 16777216, correct = 65536),
    "grover-5-r1" -> CircuitLeafMetrics(total = 32768, correct = 1024),
    "grover-5-r2" -> CircuitLeafMetrics(total = 33554432, correct = 1048576),
    "grover-5-r3" -> CircuitLeafMetrics(total = BigInt("34359738368"), correct = 1073741824),
    "simon-n3" -> CircuitLeafMetrics(total = 64, correct = 8),
    "simon-n5" -> CircuitLeafMetrics(total = 1024, correct = 32),
    "simon-n15" -> CircuitLeafMetrics(total = 1073741824, correct = 32768),
    "shor-n15" -> CircuitLeafMetrics(total = 256, correct = 112),
    "shor-n21" ->
      CircuitLeafMetrics(total = 4294967296L, correct = 1114112)
  )

  ExperimentCatalog.all.foreach { experiment =>
    val metrics = experiment.leafMetrics
    println(s"${experiment.alias}: B_total=${metrics.total}, B_correct=${metrics.correct}")
    expected.get(experiment.alias).foreach { expectedMetrics =>
      assert(
        metrics == expectedMetrics,
        s"${experiment.alias} produced $metrics, expected $expectedMetrics"
      )
    }
  }

  assert(
    expected.keySet.subsetOf(ExperimentCatalog.aliases.toSet),
    "The expected leaf-metric table contains an unknown experiment"
  )
  Vector(
    ExperimentCatalog.simonN3,
    ExperimentCatalog.simonN5,
    ExperimentCatalog.simonN15
  ).foreach {
    experiment =>
      val retainedWidth = experiment.postSelection.get.retainedQubits.length
      assert(
        experiment.leafMetrics ==
          CircuitLeafMetrics(
            total = BigInt(1) << (retainedWidth * 2),
            correct = BigInt(1) << retainedWidth
          )
      )
  }
  assert(ExperimentCatalog.simonN3.leafMetrics.correct * 3 == 24)
}
