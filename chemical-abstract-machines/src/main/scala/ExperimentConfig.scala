package com.sinanspd

import java.nio.file.{Path, Paths}

/**
  * Source-level experiment selection.
  *
  * This is deliberately not a command-line interface: edit `circuitAlias` and
  * run `sbt run`. The remaining values are kept here so a run can be
  * reproduced from a single source revision.
  */
object ExperimentConfig {
  val circuitAlias: String = "grover-4-tagged"
  val threshold: Double = 0.5

  /** None lets tagged Grover experiments choose the minimum copy count whose
    * final marked amplitude can reach `threshold`; other experiments use their
    * catalog default. Some(n) always runs exactly n independent copies.
    */
  val instanceCountOverride: Option[Int] = None

  val workerThreads: Int = 32
  val completionJitterMillis: Int = 40
  val shutdownDrainMillis: Int = 25
  val randomSeed: Long = 260601922L
  val preflightOnly: Boolean = false
  val terminateAfterSample: Boolean = true

  /** Bounded classical search used only by Shor outcome post-processing. */
  val shorMaxIntermediateConvergents: Int = 4
  val shorMaxDenominatorMultiple: Int = 8

  val outputDirectory: Path = Paths.get("target", "experiments")

  /** Source-level configuration for RepeatedExperimentRunner. */
  val repeatedTrialCount: Int = 50
  val repeatedTrialThresholds: Vector[Double] = Vector(threshold)
  val repeatedTrialJvmMaxHeap: String = "32G"
  val repeatedTrialBatchLabel: Option[String] = None
  val repeatedTrialOutputDirectory: Path = outputDirectory.resolve("batches")
}
