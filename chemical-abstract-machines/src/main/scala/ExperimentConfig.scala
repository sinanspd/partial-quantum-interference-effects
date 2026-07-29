package com.sinanspd

import java.nio.file.{Path, Paths}

sealed trait TrialExecutionBackend {
  def label: String
  private[sinanspd] def mainClass: String
}

case object StreamingPathBackend extends TrialExecutionBackend {
  override val label: String = "bounded FS2 path generation + CHAM endpoint interference"
  override private[sinanspd] val mainClass: String = "com.sinanspd.Main"
}

case object FullyChamBackend extends TrialExecutionBackend {
  override val label: String = "CHAM path generation + CHAM endpoint interference"
  override private[sinanspd] val mainClass: String = "com.sinanspd.Cham2"
}

sealed trait OutcomeSelectionMode {
  def label: String
}

/** Preserve the prototype's original first-threshold-crossing behavior. */
case object FirstThresholdCrossing extends OutcomeSelectionMode {
  override val label: String = "first-threshold-crossing"
}

/**
  * Stop at the first threshold crossing, then sample from the normalized
  * squared magnitudes of the truncated state aggregates.
  */
case object BornRuleSampling extends OutcomeSelectionMode {
  override val label: String = "normalized-born-rule-at-threshold"
}

/**
  * Source-level experiment selection.
  *
  * This is deliberately not a command-line interface: edit `circuitAlias` and
  * run `sbt run`. The remaining values are kept here so a run can be
  * reproduced from a single source revision.
  */
object ExperimentConfig {
  val circuitAlias: String = "simon-n5"
  val threshold: Double = 0.1

  /** None lets tagged Grover experiments choose the minimum copy count whose
    * final marked amplitude can reach `threshold`; other experiments use their
    * catalog default. Some(n) always runs exactly n independent copies.
    */
  val instanceCountOverride: Option[Int] = None

  val workerThreads: Int = 32
  /** Random startup variance applied once to every bounded FS2 frontier branch.
    * None chooses and persists a fresh seed for every experiment JVM.
    */
  val fs2BranchJitterMillis: Int = 5
  val fs2BranchJitterSeedOverride: Option[Long] = None
  val completionJitterMillis: Int = 40
  val shutdownDrainMillis: Int = 25
  val randomSeed: Long = 260601922L
  val outcomeSelectionMode: OutcomeSelectionMode = FirstThresholdCrossing
  val preflightOnly: Boolean = false
  val terminateAfterSample: Boolean = true

  /** Bounded classical search used only by Shor outcome post-processing. */
  val shorMaxIntermediateConvergents: Int = 4
  val shorMaxDenominatorMultiple: Int = 8

  val outputDirectory: Path = Paths.get("target", "experiments")

  /** Source-level configuration for RepeatedExperimentRunner. */
  val repeatedTrialCount: Int = 50
  val repeatedTrialThresholds: Vector[Double] = Vector(threshold)
  val repeatedTrialBackend: TrialExecutionBackend = StreamingPathBackend
  val repeatedTrialJvmMaxHeap: String = "32G"
  val repeatedTrialTimeoutMinutes: Long = 24L * 60L
  val distributionBootstrapReplicates: Int = 5000
  val repeatedTrialBatchLabel: Option[String] = None
  val repeatedTrialOutputDirectory: Path = outputDirectory.resolve("batches")
}
