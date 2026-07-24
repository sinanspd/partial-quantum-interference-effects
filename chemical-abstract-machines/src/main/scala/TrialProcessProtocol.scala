package com.sinanspd

/** Internal JVM properties used by RepeatedExperimentRunner child processes. */
private[sinanspd] object TrialProcessProtocol {
  val thresholdProperty = "cham.internal.trial.threshold"
  val randomSeedProperty = "cham.internal.trial.randomSeed"
  val trialIdProperty = "cham.internal.trial.id"
  val sampleFileProperty = "cham.internal.trial.sampleFile"
}
