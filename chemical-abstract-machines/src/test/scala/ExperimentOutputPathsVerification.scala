package com.sinanspd

import java.nio.file.Paths
import java.time.Instant

object ExperimentOutputPathsVerification extends App {
  val root = Paths.get("target", "experiments", "batches")
  val timestamp = Instant.parse("2026-07-28T22:30:45.123Z")
  val born = ExperimentOutputPaths.directSamplePath(
    root,
    "simon-n3",
    BornRuleSampling,
    0.5d,
    timestamp,
    "born-id"
  )
  val crossing = ExperimentOutputPaths.directSamplePath(
    root,
    "simon-n3",
    FirstThresholdCrossing,
    0.5d,
    timestamp,
    "crossing-id"
  )

  assert(born != crossing)
  assert(born.startsWith(root))
  assert(crossing.startsWith(root))
  assert(born.getFileName.toString == "simon-n3-sampled-state.txt")
  assert(born.getParent.toString.contains("normalized-born-rule-at-threshold"))
  assert(crossing.getParent.toString.contains("first-threshold-crossing"))

  val explicit = Paths.get("target", "custom", "trial-0001.txt")
  assert(
    ExperimentOutputPaths.samplePath(
      Some(explicit),
      root,
      "ignored",
      BornRuleSampling,
      0.9d
    ) == explicit
  )

  println("Experiment output-path verification passed.")
}
