package com.sinanspd

import java.nio.file.Path
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}
import java.util.UUID

object ExperimentOutputPaths {
  private val timestampFormatter = DateTimeFormatter
    .ofPattern("yyyyMMdd-HHmmss-SSS")
    .withZone(ZoneOffset.UTC)

  /**
    * RepeatedExperimentRunner supplies an explicit unique per-trial path.
    * Interactive/direct runs receive a new directory under the batch root.
    */
  def samplePath(
      explicitPath: Option[Path],
      batchRoot: Path,
      experimentAlias: String,
      selectionMode: OutcomeSelectionMode,
      threshold: Double
  ): Path =
    explicitPath.getOrElse(
      directSamplePath(
        batchRoot,
        experimentAlias,
        selectionMode,
        threshold,
        Instant.now(),
        UUID.randomUUID().toString
      )
    )

  private[sinanspd] def directSamplePath(
      batchRoot: Path,
      experimentAlias: String,
      selectionMode: OutcomeSelectionMode,
      threshold: Double,
      timestamp: Instant,
      uniqueness: String
  ): Path = {
    val safeAlias = fileSafe(experimentAlias)
    val safeMode = fileSafe(selectionMode.label)
    val safeThreshold = fileSafe(java.lang.Double.toString(threshold))
    val safeUniqueness = fileSafe(uniqueness)
    val runDirectory =
      s"direct-$safeAlias-$safeMode-threshold-$safeThreshold-" +
        s"${timestampFormatter.format(timestamp)}-$safeUniqueness"
    batchRoot.resolve(runDirectory).resolve(s"$safeAlias-sampled-state.txt")
  }

  private def fileSafe(value: String): String = {
    val rendered = value.replaceAll("[^A-Za-z0-9._-]", "_")
    if (rendered.nonEmpty) rendered else "unnamed"
  }
}
