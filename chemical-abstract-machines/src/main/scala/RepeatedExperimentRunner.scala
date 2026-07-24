package com.sinanspd

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.Try

/**
  * Runs the source-selected experiment in a fresh JVM for every trial.
  *
  * Configure ExperimentConfig.repeatedTrial* and run:
  *
  *   sbt "runMain com.sinanspd.RepeatedExperimentRunner"
  */
object RepeatedExperimentRunner extends App {
  private final case class TrialRecord(
      trialIndex: Int,
      threshold: Double,
      exitCode: Int,
      values: Map[String, String]
  ) {
    def number(key: String): Option[Double] = values.get(key).flatMap(value => Try(value.toDouble).toOption)
    def boolean(key: String): Option[Boolean] = values.get(key).flatMap {
      case "true"  => Some(true)
      case "false" => Some(false)
      case _       => None
    }
  }

  private val trialCount = ExperimentConfig.repeatedTrialCount
  private val thresholds = ExperimentConfig.repeatedTrialThresholds.distinct
  require(trialCount > 0, "repeatedTrialCount must be positive")
  require(thresholds.nonEmpty && thresholds.forall(_ > 0d), "Trial thresholds must be positive")
  require(
    ExperimentConfig.repeatedTrialJvmMaxHeap.matches("[1-9][0-9]*[kKmMgG]?"),
    "repeatedTrialJvmMaxHeap must look like 32G or 4096M"
  )

  if (trialCount < 50) {
    Console.err.println(
      s"Warning: repeatedTrialCount=$trialCount is below the reviewer's requested minimum of 50."
    )
  }

  private val timestamp = DateTimeFormatter
    .ofPattern("yyyyMMdd-HHmmss-SSS")
    .withZone(ZoneOffset.UTC)
    .format(Instant.now())
  private val label = ExperimentConfig.repeatedTrialBatchLabel
    .map(_.replaceAll("[^A-Za-z0-9._-]", "_"))
    .filter(_.nonEmpty)
    .getOrElse(ExperimentConfig.circuitAlias)
  private val batchId = s"$label-$timestamp"
  private val batchDirectory =
    ExperimentConfig.repeatedTrialOutputDirectory.resolve(batchId).toAbsolutePath.normalize
  private val trialDirectory = batchDirectory.resolve("trials")
  private val logDirectory = batchDirectory.resolve("logs")
  Files.createDirectories(trialDirectory)
  Files.createDirectories(logDirectory)

  private val javaBinary =
    Paths.get(System.getProperty("java.home"), "bin", "java").toAbsolutePath.normalize.toString
  private val classpath = System.getProperty("java.class.path")
  private val records = mutable.ArrayBuffer.empty[TrialRecord]

  println(
    s"Starting $trialCount fresh-JVM trial(s) for ${ExperimentConfig.circuitAlias} " +
      s"at threshold(s) ${thresholds.mkString(", ")}"
  )
  println(s"Batch directory: $batchDirectory")

  thresholds.foreach { threshold =>
    val thresholdLabel = fileSafeThreshold(threshold)
    val thresholdTrialDirectory = trialDirectory.resolve(s"threshold-$thresholdLabel")
    val thresholdLogDirectory = logDirectory.resolve(s"threshold-$thresholdLabel")
    Files.createDirectories(thresholdTrialDirectory)
    Files.createDirectories(thresholdLogDirectory)

    (1 to trialCount).foreach { trialIndex =>
      val trialId = f"$trialIndex%04d"
      val trialSeed = ExperimentConfig.randomSeed + trialIndex.toLong - 1L
      val sampleFile = thresholdTrialDirectory.resolve(s"trial-$trialId.txt")
      val logFile = thresholdLogDirectory.resolve(s"trial-$trialId.log")
      val command = Vector(
        javaBinary,
        s"-Xmx${ExperimentConfig.repeatedTrialJvmMaxHeap}",
        s"-D${TrialProcessProtocol.thresholdProperty}=$threshold",
        s"-D${TrialProcessProtocol.randomSeedProperty}=$trialSeed",
        s"-D${TrialProcessProtocol.trialIdProperty}=$trialId",
        s"-D${TrialProcessProtocol.sampleFileProperty}=${sampleFile.toAbsolutePath.normalize}",
        "-cp",
        classpath,
        "com.sinanspd.Cham2"
      )

      val processBuilder = new ProcessBuilder(command: _*)
      processBuilder.redirectErrorStream(true)
      processBuilder.redirectOutput(logFile.toFile)
      val exitCode = processBuilder.start().waitFor()
      val resultValues =
        if (Files.exists(sampleFile)) parseKeyValueFile(sampleFile) else Map.empty[String, String]
      val enriched = enrichResult(
        resultValues,
        threshold = threshold,
        trialIndex = trialIndex,
        trialSeed = trialSeed,
        exitCode = exitCode,
        sampleFile = sampleFile,
        logFile = logFile
      )
      records += TrialRecord(trialIndex, threshold, exitCode, enriched)

      val outcome = enriched.getOrElse("observedOutcome", "missing")
      val correctness = enriched.getOrElse("isCorrect", "missing")
      println(
        f"threshold=$threshold%.6g trial=$trialIndex%4d/$trialCount " +
          s"exit=$exitCode outcome=$outcome correct=$correctness"
      )
    }
  }

  private val trialColumns = Vector(
    "batchId",
    "trialIndex",
    "trialSeed",
    "processExitCode",
    "failureReason",
    "experiment",
    "threshold",
    "instances",
    "qubits",
    "backend",
    "randomSeed",
    "bits",
    "observedOutcome",
    "amplitude.real",
    "amplitude.imag",
    "isCorrect",
    "correctnessMethod",
    "B_total",
    "B_correct",
    "B_active",
    "B_activecorrect",
    "B_totalPerCopy",
    "B_correctPerCopy",
    "B_activeFraction",
    "B_activeCorrectFraction",
    "B_correctAmongActiveFraction",
    "B_reductionFraction",
    "selectedAtTerminalContributions",
    "selectedAtInterferenceReactions",
    "elapsedMillis",
    "shorPhaseEstimate",
    "shorRationalCandidates",
    "shorTestedExponents",
    "shorSuccessfulExponent",
    "shorRecoveredOrder",
    "shorFactors",
    "shorPostProcessingSuccess",
    "sampleFile",
    "logFile"
  )
  writeCsv(
    batchDirectory.resolve("trials.csv"),
    trialColumns,
    records.toVector.map(_.values)
  )

  private val validRecords =
    records.toVector.filter(record => record.exitCode == 0 && record.boolean("isCorrect").isDefined)
  private val numericMetrics = Vector(
    "isCorrect",
    "B_total",
    "B_correct",
    "B_active",
    "B_activecorrect",
    "B_activeFraction",
    "B_activeCorrectFraction",
    "B_correctAmongActiveFraction",
    "B_reductionFraction",
    "selectedAtTerminalContributions",
    "selectedAtInterferenceReactions",
    "elapsedMillis"
  )
  private val summaryRows = thresholds.flatMap { threshold =>
    val group = validRecords.filter(_.threshold == threshold)
    numericMetrics.flatMap { metric =>
      val values =
        if (metric == "isCorrect") {
          group.flatMap(_.boolean(metric).map(if (_) 1d else 0d))
        } else {
          group.flatMap(_.number(metric))
        }
      if (values.isEmpty) {
        None
      } else {
        val summary = TrialStatistics.summarize(values, proportion = metric == "isCorrect")
        Some(
          Map(
            "experiment" -> ExperimentConfig.circuitAlias,
            "threshold" -> threshold.toString,
            "metric" -> metric,
            "n" -> summary.count.toString,
            "mean" -> summary.mean.toString,
            "standardDeviation" -> summary.standardDeviation.toString,
            "confidenceLow95" -> summary.confidenceLow95.toString,
            "confidenceHigh95" -> summary.confidenceHigh95.toString,
            "confidenceMethod" -> summary.confidenceMethod,
            "meanPlusMinusStandardDeviation" ->
              s"${summary.mean} +/- ${summary.standardDeviation}"
          )
        )
      }
    }
  }
  private val summaryColumns = Vector(
    "experiment",
    "threshold",
    "metric",
    "n",
    "mean",
    "standardDeviation",
    "confidenceLow95",
    "confidenceHigh95",
    "confidenceMethod",
    "meanPlusMinusStandardDeviation"
  )
  writeCsv(batchDirectory.resolve("summary.csv"), summaryColumns, summaryRows)

  private val comparisonRows = pairwiseComparisons(validRecords, thresholds)
  private val comparisonColumns = Vector(
    "experiment",
    "thresholdA",
    "thresholdB",
    "metric",
    "test",
    "n",
    "statistic",
    "pValueTwoSided",
    "effectEstimateAminusB"
  )
  writeCsv(
    batchDirectory.resolve("pairwise-comparisons.csv"),
    comparisonColumns,
    comparisonRows
  )
  writeText(
    batchDirectory.resolve("README.txt"),
    batchReadme(records.toVector, summaryRows, comparisonRows)
  )

  val failed = records.count(_.exitCode != 0)
  println(
    s"Repeated experiment complete: ${records.length - failed} successful process(es), " +
      s"$failed failed. Results: ${batchDirectory.resolve("trials.csv")}"
  )

  private def enrichResult(
      values: Map[String, String],
      threshold: Double,
      trialIndex: Int,
      trialSeed: Long,
      exitCode: Int,
      sampleFile: Path,
      logFile: Path
  ): Map[String, String] = {
    def ratio(numerator: String, denominator: String): Option[Double] =
      for {
        top <- values.get(numerator).flatMap(value => Try(BigDecimal(value)).toOption)
        bottom <- values.get(denominator).flatMap(value => Try(BigDecimal(value)).toOption)
        if bottom != 0
      } yield (top / bottom).toDouble

    val activeFraction = ratio("B_active", "B_total")
    val activeCorrectFraction = ratio("B_activecorrect", "B_correct")
    val correctAmongActive = ratio("B_activecorrect", "B_active")
    values ++ Map(
      "batchId" -> batchId,
      "trialIndex" -> trialIndex.toString,
      "trialSeed" -> trialSeed.toString,
      "processExitCode" -> exitCode.toString,
      "failureReason" ->
        (if (exitCode == 0 && values.nonEmpty) "" else s"exit=$exitCode or missing result file"),
      "threshold" -> threshold.toString,
      "B_activeFraction" -> activeFraction.map(_.toString).getOrElse(""),
      "B_activeCorrectFraction" -> activeCorrectFraction.map(_.toString).getOrElse(""),
      "B_correctAmongActiveFraction" -> correctAmongActive.map(_.toString).getOrElse(""),
      "B_reductionFraction" -> activeFraction.map(value => (1d - value).toString).getOrElse(""),
      "sampleFile" -> sampleFile.toAbsolutePath.normalize.toString,
      "logFile" -> logFile.toAbsolutePath.normalize.toString
    )
  }

  private def pairwiseComparisons(
      valid: Vector[TrialRecord],
      configuredThresholds: Vector[Double]
  ): Vector[Map[String, String]] = {
    val continuousMetrics = Vector(
      "B_activeFraction",
      "B_activeCorrectFraction",
      "B_correctAmongActiveFraction",
      "B_reductionFraction",
      "elapsedMillis"
    )
    val pairs = for {
      leftIndex <- configuredThresholds.indices
      rightIndex <- (leftIndex + 1) until configuredThresholds.length
    } yield configuredThresholds(leftIndex) -> configuredThresholds(rightIndex)

    val fullSimulationComparisons = configuredThresholds.flatMap { threshold =>
      val group = valid.filter(_.threshold == threshold).sortBy(_.trialIndex)
      Vector(
        ("B_active", "B_total", "B_active versus B_total"),
        ("B_activecorrect", "B_correct", "B_activecorrect versus B_correct")
      ).flatMap {
        case (activeMetric, fullMetric, label) =>
          val pairedValues = group.flatMap { record =>
            for {
              active <- record.number(activeMetric)
              full <- record.number(fullMetric)
            } yield active -> full
          }
          if (pairedValues.isEmpty) {
            Vector.empty
          } else {
            val (activeValues, fullValues) = pairedValues.unzip
            Vector(
              baselineTestRow(
                threshold,
                label,
                "paired t-test",
                TrialStatistics.pairedTTest(activeValues, fullValues)
              ),
              baselineTestRow(
                threshold,
                label,
                "Wilcoxon signed-rank",
                TrialStatistics.wilcoxonSignedRank(activeValues, fullValues)
              )
            )
          }
      }
    }

    val thresholdComparisons = pairs.toVector.flatMap {
      case (thresholdA, thresholdB) =>
        val left = valid.filter(_.threshold == thresholdA).map(record => record.trialIndex -> record).toMap
        val right =
          valid.filter(_.threshold == thresholdB).map(record => record.trialIndex -> record).toMap
        val pairedIndices = left.keySet.intersect(right.keySet).toVector.sorted

        val continuousRows = continuousMetrics.flatMap { metric =>
          val pairedValues = pairedIndices.flatMap { index =>
            for {
              a <- left(index).number(metric)
              b <- right(index).number(metric)
            } yield a -> b
          }
          if (pairedValues.isEmpty) {
            Vector.empty
          } else {
            val (leftValues, rightValues) = pairedValues.unzip
            val pairedT = TrialStatistics.pairedTTest(leftValues, rightValues)
            val wilcoxon = TrialStatistics.wilcoxonSignedRank(leftValues, rightValues)
            Vector(
              testRow(thresholdA, thresholdB, metric, "paired t-test", pairedT),
              testRow(thresholdA, thresholdB, metric, "Wilcoxon signed-rank", wilcoxon)
            )
          }
        }

        val binaryPairs = pairedIndices.flatMap { index =>
          for {
            a <- left(index).boolean("isCorrect")
            b <- right(index).boolean("isCorrect")
          } yield a -> b
        }
        val binaryRows =
          if (binaryPairs.isEmpty) Vector.empty
          else {
            val (leftValues, rightValues) = binaryPairs.unzip
            Vector(
              testRow(
                thresholdA,
                thresholdB,
                "isCorrect",
                "exact McNemar",
                TrialStatistics.mcnemarExact(leftValues, rightValues)
              )
            )
          }

        continuousRows ++ binaryRows
    }

    fullSimulationComparisons ++ thresholdComparisons
  }

  private def testRow(
      thresholdA: Double,
      thresholdB: Double,
      metric: String,
      test: String,
      result: HypothesisTestResult
  ): Map[String, String] =
    Map(
      "experiment" -> ExperimentConfig.circuitAlias,
      "thresholdA" -> thresholdA.toString,
      "thresholdB" -> thresholdB.toString,
      "metric" -> metric,
      "test" -> test,
      "n" -> result.sampleCount.toString,
      "statistic" -> result.statistic.toString,
      "pValueTwoSided" -> result.pValue.toString,
      "effectEstimateAminusB" -> result.effectEstimate.toString
    )

  private def baselineTestRow(
      threshold: Double,
      metric: String,
      test: String,
      result: HypothesisTestResult
  ): Map[String, String] =
    Map(
      "experiment" -> ExperimentConfig.circuitAlias,
      "thresholdA" -> threshold.toString,
      "thresholdB" -> "full-simulation",
      "metric" -> metric,
      "test" -> test,
      "n" -> result.sampleCount.toString,
      "statistic" -> result.statistic.toString,
      "pValueTwoSided" -> result.pValue.toString,
      "effectEstimateAminusB" -> result.effectEstimate.toString
    )

  private def batchReadme(
      allRecords: Vector[TrialRecord],
      summaries: Vector[Map[String, String]],
      comparisons: Vector[Map[String, String]]
  ): String = {
    val successful = allRecords.count(_.exitCode == 0)
    val summaryLines = summaries
      .filter(row => row("metric") == "isCorrect" || row("metric") == "B_activeFraction")
      .map { row =>
        s"threshold=${row("threshold")} ${row("metric")}: " +
          s"${row("meanPlusMinusStandardDeviation")}, " +
          s"95% CI [${row("confidenceLow95")}, ${row("confidenceHigh95")}]"
      }
    s"""Repeated CHAM experiment batch
       |batchId=$batchId
       |experiment=${ExperimentConfig.circuitAlias}
       |requestedTrialsPerThreshold=$trialCount
       |thresholds=${thresholds.mkString(",")}
       |successfulProcesses=$successful
       |failedProcesses=${allRecords.length - successful}
       |
       |Key summaries
       |${summaryLines.mkString("\n")}
       |
       |Files
       |trials.csv: one row per fresh-JVM trial
       |summary.csv: mean, sample standard deviation, and 95% confidence interval
       |pairwise-comparisons.csv: paired t-tests, Wilcoxon signed-rank tests, and exact McNemar tests
       |trials/: complete per-trial result records
       |logs/: complete per-trial process logs
       |
       |Pairwise comparison rows=${comparisons.length}
       |""".stripMargin
  }

  private def parseKeyValueFile(path: Path): Map[String, String] =
    Files
      .readAllLines(path, StandardCharsets.UTF_8)
      .asScala
      .flatMap { line =>
        val separator = line.indexOf('=')
        if (separator <= 0) None
        else Some(line.substring(0, separator) -> line.substring(separator + 1))
      }
      .toMap

  private def writeCsv(
      path: Path,
      columns: Vector[String],
      rows: Seq[Map[String, String]]
  ): Unit = {
    val header = columns.map(csvEscape).mkString(",")
    val body = rows.map { row =>
      columns.map(column => csvEscape(row.getOrElse(column, ""))).mkString(",")
    }
    writeText(path, (header +: body).mkString("", "\n", "\n"))
  }

  private def csvEscape(value: String): String = {
    val escaped = value.replace("\"", "\"\"")
    if (escaped.exists(character => character == ',' || character == '"' || character == '\n'))
      "\"" + escaped + "\""
    else escaped
  }

  private def writeText(path: Path, contents: String): Unit =
    Files.write(
      path,
      contents.getBytes(StandardCharsets.UTF_8),
      StandardOpenOption.CREATE_NEW,
      StandardOpenOption.WRITE
    )

  private def fileSafeThreshold(threshold: Double): String =
    threshold.toString.replace('-', 'm').replace('.', 'p')
}
