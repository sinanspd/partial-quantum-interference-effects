package com.sinanspd

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference
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
    def succeeded: Boolean =
      exitCode == 0 && values.get("failureReason").forall(_.isEmpty)
    def number(key: String): Option[Double] = values.get(key).flatMap(value => Try(value.toDouble).toOption)
    def boolean(key: String): Option[Boolean] = values.get(key).flatMap {
      case "true"  => Some(true)
      case "false" => Some(false)
      case _       => None
    }
  }

  private final case class ChildProcessResult(exitCode: Int, failureReason: String)

  private val trialCount = ExperimentConfig.repeatedTrialCount
  private val thresholds = ExperimentConfig.repeatedTrialThresholds.distinct
  require(trialCount > 0, "repeatedTrialCount must be positive")
  require(thresholds.nonEmpty && thresholds.forall(_ > 0d), "Trial thresholds must be positive")
  require(
    ExperimentConfig.repeatedTrialJvmMaxHeap.matches("[1-9][0-9]*[kKmMgG]?"),
    "repeatedTrialJvmMaxHeap must look like 32G or 4096M"
  )
  require(
    ExperimentConfig.repeatedTrialTimeoutMinutes > 0L,
    "repeatedTrialTimeoutMinutes must be positive"
  )
  require(
    ExperimentConfig.distributionBootstrapReplicates > 0,
    "distributionBootstrapReplicates must be positive"
  )
  private val experiment =
    ExperimentCatalog(ExperimentConfig.circuitAlias)
  private val experimentAlias = experiment.alias

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
    .getOrElse(experimentAlias)
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
  private val activeChild = new AtomicReference[Process]()
  Runtime.getRuntime.addShutdownHook(
    new Thread(
      new Runnable {
        override def run(): Unit =
          Option(activeChild.getAndSet(null)).foreach(terminateChild)
      },
      "repeated-experiment-child-cleanup"
    )
  )

  println(
    s"Starting $trialCount fresh-JVM trial(s) for $experimentAlias " +
      s"at threshold(s) ${thresholds.mkString(", ")}"
  )
  println(s"Trial backend: ${ExperimentConfig.repeatedTrialBackend.label}")
  println(s"Batch directory: $batchDirectory")

  thresholds.foreach { threshold =>
    val thresholdLabel = fileSafeThreshold(threshold)
    val thresholdTrialDirectory = trialDirectory.resolve(s"threshold-$thresholdLabel")
    val thresholdLogDirectory = logDirectory.resolve(s"threshold-$thresholdLabel")
    Files.createDirectories(thresholdTrialDirectory)
    Files.createDirectories(thresholdLogDirectory)

    (1 to trialCount).foreach { trialIndex =>
      val trialId = f"$trialIndex%04d"
      val trialSeed =
        ExperimentRandom.trialSeed(ExperimentConfig.randomSeed, trialIndex)
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
        ExperimentConfig.repeatedTrialBackend.mainClass
      )

      val processBuilder = new ProcessBuilder(command: _*)
      processBuilder.redirectErrorStream(true)
      processBuilder.redirectOutput(logFile.toFile)
      println(
        f"starting threshold=$threshold%.6g trial=$trialIndex%4d/$trialCount " +
          s"log=${logFile.toAbsolutePath.normalize}"
      )
      Console.out.flush()
      val childResult = runChild(processBuilder, logFile)
      val exitCode = childResult.exitCode
      val resultValues =
        if (Files.exists(sampleFile)) parseKeyValueFile(sampleFile) else Map.empty[String, String]
      val enriched = enrichResult(
        resultValues,
        threshold = threshold,
        trialIndex = trialIndex,
        trialSeed = trialSeed,
        exitCode = exitCode,
        childFailureReason = childResult.failureReason,
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
    "postSelectionRandomSeed",
    "outcomeSelectionMode",
    "outcomeSelectionBoundary",
    "bits",
    "observedOutcome",
    "amplitude.real",
    "amplitude.imag",
    "amplitude.magnitude",
    "developedAmplitudeMagnitude",
    "maxIncorrectReadyAmplitudeAtSampling",
    "incorrectReadyMoleculesAtSampling",
    "maxIncorrectReadyAmplitudeAtSamplingExact",
    "incorrectReadyPoolSnapshotSemantics",
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
    "interferenceReactionsAtSampling",
    "interferenceReactionDefinition",
    "thresholdTriggerBits",
    "thresholdTriggerAmplitude.real",
    "thresholdTriggerAmplitude.imag",
    "thresholdTriggerAmplitude.magnitude",
    "thresholdTriggerWasSelected",
    "samplingPopulationMoleculesAtSampling",
    "samplingPopulationEndpointStatesAtSampling",
    "samplingPopulationAmplitudeSquaredMass",
    "bornRuleNormalizationFactor",
    "samplingPopulationNormalizedProbabilitySum",
    "samplingPopulationBornDistribution",
    "selectedStateNormalizedBornProbabilityAtSampling",
    "bornRuleRandomDraw",
    "bornRuleRandomSeed",
    "bornRuleProbabilityDefinition",
    "I_full",
    "I_fullContributions",
    "I_fullEndpointStates",
    "I_fullDefinition",
    "interferenceCompletionFraction",
    "interferenceReductionFraction",
    "idealSampledOutcomeProbability",
    "idealOutputDistribution",
    "idealDistributionDefinition",
    "selectedPostSelectionOutcome",
    "postSelectionProbability",
    "postSelectionAmplitudeScale",
    "postSelectionDescription",
    "selectedSimonOracleOutput",
    "selectedStateAggregateAmplitude.real",
    "selectedStateAggregateAmplitude.imag",
    "selectedStateAggregateAmplitude.magnitude",
    "maxIncorrectReadyStateAggregateAmplitudeAtSampling",
    "selectedVsMaxIncorrectStateAmplitudeMargin",
    "selectedVsMaxIncorrectStateAmplitudeRatio",
    "selectedStateAmplitudeRank",
    "readyMoleculesAtSampling",
    "readyCorrectMoleculesAtSampling",
    "readyIncorrectMoleculesAtSampling",
    "distinctReadyEndpointStatesAtSampling",
    "distinctCorrectReadyEndpointStatesAtSampling",
    "distinctIncorrectReadyEndpointStatesAtSampling",
    "readyPoolStateSnapshotExact",
    "readyPoolStateSnapshotSemantics",
    "pathPartitions",
    "pathGenerationParallelism",
    "fs2BranchJitterMillis",
    "fs2BranchJitterSeed",
    "fs2BranchJitterRealizedMinimumMillis",
    "fs2BranchJitterRealizedMaximumMillis",
    "fs2BranchJitterRealizedMeanMillis",
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
    records.toVector.filter(record => record.succeeded && record.boolean("isCorrect").isDefined)
  private val binaryMetrics = Set("isCorrect", "thresholdTriggerWasSelected")
  private val numericMetrics = Vector(
    "isCorrect",
    "thresholdTriggerWasSelected",
    "B_total",
    "B_correct",
    "B_active",
    "B_activecorrect",
    "B_activeFraction",
    "B_activeCorrectFraction",
    "B_correctAmongActiveFraction",
    "B_reductionFraction",
    "developedAmplitudeMagnitude",
    "maxIncorrectReadyAmplitudeAtSampling",
    "incorrectReadyMoleculesAtSampling",
    "selectedAtTerminalContributions",
    "interferenceReactionsAtSampling",
    "thresholdTriggerAmplitude.magnitude",
    "samplingPopulationMoleculesAtSampling",
    "samplingPopulationEndpointStatesAtSampling",
    "samplingPopulationAmplitudeSquaredMass",
    "bornRuleNormalizationFactor",
    "samplingPopulationNormalizedProbabilitySum",
    "selectedStateNormalizedBornProbabilityAtSampling",
    "I_full",
    "I_fullContributions",
    "I_fullEndpointStates",
    "interferenceCompletionFraction",
    "interferenceReductionFraction",
    "idealSampledOutcomeProbability",
    "selectedStateAggregateAmplitude.magnitude",
    "maxIncorrectReadyStateAggregateAmplitudeAtSampling",
    "selectedVsMaxIncorrectStateAmplitudeMargin",
    "selectedVsMaxIncorrectStateAmplitudeRatio",
    "selectedStateAmplitudeRank",
    "readyMoleculesAtSampling",
    "readyCorrectMoleculesAtSampling",
    "readyIncorrectMoleculesAtSampling",
    "distinctReadyEndpointStatesAtSampling",
    "distinctCorrectReadyEndpointStatesAtSampling",
    "distinctIncorrectReadyEndpointStatesAtSampling",
    "fs2BranchJitterRealizedMinimumMillis",
    "fs2BranchJitterRealizedMaximumMillis",
    "fs2BranchJitterRealizedMeanMillis",
    "elapsedMillis"
  )
  private val summaryRows = thresholds.flatMap { threshold =>
    val group = validRecords.filter(_.threshold == threshold)
    numericMetrics.flatMap { metric =>
      val values =
        if (binaryMetrics.contains(metric)) {
          group.flatMap(_.boolean(metric).map(if (_) 1d else 0d))
        } else {
          group.flatMap(_.number(metric))
        }
      if (values.isEmpty) {
        None
      } else {
        val summary =
          TrialStatistics.summarize(values, proportion = binaryMetrics.contains(metric))
        Some(
          Map(
            "experiment" -> experimentAlias,
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

  private val distributionObservations = thresholds.map { threshold =>
    val observations = validRecords
      .filter(_.threshold == threshold)
      .flatMap { record =>
        for {
          outcome <- record.values.get("observedOutcome").filter(_.nonEmpty)
          rendered <- record.values.get("idealOutputDistribution").filter(_.nonEmpty)
          ideal <- Try(OutputDistributionMetrics.parse(rendered)).toOption
          if ideal.nonEmpty
        } yield outcome -> ideal
      }
    threshold -> observations
  }.toMap

  private val distributionQualityRows = thresholds.flatMap { threshold =>
    val observations = distributionObservations.getOrElse(threshold, Vector.empty)
    if (observations.isEmpty) {
      None
    } else {
      val empirical = OutputDistributionMetrics.empirical(observations.map(_._1))
      val ideal = OutputDistributionMetrics.average(observations.map(_._2))
      val distance = OutputDistributionMetrics.distance(empirical, ideal)
      val bootstrap = OutputDistributionMetrics.bootstrap(
        observations,
        ExperimentConfig.distributionBootstrapReplicates,
        ExperimentRandom.bootstrapSeed(
          ExperimentConfig.randomSeed,
          java.lang.Double.doubleToLongBits(threshold)
        )
      )
      Some(
        Map(
          "experiment" -> experimentAlias,
          "threshold" -> threshold.toString,
          "n" -> observations.length.toString,
          "totalVariationDistance" -> distance.totalVariationDistance.toString,
          "tvdBootstrapStandardDeviation" ->
            bootstrap.totalVariation.standardDeviation.toString,
          "tvdBootstrapConfidenceLow95" ->
            bootstrap.totalVariation.confidenceLow95.toString,
          "tvdBootstrapConfidenceHigh95" ->
            bootstrap.totalVariation.confidenceHigh95.toString,
          "hellingerDistance" -> distance.hellingerDistance.toString,
          "hellingerFidelity" -> distance.hellingerFidelity.toString,
          "hellingerBootstrapStandardDeviation" ->
            bootstrap.hellinger.standardDeviation.toString,
          "hellingerBootstrapConfidenceLow95" ->
            bootstrap.hellinger.confidenceLow95.toString,
          "hellingerBootstrapConfidenceHigh95" ->
            bootstrap.hellinger.confidenceHigh95.toString,
          "bootstrapReplicates" -> bootstrap.replicates.toString,
          "referenceDistribution" ->
            renderDistribution(ideal),
          "definition" ->
            "empirical sampled-output distribution versus the mean trial-specific exact ideal distribution"
        )
      )
    }
  }
  private val distributionQualityColumns = Vector(
    "experiment",
    "threshold",
    "n",
    "totalVariationDistance",
    "tvdBootstrapStandardDeviation",
    "tvdBootstrapConfidenceLow95",
    "tvdBootstrapConfidenceHigh95",
    "hellingerDistance",
    "hellingerFidelity",
    "hellingerBootstrapStandardDeviation",
    "hellingerBootstrapConfidenceLow95",
    "hellingerBootstrapConfidenceHigh95",
    "bootstrapReplicates",
    "referenceDistribution",
    "definition"
  )
  writeCsv(
    batchDirectory.resolve("distribution-quality.csv"),
    distributionQualityColumns,
    distributionQualityRows
  )

  private val distributionDetailRows = thresholds.flatMap { threshold =>
    val observations = distributionObservations.getOrElse(threshold, Vector.empty)
    if (observations.isEmpty) {
      Vector.empty
    } else {
      val empirical = OutputDistributionMetrics.empirical(observations.map(_._1))
      val ideal = OutputDistributionMetrics.average(observations.map(_._2))
      val observedCounts = observations.map(_._1).groupBy(identity).map {
        case (outcome, values) => outcome -> values.length
      }
      (empirical.keySet ++ ideal.keySet).toVector.sorted.map { outcome =>
        Map(
          "experiment" -> experimentAlias,
          "threshold" -> threshold.toString,
          "outcome" -> outcome,
          "observedCount" -> observedCounts.getOrElse(outcome, 0).toString,
          "empiricalProbability" -> empirical.getOrElse(outcome, 0d).toString,
          "idealProbability" -> ideal.getOrElse(outcome, 0d).toString,
          "empiricalMinusIdeal" ->
            (empirical.getOrElse(outcome, 0d) - ideal.getOrElse(outcome, 0d)).toString
        )
      }
    }
  }
  writeCsv(
    batchDirectory.resolve("distribution-details.csv"),
    Vector(
      "experiment",
      "threshold",
      "outcome",
      "observedCount",
      "empiricalProbability",
      "idealProbability",
      "empiricalMinusIdeal"
    ),
    distributionDetailRows
  )

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

  val failed = records.count(!_.succeeded)
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
      childFailureReason: String,
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
    val failureReason =
      if (childFailureReason.nonEmpty) childFailureReason
      else if (exitCode != 0) s"child process exited with code $exitCode"
      else if (values.isEmpty) "child process produced no result file"
      else ""
    values ++ Map(
      "batchId" -> batchId,
      "experiment" -> experimentAlias,
      "trialIndex" -> trialIndex.toString,
      "trialSeed" -> trialSeed.toString,
      "processExitCode" -> exitCode.toString,
      "failureReason" -> failureReason,
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
      "developedAmplitudeMagnitude",
      "maxIncorrectReadyAmplitudeAtSampling",
      "interferenceReactionsAtSampling",
      "thresholdTriggerAmplitude.magnitude",
      "samplingPopulationMoleculesAtSampling",
      "samplingPopulationEndpointStatesAtSampling",
      "samplingPopulationAmplitudeSquaredMass",
      "bornRuleNormalizationFactor",
      "selectedStateNormalizedBornProbabilityAtSampling",
      "interferenceCompletionFraction",
      "interferenceReductionFraction",
      "idealSampledOutcomeProbability",
      "selectedStateAggregateAmplitude.magnitude",
      "maxIncorrectReadyStateAggregateAmplitudeAtSampling",
      "selectedVsMaxIncorrectStateAmplitudeMargin",
      "selectedVsMaxIncorrectStateAmplitudeRatio",
      "selectedStateAmplitudeRank",
      "readyMoleculesAtSampling",
      "readyIncorrectMoleculesAtSampling",
      "distinctReadyEndpointStatesAtSampling",
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
        ("B_activecorrect", "B_correct", "B_activecorrect versus B_correct"),
        (
          "interferenceReactionsAtSampling",
          "I_full",
          "interference reactions at sampling versus canonical full interference"
        )
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
      "experiment" -> experimentAlias,
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
      "experiment" -> experimentAlias,
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
    val successful = allRecords.count(_.succeeded)
    val summaryLines = summaries
      .filter(row =>
        row("metric") == "isCorrect" ||
          row("metric") == "interferenceReductionFraction"
      )
      .map { row =>
        s"threshold=${row("threshold")} ${row("metric")}: " +
          s"${row("meanPlusMinusStandardDeviation")}, " +
          s"95% CI [${row("confidenceLow95")}, ${row("confidenceHigh95")}]"
      }
    s"""Repeated CHAM experiment batch
       |batchId=$batchId
       |experiment=$experimentAlias
       |backend=${ExperimentConfig.repeatedTrialBackend.label}
       |outcomeSelectionMode=${ExperimentConfig.outcomeSelectionMode.label}
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
       |distribution-quality.csv: TVD and Hellinger distance with paired bootstrap uncertainty
       |distribution-details.csv: empirical and exact ideal probability for every outcome
       |pairwise-comparisons.csv: paired t-tests, Wilcoxon signed-rank tests, and exact McNemar tests
       |trials/: complete per-trial result records
       |logs/: complete per-trial process logs
       |
       |Pairwise comparison rows=${comparisons.length}
       |""".stripMargin
  }

  private def runChild(
      processBuilder: ProcessBuilder,
      logFile: Path
  ): ChildProcessResult = {
    val process = processBuilder.start()
    activeChild.set(process)
    try {
      val completed = process.waitFor(
        ExperimentConfig.repeatedTrialTimeoutMinutes,
        TimeUnit.MINUTES
      )
      if (completed) {
        ChildProcessResult(process.exitValue(), "")
      } else {
        terminateChild(process)
        val message =
          s"trial timed out after ${ExperimentConfig.repeatedTrialTimeoutMinutes} minute(s)"
        appendRunnerMessage(logFile, message)
        ChildProcessResult(124, message)
      }
    } catch {
      case interrupted: InterruptedException =>
        terminateChild(process)
        appendRunnerMessage(logFile, "trial interrupted; child process terminated")
        Thread.currentThread().interrupt()
        throw interrupted
    } finally {
      activeChild.compareAndSet(process, null)
    }
  }

  private def terminateChild(process: Process): Unit = {
    process.destroy()
    if (!process.waitFor(5L, TimeUnit.SECONDS)) {
      process.destroyForcibly()
      process.waitFor(5L, TimeUnit.SECONDS)
      ()
    }
  }

  private def appendRunnerMessage(path: Path, message: String): Unit =
    Files.write(
      path,
      s"\n[repeated-runner] $message\n".getBytes(StandardCharsets.UTF_8),
      StandardOpenOption.CREATE,
      StandardOpenOption.APPEND,
      StandardOpenOption.WRITE
    )

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

  private def renderDistribution(distribution: Map[String, Double]): String =
    distribution.toVector.sortBy(_._1).map {
      case (outcome, probability) => s"$outcome:$probability"
    }.mkString("|")
}
