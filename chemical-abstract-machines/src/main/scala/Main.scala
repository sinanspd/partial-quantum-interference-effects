package com.sinanspd

import cats.effect.{Deferred, IO}
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import com.sinanspd.qure.circuit.QVec
import com.sinanspd.qure.circuit.gates._
import fs2.Stream
import io.chymyst.jc._
import spire.implicits._
import spire.math.Complex

import java.nio.file.Paths
import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference, LongAdder}
import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.Random

/**
  * Bounded FS2 path generation with CHAM endpoint interference.
  *
  * Unlike the legacy prototype that used to live in this file, this entry
  * point runs the circuit selected in ExperimentConfig and implements the same
  * sampling, correctness, metrics, and child-process protocol as Cham2.
  */
object Main extends App {
  private final case class PathPartition(state: QVec, remainingGates: List[Gate])

  private final case class ActivitySnapshot(
      generatedTerminals: Long,
      interferenceReactions: Long,
      bActive: Long,
      bActiveCorrect: Long,
      selection: ThresholdSelectionDecision
  )

  private val backendLabel = "bounded FS2 path generation + CHAM endpoint interference"
  private val startedAtNanos = System.nanoTime()
  private val experiment = ExperimentCatalog(ExperimentConfig.circuitAlias)
  private val threshold = sys.props
    .get(TrialProcessProtocol.thresholdProperty)
    .fold(ExperimentConfig.threshold)(_.toDouble)
  private val effectiveRandomSeed = sys.props
    .get(TrialProcessProtocol.randomSeedProperty)
    .fold(ExperimentConfig.randomSeed)(_.toLong)
  private val trialId = sys.props.get(TrialProcessProtocol.trialIdProperty)
  private val instanceCount =
    ExperimentConfig.instanceCountOverride.getOrElse(experiment.instancesForThreshold(threshold))
  private val bTotalPerCopy = experiment.leafMetrics.total
  private val bCorrectPerCopy = experiment.leafMetrics.correct
  private val bTotal = bTotalPerCopy * instanceCount
  private val bCorrect = bCorrectPerCopy * instanceCount

  require(threshold > 0d, "ExperimentConfig.threshold must be greater than zero")
  require(instanceCount > 0, "The experiment must use at least one circuit instance")
  require(ExperimentConfig.workerThreads > 0, "workerThreads must be greater than zero")
  require(
    ExperimentConfig.fs2BranchJitterMillis >= 0 &&
      ExperimentConfig.fs2BranchJitterMillis < Int.MaxValue,
    "fs2BranchJitterMillis must be between zero and Int.MaxValue - 1"
  )
  require(
    ExperimentConfig.completionJitterMillis >= 0,
    "completionJitterMillis cannot be negative"
  )
  require(ExperimentConfig.shutdownDrainMillis >= 0, "shutdownDrainMillis cannot be negative")
  require(experiment.gates.nonEmpty, s"${experiment.alias} has no gates to stream")

  private val selectedSimonOutput = experiment.simonPostSelection.map { selection =>
    val random = new Random(effectiveRandomSeed)
    selection.possibleOracleOutputs(random.nextInt(selection.possibleOracleOutputs.length))
  }
  private val referenceMetrics =
    CircuitReferenceMetrics.calculate(experiment, selectedSimonOutput, instanceCount)
  private val bornRuleRandomSeed = effectiveRandomSeed ^ 0x5DEECE66DL
  private val outcomeSelectionRandom = new Random(bornRuleRandomSeed)
  private val fs2BranchJitterSeed =
    Fs2BranchJitter.resolveSeed(ExperimentConfig.fs2BranchJitterSeedOverride)

  private val samplePath = ExperimentOutputPaths.samplePath(
    explicitPath =
      sys.props.get(TrialProcessProtocol.sampleFileProperty).map(Paths.get(_)),
    batchRoot = ExperimentConfig.repeatedTrialOutputDirectory,
    experimentAlias = experiment.alias,
    selectionMode = ExperimentConfig.outcomeSelectionMode,
    threshold = threshold
  )
  private val sampledState = new SampledStateRecorder(
    configuredOutputPath = Some(samplePath),
    context = Vector(
      "experiment" -> experiment.alias,
      "backend" -> backendLabel,
      "threshold" -> threshold.toString,
      "instances" -> instanceCount.toString,
      "qubits" -> experiment.qubitCount.toString,
      "randomSeed" -> effectiveRandomSeed.toString,
      "outcomeSelectionMode" -> ExperimentConfig.outcomeSelectionMode.label,
      "fs2BranchJitterMillis" -> ExperimentConfig.fs2BranchJitterMillis.toString,
      "fs2BranchJitterSeed" -> fs2BranchJitterSeed.toString
    ) ++ trialId.map("trialId" -> _).toVector
  )

  private val generatedTerminalContributions = new LongAdder
  private val interferenceReactions = new LongAdder
  private val readyTerminalMolecules = new LongAdder
  private val readyCorrectTerminalMolecules = new LongAdder
  private val stopRequested = new AtomicBoolean(false)
  private val activityLock = new AnyRef
  private val readyPoolAmplitudeTracker = new ReadyPoolAmplitudeTracker
  private var interferenceReactionsInFlight = 0L
  private val selectedActivity = new AtomicReference[ActivitySnapshot]()
  private val hScale = 1d / math.sqrt(2d)
  private lazy val endpointPool = BlockingPool(ExperimentConfig.workerThreads)
  private lazy val ready = m[ReadyMolecule]

  private lazy val reactionSite: Unit = {
    site(endpointPool)(
      go {
        case ready(left) + ready(right)
            if left.state.v.sameElements(right.state.v) =>
          var selected = Option.empty[QVec]

          activityLock.synchronized {
            interferenceReactionsInFlight += 1L
            readyPoolAmplitudeTracker.consumed(left)
            readyPoolAmplitudeTracker.consumed(right)
          }

          try {
            require(
              left.isCorrect == right.isCorrect,
              s"Endpoint ${left.state.v} was assigned inconsistent correctness"
            )
            val combined = QVec(
              Complex(
                left.state.prop.real + right.state.prop.real,
                left.state.prop.imag + right.state.prop.imag
              ),
              left.state.v
            )

            activityLock.synchronized {
              if (!stopRequested.get()) {
                interferenceReactions.increment()
                if (combined.prop.abs >= threshold) {
                  stopRequested.set(true)
                  val snapshot =
                    captureActivity(ReadyMolecule(combined, left.isCorrect))
                  selectedActivity.set(snapshot)
                  selected = Some(snapshot.selection.selected.state)
                } else if (combined.prop.abs != 0d) {
                  emitReadyLocked(combined, left.isCorrect)
                }
              }
            }
          } finally {
            activityLock.synchronized {
              interferenceReactionsInFlight -= 1L
              require(
                interferenceReactionsInFlight >= 0L,
                "Endpoint interference in-flight count became negative"
              )
              activityLock.notifyAll()
            }
          }

          selected.foreach { stateToRecord =>
            require(
              sampledState.tryRecord(stateToRecord),
              "The selected state recorder was already populated"
            )
          }
      }
    )
    ()
  }

  private def acceptTerminal(state: QVec): Unit = {
    val terminal = terminalState(state)
    var selected: Option[QVec] = None

    activityLock.synchronized {
      if (!stopRequested.get()) {
        generatedTerminalContributions.increment()
        terminal.foreach { terminalState =>
          readyTerminalMolecules.increment()
          val isCorrect = experiment.isCorrectTerminalState(terminalState.v)
          if (isCorrect) {
            readyCorrectTerminalMolecules.increment()
          }

          if (terminalState.prop.abs >= threshold) {
            stopRequested.set(true)
            val snapshot =
              captureActivity(ReadyMolecule(terminalState, isCorrect))
            selectedActivity.set(snapshot)
            selected = Some(snapshot.selection.selected.state)
          } else {
            emitReadyLocked(terminalState, isCorrect)
          }
        }
      }
    }

    selected match {
      case Some(stateToRecord) =>
        require(
          sampledState.tryRecord(stateToRecord),
          "The selected state recorder was already populated"
        )
      case None => ()
    }
  }

  private def terminalState(state: QVec): Option[QVec] =
    experiment.simonPostSelection match {
      case Some(selection) =>
        val measuredOutput = selectedSimonOutput.get
        if (state.v.takeRight(selection.inputQubits).sameElements(measuredOutput)) {
          Some(QVec(state.prop, state.v.take(selection.inputQubits)))
        } else {
          None
        }
      case None =>
        experiment.resultQubits match {
          case Some(qubits) =>
            Some(QVec(state.prop, qubits.map(state.v)))
          case None =>
            Some(state)
        }
    }

  /** Must be called while holding activityLock. */
  private def emitReadyLocked(state: QVec, isCorrect: Boolean): Unit = {
    val molecule = ReadyMolecule(state, isCorrect)
    readyPoolAmplitudeTracker.emitted(molecule)
    ready(molecule)
  }

  private def captureActivity(thresholdTrigger: ReadyMolecule): ActivitySnapshot = {
    val selection = readyPoolAmplitudeTracker.select(
      thresholdTrigger,
      ExperimentConfig.outcomeSelectionMode,
      outcomeSelectionRandom
    )
    ActivitySnapshot(
      generatedTerminals = generatedTerminalContributions.sum(),
      interferenceReactions = interferenceReactions.sum(),
      bActive = readyTerminalMolecules.sum(),
      bActiveCorrect = readyCorrectTerminalMolecules.sum(),
      selection = selection
    )
  }

  /**
    * Called only after the FS2 terminal stream has completed normally.
    *
    * Molecules already dispatched to a reaction remain in the shadow tracker
    * until the reaction body starts. Once it starts, the in-flight count keeps
    * the pool non-quiescent until the combined molecule has been emitted or
    * discarded. Together, those signals close the gap between consuming a
    * pair and emitting its result.
    */
  private def awaitSampleOrQuiescentFailure(): QVec = {
    var exhaustedPool = Option.empty[ReadyPoolProgressSnapshot]

    activityLock.synchronized {
      var progress = readyPoolAmplitudeTracker.progressSnapshot()
      while (
        !stopRequested.get() &&
        (interferenceReactionsInFlight > 0L || progress.hasCompatiblePair)
      ) {
        activityLock.wait()
        progress = readyPoolAmplitudeTracker.progressSnapshot()
      }

      if (!stopRequested.get()) {
        exhaustedPool = Some(progress)
        stopRequested.set(true)
      }
    }

    exhaustedPool.foreach { progress =>
      throw new IllegalStateException(
        s"FS2 path generation and endpoint interference completed without any " +
          s"molecule reaching threshold $threshold. The quiescent ready pool " +
          s"contains ${progress.moleculeCount} molecule(s) across " +
          s"${progress.distinctEndpointStates} endpoint state(s), with no " +
          s"compatible pair remaining. This trial cannot satisfy the configured " +
          s"first-threshold-crossing boundary."
      )
    }

    // A reaction can set stopRequested immediately before it records the
    // selected state. Wait for that short hand-off instead of reporting a
    // spurious no-threshold failure.
    sampledState.awaitSample()
  }

  private def terminateFailedExperiment(error: Throwable): Nothing = {
    stopRequested.set(true)
    Console.err.println(s"Experiment ${experiment.alias} failed: ${error.getMessage}")
    error.printStackTrace(Console.err)
    endpointPool.shutdownNow()
    System.exit(1)
    throw error
  }

  /**
    * Split only enough leading work to keep the configured workers busy.
    * Everything after this bounded frontier remains lazy in FS2.
    */
  @tailrec
  private def buildPartitions(
      frontier: Vector[PathPartition]
  ): Vector[PathPartition] = {
    if (
      frontier.length >= ExperimentConfig.workerThreads ||
      frontier.forall(_.remainingGates.isEmpty)
    ) {
      frontier
    } else {
      val next = frontier.flatMap {
        case partition @ PathPartition(_, Nil) =>
          Vector(partition)
        case PathPartition(state, gate :: remaining) =>
          applyGate(gate, state).map(nextState => PathPartition(nextState, remaining))
      }
      buildPartitions(next)
    }
  }

  private def streamPaths(
      state: QVec,
      remainingGates: List[Gate]
  ): Stream[IO, QVec] = {
    if (stopRequested.get()) {
      Stream.empty
    } else {
      remainingGates match {
        case Nil =>
          Stream.emit(state)
        case gate :: remaining =>
          Stream
            .emits(applyGate(gate, state))
            .covary[IO]
            .flatMap(nextState => streamPaths(nextState, remaining))
      }
    }
  }

  private def emitTerminal(state: QVec): IO[Unit] = {
    if (stopRequested.get()) {
      IO.unit
    } else {
      val jitter =
        if (ExperimentConfig.completionJitterMillis == 0) 0
        else
          ThreadLocalRandom
            .current()
            .nextInt(ExperimentConfig.completionJitterMillis)
      val delay = if (jitter == 0) IO.unit else IO.sleep(jitter.millis)
      delay *> IO(acceptTerminal(state))
    }
  }

  private def applyGate(gate: Gate, state: QVec): Vector[QVec] =
    gate match {
      case X(target) =>
        Vector(QVec(state.prop, state.v.updated(target, !state.v(target))))

      case H(target) =>
        val sign = if (state.v(target)) -1d else 1d
        Vector(
          QVec(
            Complex(sign * hScale * state.prop.real, sign * hScale * state.prop.imag),
            state.v
          ),
          QVec(
            Complex(hScale * state.prop.real, hScale * state.prop.imag),
            state.v.updated(target, !state.v(target))
          )
        )

      case CX(control, target) =>
        val nextBits =
          if (state.v(control)) state.v.updated(target, !state.v(target)) else state.v
        Vector(QVec(state.prop, nextBits))

      case CCX(control1, control2, target) =>
        val nextBits =
          if (state.v(control1) && state.v(control2))
            state.v.updated(target, !state.v(target))
          else state.v
        Vector(QVec(state.prop, nextBits))

      case CZ(control, target) =>
        val nextAmplitude =
          if (state.v(control) && state.v(target)) state.prop * -1d else state.prop
        Vector(QVec(nextAmplitude, state.v))

      case PhaseFlipWhenAllOne(qubits) =>
        val nextAmplitude =
          if (qubits.forall(state.v)) state.prop * -1d else state.prop
        Vector(QVec(nextAmplitude, state.v))

      case Swap(q1, q2) =>
        val swapped = state.v.updated(q1, state.v(q2)).updated(q2, state.v(q1))
        Vector(QVec(state.prop, swapped))

      case RZ(thetaDenominator, target) =>
        Vector(
          QVec(
            RotationMath.applyRz(state.prop, thetaDenominator, state.v(target)),
            state.v
          )
        )

      case CRotate(control, thetaDenominator, target) =>
        Vector(
          QVec(
            RotationMath.applyControlledRotate(
              state.prop,
              thetaDenominator,
              state.v(control),
              state.v(target)
            ),
            state.v
          )
        )

      case Rotate(thetaDenominator, target) =>
        Vector(
          QVec(
            RotationMath.applyPhase(state.prop, thetaDenominator, state.v(target)),
            state.v
          )
        )

      case ModularExponentiation(base, modulus, inputQubits, outputQubits) =>
        val exponent = inputQubits.foldLeft(0) { (value, qubit) =>
          (value << 1) | (if (state.v(qubit)) 1 else 0)
        }
        val modularResult =
          BigInt(base).modPow(BigInt(exponent), BigInt(modulus)).toInt
        val nextBits = outputQubits.zipWithIndex.foldLeft(state.v) {
          case (current, (qubit, index)) =>
            val shift = outputQubits.length - index - 1
            current.updated(qubit, ((modularResult >> shift) & 1) == 1)
        }
        Vector(QVec(state.prop, nextBits))

      case Measure(_) =>
        throw new UnsupportedOperationException(
          "Measurements must be represented by an experiment terminal policy"
        )

      case unsupported =>
        throw new UnsupportedOperationException(
          s"${experiment.alias} contains unsupported gate $unsupported"
        )
    }

  private def formatPathEstimate(hadamards: Long): String = {
    val log10 = hadamards * math.log10(2d) + math.log10(instanceCount.toDouble)
    if (hadamards <= 50) {
      (BigInt(instanceCount) * (BigInt(1) << hadamards.toInt)).toString
    } else {
      f"$instanceCount%,d x 2^$hadamards%,d (approximately 10^$log10%.1f)"
    }
  }

  private val correctnessDescription =
    experiment.shorPostProcessing
      .map(_.correctnessDescription)
      .getOrElse(experiment.correctOutcomes.description)

  private val successTarget =
    experiment.shorPostProcessing
      .map(_.targetDescription)
      .getOrElse(experiment.correctOutcomes.renderedStates)

  private def printRunSummary(): Unit = {
    println(
      s"""
         |===================== EXPERIMENT =====================
         |alias:       ${experiment.alias}
         |description: ${experiment.description}
         |backend:     $backendLabel
         |qubits:      ${experiment.qubitCount}
         |instances:   $instanceCount
         |copy source: ${if (ExperimentConfig.instanceCountOverride.isDefined) "override" else "threshold/catalog"}
         |threshold:   $threshold
         |selection:   ${ExperimentConfig.outcomeSelectionMode.label}
         |FS2 jitter:  0-${ExperimentConfig.fs2BranchJitterMillis} ms per bounded branch
         |Hadamards:   ${experiment.hadamardCount}
         |path scale:  ${formatPathEstimate(experiment.hadamardCount.toLong)}
         |B_total:     $bTotal ($bTotalPerCopy per copy)
         |B_correct:   $bCorrect ($bCorrectPerCopy per copy)
         |I_full:      ${referenceMetrics.fullInterferenceReactions}
         |I endpoints: ${referenceMetrics.fullInterferenceEndpointStates}
         |check bits:  ${experiment.correctOutcomes.observedQubits.mkString(",")}
         |success:     $successTarget
         |output:      ${samplePath.toAbsolutePath.normalize}
         |aliases:     ${ExperimentCatalog.aliases.mkString(", ")}
         |======================================================""".stripMargin
    )

    selectedSimonOutput.foreach { output =>
      println(s"Simon post-selected oracle output: ${output.map(if (_) '1' else '0').mkString}")
    }
    experiment.paperTerminalContributions.foreach { total =>
      println(s"Paper terminal-contribution count: $total")
    }
    experiment.shorPostProcessing.foreach { _ =>
      println(s"Reference quantum frequency bins: ${experiment.correctOutcomes.renderedStates}")
    }
    println(s"Correctness definition: $correctnessDescription")
    experiment.notes.foreach(note => println(s"Note: $note"))
  }

  printRunSummary()

  if (ExperimentConfig.preflightOnly) {
    println("Preflight complete; no path stream or circuit molecules were created.")
    System.exit(0)
  }

  reactionSite
  private val initialPartitions = Vector.fill(instanceCount)(
    PathPartition(experiment.initialState, experiment.gates)
  )
  private val partitions = buildPartitions(initialPartitions)
  private val branchJitterSchedule = Fs2BranchJitter.schedule(
    partitions.length,
    ExperimentConfig.fs2BranchJitterMillis,
    fs2BranchJitterSeed
  )
  private val pathParallelism =
    math.max(1, math.min(ExperimentConfig.workerThreads, partitions.length))
  println(
    s"FS2 path generation: ${partitions.length} bounded partition(s), " +
      s"parallelism=$pathParallelism, branch jitter " +
      f"min=${branchJitterSchedule.minimumMillis}ms " +
      f"max=${branchJitterSchedule.maximumMillis}ms " +
      f"mean=${branchJitterSchedule.meanMillis}%.2fms " +
      s"seed=${branchJitterSchedule.seed}"
  )

  private val terminalStream = Stream
    .emits(partitions.zip(branchJitterSchedule.delaysMillis).map {
      case (partition, delayMillis) =>
        val delay =
          if (delayMillis == 0) Stream.empty
          else Stream.eval(IO.sleep(delayMillis.millis)).drain
        delay ++ streamPaths(partition.state, partition.remainingGates)
    })
    .covary[IO]
    .parJoin(pathParallelism)
    .parEvalMapUnordered(ExperimentConfig.workerThreads)(emitTerminal)

  private val selected = {
    val program = for {
      generationResult <- Deferred[IO, Either[Throwable, Unit]]
      generationFiber <- terminalStream.compile.drain.attempt
        .flatMap(result => generationResult.complete(result).void)
        .start
      result <- IO
        .interruptible(sampledState.awaitSample())
        .race(
          generationResult.get.flatMap {
            case Left(error) => IO.raiseError[QVec](error)
            case Right(_)    => IO.interruptible(awaitSampleOrQuiescentFailure())
          }
        )
        .guarantee(generationFiber.cancel)
    } yield result.fold(identity, identity)

    program.attempt.unsafeRunSync() match {
      case Right(sample) => sample
      case Left(error)   => terminateFailedExperiment(error)
    }
  }

  stopRequested.set(true)
  private val observedOutcome = experiment.correctOutcomes.observe(selected.v)
  private val observedBits = observedOutcome.map(if (_) '1' else '0').mkString
  private val shorPostProcessingResult =
    experiment.shorPostProcessing.map(_.process(observedOutcome))
  private val isCorrect = shorPostProcessingResult
    .map(_.success)
    .getOrElse(experiment.correctOutcomes.states.contains(observedOutcome))
  private val elapsedMillis = (System.nanoTime() - startedAtNanos) / 1000000L
  private val activityAtSelection = Option(selectedActivity.get()).getOrElse(
    throw new IllegalStateException("A sampled state was recorded without an activity snapshot")
  )
  private val terminalContributionsAtSelection = activityAtSelection.generatedTerminals
  private val interferenceReactionsAtSelection = activityAtSelection.interferenceReactions
  private val developedAmplitudeMagnitude = selected.prop.abs
  private val selectionDecision = activityAtSelection.selection
  private val readyPoolAtSelection = selectionDecision.readyPool
  private val incorrectReadyPoolAtSelection = readyPoolAtSelection.incorrectMolecules
  private val fullInterferenceReactions = referenceMetrics.fullInterferenceReactions
  private val interferenceCompletionFraction =
    if (fullInterferenceReactions == 0) 1d
    else
      (BigDecimal(interferenceReactionsAtSelection) /
        BigDecimal(fullInterferenceReactions)).toDouble
  private val interferenceReductionFraction = 1d - interferenceCompletionFraction
  private val idealSampledOutcomeProbability =
    referenceMetrics.idealProbability(observedOutcome)
  private val correctnessMetadata =
    experiment.shorPostProcessing match {
      case Some(postProcessing) =>
        Vector(
          "correctnessMethod" -> "shor-classical-factor-recovery",
          "correctOutcomeStates" -> "post-processing-dependent",
          "referenceFrequencyBins" -> experiment.correctOutcomes.renderedStates,
          "shorMaxIntermediateConvergents" ->
            postProcessing.maxIntermediateConvergents.toString,
          "shorMaxDenominatorMultiple" ->
            postProcessing.maxDenominatorMultiple.toString
        )
      case None =>
        Vector(
          "correctnessMethod" -> "accepted-state-membership",
          "correctOutcomeStates" -> experiment.correctOutcomes.renderedStates
        )
    }

  sampledState.appendMetadata(
    Vector(
      "selectedAtTerminalContributions" -> terminalContributionsAtSelection.toString,
      "selectedAtInterferenceReactions" -> interferenceReactionsAtSelection.toString,
      "interferenceReactionsAtSampling" -> interferenceReactionsAtSelection.toString,
      "interferenceReactionDefinition" ->
        "completed compatible endpoint combinations; incompatible molecule encounters excluded",
      "outcomeSelectionBoundary" -> "first state-molecule threshold crossing",
      "thresholdTriggerBits" ->
        selectionDecision.thresholdTrigger.state.v.map(if (_) '1' else '0').mkString,
      "thresholdTriggerAmplitude.real" ->
        selectionDecision.thresholdTrigger.state.prop.real.toString,
      "thresholdTriggerAmplitude.imag" ->
        selectionDecision.thresholdTrigger.state.prop.imag.toString,
      "thresholdTriggerAmplitude.magnitude" ->
        selectionDecision.thresholdTrigger.amplitudeMagnitude.toString,
      "thresholdTriggerWasSelected" ->
        selectionDecision.thresholdTriggerWasSelected.toString,
      "samplingPopulationMoleculesAtSampling" ->
        selectionDecision.samplingPopulationMoleculeCount.toString,
      "samplingPopulationEndpointStatesAtSampling" ->
        selectionDecision.samplingPopulationEndpointStateCount.toString,
      "samplingPopulationAmplitudeSquaredMass" ->
        selectionDecision.amplitudeSquaredMass.toString,
      "bornRuleNormalizationFactor" ->
        selectionDecision.bornRuleNormalizationFactor.toString,
      "samplingPopulationNormalizedProbabilitySum" ->
        selectionDecision.normalizedProbabilitySum.toString,
      "samplingPopulationBornDistribution" ->
        selectionDecision.renderedNormalizedStateProbabilities,
      "selectedStateNormalizedBornProbabilityAtSampling" ->
        selectionDecision.selectedStateNormalizedBornProbability.toString,
      "bornRuleRandomDraw" ->
        selectionDecision.bornRuleRandomDraw.map(_.toString).getOrElse("not-used"),
      "bornRuleRandomSeed" -> bornRuleRandomSeed.toString,
      "bornRuleProbabilityDefinition" ->
        ReadyPoolAmplitudeTracker.bornRuleProbabilityDefinition,
      "I_full" -> fullInterferenceReactions.toString,
      "I_fullContributions" ->
        referenceMetrics.fullInterferenceContributions.toString,
      "I_fullEndpointStates" ->
        referenceMetrics.fullInterferenceEndpointStates.toString,
      "I_fullDefinition" -> CircuitReferenceMetrics.fullInterferenceDefinition,
      "interferenceCompletionFraction" -> interferenceCompletionFraction.toString,
      "interferenceReductionFraction" -> interferenceReductionFraction.toString,
      "idealSampledOutcomeProbability" -> idealSampledOutcomeProbability.toString,
      "idealOutputDistribution" -> referenceMetrics.renderedIdealDistribution,
      "idealDistributionDefinition" ->
        CircuitReferenceMetrics.idealDistributionDefinition,
      "developedAmplitudeMagnitude" -> developedAmplitudeMagnitude.toString,
      "maxIncorrectReadyAmplitudeAtSampling" ->
        incorrectReadyPoolAtSelection.maximumAmplitude.toString,
      "incorrectReadyMoleculesAtSampling" ->
        incorrectReadyPoolAtSelection.moleculeCount.toString,
      "maxIncorrectReadyAmplitudeAtSamplingExact" -> "false",
      "incorrectReadyPoolSnapshotSemantics" ->
        ReadyPoolAmplitudeTracker.snapshotSemantics,
      "selectedStateAggregateAmplitude.real" ->
        readyPoolAtSelection.selectedStateAggregateAmplitude.real.toString,
      "selectedStateAggregateAmplitude.imag" ->
        readyPoolAtSelection.selectedStateAggregateAmplitude.imag.toString,
      "selectedStateAggregateAmplitude.magnitude" ->
        readyPoolAtSelection.selectedStateAggregateAmplitudeMagnitude.toString,
      "maxIncorrectReadyStateAggregateAmplitudeAtSampling" ->
        readyPoolAtSelection.maximumIncorrectReadyStateAggregateAmplitude.toString,
      "selectedVsMaxIncorrectStateAmplitudeMargin" ->
        readyPoolAtSelection.selectedVsMaxIncorrectStateAmplitudeMargin.toString,
      "selectedVsMaxIncorrectStateAmplitudeRatio" ->
        readyPoolAtSelection.selectedVsMaxIncorrectStateAmplitudeRatio
          .map(_.toString)
          .getOrElse("undefined-no-incorrect-ready-state"),
      "selectedStateAmplitudeRank" ->
        readyPoolAtSelection.selectedStateAmplitudeRank.toString,
      "readyMoleculesAtSampling" ->
        readyPoolAtSelection.readyMoleculeCount.toString,
      "readyCorrectMoleculesAtSampling" ->
        readyPoolAtSelection.correctReadyMoleculeCount.toString,
      "readyIncorrectMoleculesAtSampling" ->
        readyPoolAtSelection.incorrectReadyMoleculeCount.toString,
      "distinctReadyEndpointStatesAtSampling" ->
        readyPoolAtSelection.distinctReadyEndpointStates.toString,
      "distinctCorrectReadyEndpointStatesAtSampling" ->
        readyPoolAtSelection.distinctCorrectReadyEndpointStates.toString,
      "distinctIncorrectReadyEndpointStatesAtSampling" ->
        readyPoolAtSelection.distinctIncorrectReadyEndpointStates.toString,
      "readyPoolStateSnapshotExact" -> "false",
      "readyPoolStateSnapshotSemantics" ->
        ReadyPoolAmplitudeTracker.snapshotSemantics,
      "B_total" -> bTotal.toString,
      "B_correct" -> bCorrect.toString,
      "B_active" -> activityAtSelection.bActive.toString,
      "B_activecorrect" -> activityAtSelection.bActiveCorrect.toString,
      "B_totalPerCopy" -> bTotalPerCopy.toString,
      "B_correctPerCopy" -> bCorrectPerCopy.toString,
      "pathPartitions" -> partitions.length.toString,
      "pathGenerationParallelism" -> pathParallelism.toString,
      "fs2BranchJitterRealizedMinimumMillis" ->
        branchJitterSchedule.minimumMillis.toString,
      "fs2BranchJitterRealizedMaximumMillis" ->
        branchJitterSchedule.maximumMillis.toString,
      "fs2BranchJitterRealizedMeanMillis" ->
        branchJitterSchedule.meanMillis.toString,
      "observedOutcome" -> observedBits,
      "isCorrect" -> isCorrect.toString,
      "correctnessDefinition" -> correctnessDescription,
      "elapsedMillis" -> elapsedMillis.toString
    ) ++ selectedSimonOutput
      .map(output =>
        "selectedSimonOracleOutput" -> output.map(if (_) '1' else '0').mkString
      )
      .toVector ++ correctnessMetadata ++ shorPostProcessingResult.toVector.flatMap(_.metadata)
  )

  shorPostProcessingResult.foreach { result =>
    val postProcessing = experiment.shorPostProcessing.get
    result.printReport(postProcessing.modulus, postProcessing.base)
  }
  println(
    s"""
       |=================== OUTCOME CHECK ====================
       |observed:       $observedBits
       |correct:        ${if (isCorrect) "YES" else "NO"}
       |selection mode: ${ExperimentConfig.outcomeSelectionMode.label}
       |trigger:        ${selectionDecision.thresholdTrigger.state.v.map(if (_) '1' else '0').mkString}
       |developed amp:  $developedAmplitudeMagnitude
       |state agg amp:  ${readyPoolAtSelection.selectedStateAggregateAmplitudeMagnitude}
       |max wrong state:${readyPoolAtSelection.maximumIncorrectReadyStateAggregateAmplitude}
       |state rank:     ${readyPoolAtSelection.selectedStateAmplitudeRank}
       |interference:   $interferenceReactionsAtSelection / $fullInterferenceReactions
       |I reduction:    $interferenceReductionFraction
       |distribution p: $idealSampledOutcomeProbability
       |partial mass:   ${selectionDecision.amplitudeSquaredMass}
       |partial p:      ${selectionDecision.selectedStateNormalizedBornProbability}
       |ready soup:     ${readyPoolAtSelection.readyMoleculeCount} molecule(s), ${readyPoolAtSelection.distinctReadyEndpointStates} state(s)
       |B_total:        $bTotal
       |B_correct:      $bCorrect
       |B_active:       ${activityAtSelection.bActive}
       |B_activecorrect:${activityAtSelection.bActiveCorrect}
       |success target: $successTarget
       |definition:     $correctnessDescription
       |======================================================""".stripMargin
  )
  println(
    s"Run complete: sampled ${selected.v.map(if (_) '1' else '0').mkString}; " +
      s"generated terminals=$terminalContributionsAtSelection, " +
      s"interference reactions=$interferenceReactionsAtSelection, elapsed=${elapsedMillis}ms"
  )

  if (ExperimentConfig.terminateAfterSample) {
    if (ExperimentConfig.shutdownDrainMillis > 0) {
      Thread.sleep(ExperimentConfig.shutdownDrainMillis.toLong)
    }
    endpointPool.shutdownNow()
    System.exit(0)
  }
}
