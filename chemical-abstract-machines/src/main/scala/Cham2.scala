package com.sinanspd

import com.sinanspd.qure.circuit.gates._
import com.sinanspd.qure.circuit.{Circuit, QVec}
import io.chymyst.jc._
import spire.math.Complex
import spire.implicits._

import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicReference, LongAdder}
import java.nio.file.Paths
import scala.util.Random

/**
  * Experiment runner for the CHAM implementation.
  *
  * Select an experiment by editing ExperimentConfig.circuitAlias, then use
  * `sbt run`. No program argument is consulted.
  */
object Cham2 extends App {
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
    ExperimentConfig.completionJitterMillis >= 0,
    "completionJitterMillis cannot be negative"
  )
  require(ExperimentConfig.shutdownDrainMillis >= 0, "shutdownDrainMillis cannot be negative")

  private val selectedSimonOutput = experiment.simonPostSelection.map { selection =>
    val random = new Random(effectiveRandomSeed)
    selection.possibleOracleOutputs(random.nextInt(selection.possibleOracleOutputs.length))
  }

  private val samplePath =
    sys.props
      .get(TrialProcessProtocol.sampleFileProperty)
      .map(Paths.get(_))
      .getOrElse(
        ExperimentConfig.outputDirectory.resolve(s"${experiment.alias}-sampled-state.txt")
      )
  private val sampledState = new SampledStateRecorder(
    configuredOutputPath = Some(samplePath),
    context = Vector(
      "experiment" -> experiment.alias,
      "backend" -> experiment.backend.label,
      "threshold" -> threshold.toString,
      "instances" -> instanceCount.toString,
      "qubits" -> experiment.qubitCount.toString,
      "randomSeed" -> effectiveRandomSeed.toString
    ) ++ trialId.map("trialId" -> _).toVector
  )

  private val generatedTerminalContributions = new LongAdder
  private val interferenceReactions = new LongAdder
  private val readyTerminalMolecules = new LongAdder
  private val readyCorrectTerminalMolecules = new LongAdder
  private val worldIds = new AtomicLong(0L)
  private val stopRequested = new AtomicBoolean(false)
  private val activityLock = new AnyRef
  private final case class ActivitySnapshot(
      generatedTerminals: Long,
      interferenceReactions: Long,
      bActive: Long,
      bActiveCorrect: Long
  )
  private val selectedActivity = new AtomicReference[ActivitySnapshot]()
  private val hScale = 1d / math.sqrt(2d)
  private lazy val workerPool = BlockingPool(ExperimentConfig.workerThreads)

  // A `ready` molecule has completed its circuit and may interfere. A
  // `commit` molecule still has gates to execute and cannot be sampled.
  private lazy val ready = m[QVec]
  private lazy val commit = m[(QVec, Circuit, String)]
  private lazy val step = m[Unit]

  private lazy val reactionSites: Unit = {
    site(workerPool)(
      go {
        case ready(left) + ready(right) =>
          if (stopRequested.get()) {
            ()
          } else if (left.v.sameElements(right.v)) {
            interferenceReactions.increment()
            val combined = QVec(
              Complex(
                left.prop.real + right.prop.real,
                left.prop.imag + right.prop.imag
              ),
              left.v
            )

            if (combined.prop.abs >= threshold) {
              selectSample(combined)
            } else if (combined.prop.abs != 0d && !stopRequested.get()) {
              ready(combined)
            }
          } else if (!stopRequested.get()) {
            ready(left) + ready(right)
          }
      }
    )

    site(workerPool)(
      go {
        case commit((state, circuit, world)) + step(()) =>
          if (stopRequested.get()) {
            ()
          } else circuit.remainingGates match {
            case Nil =>
              throw new IllegalStateException(
                s"${experiment.alias} emitted a commit molecule with no remaining gates"
              )
            case gate :: remaining =>
              if (remaining.isEmpty && ExperimentConfig.completionJitterMillis > 0) {
                Thread.sleep(
                  ThreadLocalRandom.current().nextInt(ExperimentConfig.completionJitterMillis)
                )
              }
              applyGate(gate, remaining, state, world)
          }
      }
    )
    ()
  }

  private def newWorld(): String = s"World ${worldIds.getAndIncrement()}"

  private def release(next: QVec, remaining: List[Gate], world: String): Unit = {
    if (stopRequested.get()) {
      ()
    } else if (remaining.nonEmpty) {
      commit((next, Circuit(remaining), world)) + step(())
    } else {
      val terminal = terminalState(next)
      var readyToEmit: Option[QVec] = None
      var selected: Option[(QVec, ActivitySnapshot)] = None

      activityLock.synchronized {
        if (!stopRequested.get()) {
          generatedTerminalContributions.increment()
          terminal.foreach { terminalState =>
            readyTerminalMolecules.increment()
            if (experiment.isCorrectTerminalState(terminalState.v)) {
              readyCorrectTerminalMolecules.increment()
            }

            if (terminalState.prop.abs >= threshold) {
              stopRequested.set(true)
              val snapshot = captureActivity()
              selectedActivity.set(snapshot)
              selected = Some(terminalState -> snapshot)
            } else {
              readyToEmit = Some(terminalState)
            }
          }
        }
      }

      selected match {
        case Some((state, _)) =>
          require(sampledState.tryRecord(state), "The selected state recorder was already populated")
        case None =>
          readyToEmit.foreach { state =>
            if (!stopRequested.get()) {
              ready(state)
            }
          }
      }
      // Simon-discarded paths and stopped paths still return their progress token.
      step(())
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

  private def selectSample(state: QVec): Unit = {
    val snapshot = activityLock.synchronized {
      if (stopRequested.get()) {
        None
      } else {
        stopRequested.set(true)
        val selectedSnapshot = captureActivity()
        selectedActivity.set(selectedSnapshot)
        Some(selectedSnapshot)
      }
    }

    snapshot.foreach { _ =>
      require(sampledState.tryRecord(state), "The selected state recorder was already populated")
    }
  }

  private def captureActivity(): ActivitySnapshot =
    ActivitySnapshot(
      generatedTerminals = generatedTerminalContributions.sum(),
      interferenceReactions = interferenceReactions.sum(),
      bActive = readyTerminalMolecules.sum(),
      bActiveCorrect = readyCorrectTerminalMolecules.sum()
    )

  private def applyGate(
      gate: Gate,
      remaining: List[Gate],
      state: QVec,
      world: String
  ): Unit =
    gate match {
      case X(target) =>
        release(
          QVec(state.prop, state.v.updated(target, !state.v(target))),
          remaining,
          world
        )

      case H(target) =>
        val sign = if (state.v(target)) -1d else 1d
        val same = QVec(
          Complex(sign * hScale * state.prop.real, sign * hScale * state.prop.imag),
          state.v
        )
        val flipped = QVec(
          Complex(hScale * state.prop.real, hScale * state.prop.imag),
          state.v.updated(target, !state.v(target))
        )

        release(same, remaining, newWorld())
        release(flipped, remaining, newWorld())

      case CX(control, target) =>
        val nextBits =
          if (state.v(control)) state.v.updated(target, !state.v(target)) else state.v
        release(QVec(state.prop, nextBits), remaining, world)

      case CCX(control1, control2, target) =>
        val nextBits =
          if (state.v(control1) && state.v(control2))
            state.v.updated(target, !state.v(target))
          else state.v
        release(QVec(state.prop, nextBits), remaining, world)

      case CZ(control, target) =>
        val nextAmplitude =
          if (state.v(control) && state.v(target)) state.prop * -1d else state.prop
        release(QVec(nextAmplitude, state.v), remaining, world)

      case PhaseFlipWhenAllOne(qubits) =>
        val nextAmplitude =
          if (qubits.forall(state.v)) state.prop * -1d else state.prop
        release(QVec(nextAmplitude, state.v), remaining, world)

      case Swap(q1, q2) =>
        val swapped = state.v.updated(q1, state.v(q2)).updated(q2, state.v(q1))
        release(QVec(state.prop, swapped), remaining, world)

      case RZ(thetaDenominator, target) =>
        val nextAmplitude =
          RotationMath.applyRz(state.prop, thetaDenominator, state.v(target))
        release(QVec(nextAmplitude, state.v), remaining, world)

      case CRotate(control, thetaDenominator, target) =>
        val nextAmplitude = RotationMath.applyControlledRotate(
          state.prop,
          thetaDenominator,
          state.v(control),
          state.v(target)
        )
        release(QVec(nextAmplitude, state.v), remaining, world)

      case Rotate(thetaDenominator, target) =>
        val nextAmplitude =
          RotationMath.applyPhase(state.prop, thetaDenominator, state.v(target))
        release(QVec(nextAmplitude, state.v), remaining, world)

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
        release(QVec(state.prop, nextBits), remaining, world)

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
    val backendHadamards = experiment.hadamardCount.toLong

    println(
      s"""
         |===================== EXPERIMENT =====================
         |alias:       ${experiment.alias}
         |description: ${experiment.description}
         |backend:     ${experiment.backend.label}
         |qubits:      ${experiment.qubitCount}
         |instances:   $instanceCount
         |copy source: ${if (ExperimentConfig.instanceCountOverride.isDefined) "override" else "threshold/catalog"}
         |threshold:   $threshold
         |Hadamards:   $backendHadamards
         |path scale:  ${formatPathEstimate(backendHadamards)}
         |B_total:     $bTotal ($bTotalPerCopy per copy)
         |B_correct:   $bCorrect ($bCorrectPerCopy per copy)
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

  private def runChamPathSum(): Unit = {
    if (experiment.gates.isEmpty) {
      throw new IllegalArgumentException(
        s"${experiment.alias} has no gates for the CHAM path-sum backend"
      )
    }

    reactionSites
    (0 until instanceCount).foreach { copy =>
      commit((experiment.initialState, Circuit(experiment.gates), s"Copy $copy")) + step(())
    }
  }

  printRunSummary()

  if (ExperimentConfig.preflightOnly) {
    println("Preflight complete; no circuit molecules were emitted.")
    System.exit(0)
  }

  runChamPathSum()

  val selected = sampledState.awaitSample()
  stopRequested.set(true)
  val observedOutcome = experiment.correctOutcomes.observe(selected.v)
  val observedBits = observedOutcome.map(if (_) '1' else '0').mkString
  val shorPostProcessingResult =
    experiment.shorPostProcessing.map(_.process(observedOutcome))
  val isCorrect = shorPostProcessingResult
    .map(_.success)
    .getOrElse(experiment.correctOutcomes.states.contains(observedOutcome))
  val elapsedMillis = (System.nanoTime() - startedAtNanos) / 1000000L
  private val activityAtSelection = Option(selectedActivity.get()).getOrElse(
    throw new IllegalStateException("A sampled state was recorded without an activity snapshot")
  )
  val terminalContributionsAtSelection = activityAtSelection.generatedTerminals
  val interferenceReactionsAtSelection = activityAtSelection.interferenceReactions
  val correctnessMetadata =
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
      "B_total" -> bTotal.toString,
      "B_correct" -> bCorrect.toString,
      "B_active" -> activityAtSelection.bActive.toString,
      "B_activecorrect" -> activityAtSelection.bActiveCorrect.toString,
      "B_totalPerCopy" -> bTotalPerCopy.toString,
      "B_correctPerCopy" -> bCorrectPerCopy.toString,
      "observedOutcome" -> observedBits,
      "isCorrect" -> isCorrect.toString,
      "correctnessDefinition" -> correctnessDescription,
      "elapsedMillis" -> elapsedMillis.toString
    ) ++ correctnessMetadata ++ shorPostProcessingResult.toVector.flatMap(_.metadata)
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
    workerPool.shutdownNow()
  }

  // Chymyst 0.2.0 owns non-daemon scheduler threads even after its explicit
  // pool is stopped. `sbt run` is forked, so ending this experiment JVM is the
  // clean lifecycle boundary and cannot terminate the sbt shell itself.
  if (ExperimentConfig.terminateAfterSample) {
    System.exit(0)
  }
}
