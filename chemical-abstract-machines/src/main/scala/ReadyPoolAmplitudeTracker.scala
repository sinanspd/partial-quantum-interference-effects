package com.sinanspd

import com.sinanspd.qure.circuit.QVec
import spire.implicits._
import spire.math.Complex

import java.util.TreeMap
import scala.collection.mutable
import scala.util.Random

/** Molecule payload used by the endpoint-only CHAM pool. */
final case class ReadyMolecule(
    state: QVec,
    isCorrect: Boolean,
    amplitudeMagnitude: Double
)

object ReadyMolecule {
  def apply(state: QVec, isCorrect: Boolean): ReadyMolecule =
    ReadyMolecule(state, isCorrect, state.prop.abs)
}

final case class IncorrectReadyPoolSnapshot(
    maximumAmplitude: Double,
    moleculeCount: Long
)

final case class ReadyPoolProgressSnapshot(
    moleculeCount: Long,
    distinctEndpointStates: Int,
    compatibleEndpointStates: Int
) {
  val hasCompatiblePair: Boolean = compatibleEndpointStates > 0
}

final case class ReadyPoolSelectionSnapshot(
    incorrectMolecules: IncorrectReadyPoolSnapshot,
    readyMoleculeCount: Long,
    correctReadyMoleculeCount: Long,
    incorrectReadyMoleculeCount: Long,
    distinctReadyEndpointStates: Int,
    distinctCorrectReadyEndpointStates: Int,
    distinctIncorrectReadyEndpointStates: Int,
    selectedStateAggregateAmplitude: Complex[Double],
    maximumIncorrectReadyStateAggregateAmplitude: Double,
    selectedStateAmplitudeRank: Int
) {
  val selectedStateAggregateAmplitudeMagnitude: Double =
    selectedStateAggregateAmplitude.abs
  val selectedVsMaxIncorrectStateAmplitudeMargin: Double =
    selectedStateAggregateAmplitudeMagnitude - maximumIncorrectReadyStateAggregateAmplitude
  val selectedVsMaxIncorrectStateAmplitudeRatio: Option[Double] =
    if (maximumIncorrectReadyStateAggregateAmplitude == 0d) None
    else
      Some(
        selectedStateAggregateAmplitudeMagnitude /
          maximumIncorrectReadyStateAggregateAmplitude
      )
}

final case class ThresholdSelectionDecision(
    selected: ReadyMolecule,
    thresholdTrigger: ReadyMolecule,
    readyPool: ReadyPoolSelectionSnapshot,
    samplingPopulationMoleculeCount: Long,
    samplingPopulationEndpointStateCount: Int,
    amplitudeSquaredMass: Double,
    bornRuleNormalizationFactor: Double,
    normalizedProbabilitySum: Double,
    normalizedStateProbabilities: Vector[(Vector[Boolean], Double)],
    selectedStateNormalizedBornProbability: Double,
    bornRuleRandomDraw: Option[Double]
) {
  val thresholdTriggerWasSelected: Boolean =
    selected.state.v == thresholdTrigger.state.v

  val renderedNormalizedStateProbabilities: String =
    normalizedStateProbabilities
      .map {
        case (state, probability) =>
          s"${state.map(if (_) '1' else '0').mkString}:${java.lang.Double.toString(probability)}"
      }
      .mkString("|")
}

object ReadyPoolAmplitudeTracker {
  val snapshotSemantics: String =
    "instrumented ready shadow multiset excluding the threshold-triggering candidate; may conservatively include inputs dispatched by Chymyst whose reaction body has not started"

  val bornRuleProbabilityDefinition: String =
    "p_s=|A_s|^2/sum_j(|A_j|^2) over complex state aggregates in the truncated sampling population"
}

/**
  * A compact shadow multiset for incorrect ready-molecule amplitudes.
  *
  * It stores one count per distinct amplitude rather than copying QVec values.
  * Callers serialize emission, consumption, and selection with their activity
  * lock so a threshold snapshot has a well-defined application-level boundary.
  */
final class ReadyPoolAmplitudeTracker {
  private final case class StateAggregate(
      amplitude: Complex[Double],
      moleculeCount: Long,
      isCorrect: Boolean
  )

  private val incorrectAmplitudeCounts = new TreeMap[java.lang.Double, java.lang.Long]()
  private val stateAggregates = mutable.Map.empty[Vector[Boolean], StateAggregate]
  private var moleculeCount = 0L
  private var correctMoleculeCount = 0L
  private var incorrectMoleculeCount = 0L

  def emitted(molecule: ReadyMolecule): Unit = synchronized {
    stateAggregates.get(molecule.state.v) match {
      case Some(previous) =>
        require(
          previous.isCorrect == molecule.isCorrect,
          s"Endpoint ${molecule.state.v} was assigned inconsistent correctness"
        )
        stateAggregates.update(
          molecule.state.v,
          previous.copy(
            amplitude = previous.amplitude + molecule.state.prop,
            moleculeCount = previous.moleculeCount + 1L
          )
        )
      case None =>
        stateAggregates.update(
          molecule.state.v,
          StateAggregate(molecule.state.prop, 1L, molecule.isCorrect)
        )
    }
    moleculeCount += 1L
    if (molecule.isCorrect) {
      correctMoleculeCount += 1L
    } else {
      val key = java.lang.Double.valueOf(molecule.amplitudeMagnitude)
      val previous = Option(incorrectAmplitudeCounts.get(key)).fold(0L)(_.longValue())
      incorrectAmplitudeCounts.put(key, java.lang.Long.valueOf(previous + 1L))
      incorrectMoleculeCount += 1L
    }
  }

  def consumed(molecule: ReadyMolecule): Unit = synchronized {
    val previousState = stateAggregates.getOrElse(
      molecule.state.v,
      throw new IllegalArgumentException(s"Consumed an untracked ready state ${molecule.state.v}")
    )
    require(
      previousState.isCorrect == molecule.isCorrect && previousState.moleculeCount > 0L,
      s"Consumed an inconsistent ready molecule $molecule"
    )
    if (previousState.moleculeCount == 1L) {
      stateAggregates.remove(molecule.state.v)
    } else {
      stateAggregates.update(
        molecule.state.v,
        previousState.copy(
          amplitude = previousState.amplitude - molecule.state.prop,
          moleculeCount = previousState.moleculeCount - 1L
        )
      )
    }
    moleculeCount -= 1L
    if (molecule.isCorrect) {
      correctMoleculeCount -= 1L
    } else {
      val key = java.lang.Double.valueOf(molecule.amplitudeMagnitude)
      val previous = Option(incorrectAmplitudeCounts.get(key)).fold(0L)(_.longValue())
      require(
        previous > 0L,
        s"Consumed an untracked incorrect ready amplitude ${molecule.amplitudeMagnitude}"
      )
      if (previous == 1L) incorrectAmplitudeCounts.remove(key)
      else incorrectAmplitudeCounts.put(key, java.lang.Long.valueOf(previous - 1L))
      incorrectMoleculeCount -= 1L
    }
  }

  def snapshot(): IncorrectReadyPoolSnapshot = synchronized {
    val maximum =
      if (incorrectAmplitudeCounts.isEmpty) 0d
      else incorrectAmplitudeCounts.lastKey().doubleValue()
    IncorrectReadyPoolSnapshot(maximum, incorrectMoleculeCount)
  }

  /**
    * Reports whether at least one endpoint has two molecules that can
    * interfere. Callers use this after path generation completes to
    * distinguish pending CHAM work from a quiescent, unattained threshold.
    */
  def progressSnapshot(): ReadyPoolProgressSnapshot = synchronized {
    ReadyPoolProgressSnapshot(
      moleculeCount = moleculeCount,
      distinctEndpointStates = stateAggregates.size,
      compatibleEndpointStates =
        stateAggregates.valuesIterator.count(_.moleculeCount >= 2L)
    )
  }

  /**
    * Adds the threshold-triggering candidate to the tracked state aggregates
    * and applies the configured outcome-selection rule. Ready-pool population
    * fields exclude that candidate; sampling-population fields include it.
    */
  def select(
      thresholdTrigger: ReadyMolecule,
      mode: OutcomeSelectionMode,
      random: Random
  ): ThresholdSelectionDecision = synchronized {
    val augmented = mutable.Map.empty[Vector[Boolean], StateAggregate] ++ stateAggregates
    val triggerAggregate = augmented.get(thresholdTrigger.state.v) match {
      case Some(previous) =>
        require(
          previous.isCorrect == thresholdTrigger.isCorrect,
          s"Threshold endpoint ${thresholdTrigger.state.v} has inconsistent correctness"
        )
        previous.copy(
          amplitude = previous.amplitude + thresholdTrigger.state.prop,
          moleculeCount = previous.moleculeCount + 1L
        )
      case None =>
        StateAggregate(thresholdTrigger.state.prop, 1L, thresholdTrigger.isCorrect)
    }
    augmented.update(thresholdTrigger.state.v, triggerAggregate)

    val orderedPopulation = augmented.toVector.sortBy {
      case (state, _) => state.map(if (_) '1' else '0').mkString
    }
    val amplitudeSquaredMass = orderedPopulation.iterator.map {
      case (_, aggregate) =>
        aggregate.amplitude.real * aggregate.amplitude.real +
          aggregate.amplitude.imag * aggregate.amplitude.imag
    }.sum
    require(
      amplitudeSquaredMass > 0d && java.lang.Double.isFinite(amplitudeSquaredMass),
      s"Cannot normalize a truncated sampling population with squared-amplitude mass $amplitudeSquaredMass"
    )
    val normalizationFactor = 1d / amplitudeSquaredMass
    val normalizedWeights = orderedPopulation.map {
      case (state, aggregate) =>
        val squaredMagnitude =
          aggregate.amplitude.real * aggregate.amplitude.real +
            aggregate.amplitude.imag * aggregate.amplitude.imag
        (state, aggregate, squaredMagnitude * normalizationFactor)
    }
    val normalizedProbabilitySum = normalizedWeights.map(_._3).sum
    require(
      math.abs(normalizedProbabilitySum - 1d) <= 1e-9d,
      s"Normalized truncated Born probabilities sum to $normalizedProbabilitySum instead of 1"
    )

    val (selectedState, selectedAggregate, selectedProbability, randomDraw) =
      mode match {
        case FirstThresholdCrossing =>
          val probability = normalizedWeights
            .find(_._1 == thresholdTrigger.state.v)
            .map(_._3)
            .getOrElse(
              throw new IllegalStateException("Threshold trigger disappeared from population")
            )
          (
            thresholdTrigger.state.v,
            triggerAggregate,
            probability,
            Option.empty[Double]
          )

        case BornRuleSampling =>
          val draw = random.nextDouble()
          val target = draw * amplitudeSquaredMass
          var cumulative = 0d
          var chosen = Option.empty[(Vector[Boolean], StateAggregate, Double)]
          val iterator = normalizedWeights.iterator
          while (iterator.hasNext && chosen.isEmpty) {
            val candidate @ (_, aggregate, _) = iterator.next()
            val squaredMagnitude =
              aggregate.amplitude.real * aggregate.amplitude.real +
                aggregate.amplitude.imag * aggregate.amplitude.imag
            cumulative += squaredMagnitude
            if (target < cumulative) chosen = Some(candidate)
          }
          val selected = chosen.getOrElse(
            normalizedWeights.reverseIterator
              .find(_._3 > 0d)
              .getOrElse(
                throw new IllegalStateException(
                  "Normalized Born population contained no positive-probability state"
                )
              )
          )
          (selected._1, selected._2, selected._3, Some(draw))
      }

    val selectedMolecule = mode match {
      case FirstThresholdCrossing => thresholdTrigger
      case BornRuleSampling =>
        ReadyMolecule(
          QVec(selectedAggregate.amplitude, selectedState),
          selectedAggregate.isCorrect
        )
    }

    val selectedMagnitude = selectedAggregate.amplitude.abs
    val rank =
      1 + augmented.valuesIterator.count(_.amplitude.abs > selectedMagnitude + 1e-12d)
    val maximumIncorrectState =
      augmented.valuesIterator
        .filter(!_.isCorrect)
        .map(_.amplitude.abs)
        .foldLeft(0d)(math.max)
    val incorrectMaximumMolecule =
      if (incorrectAmplitudeCounts.isEmpty) 0d
      else incorrectAmplitudeCounts.lastKey().doubleValue()

    val poolSnapshot = ReadyPoolSelectionSnapshot(
      incorrectMolecules =
        IncorrectReadyPoolSnapshot(incorrectMaximumMolecule, incorrectMoleculeCount),
      readyMoleculeCount = moleculeCount,
      correctReadyMoleculeCount = correctMoleculeCount,
      incorrectReadyMoleculeCount = incorrectMoleculeCount,
      distinctReadyEndpointStates = stateAggregates.size,
      distinctCorrectReadyEndpointStates =
        stateAggregates.valuesIterator.count(_.isCorrect),
      distinctIncorrectReadyEndpointStates =
        stateAggregates.valuesIterator.count(!_.isCorrect),
      selectedStateAggregateAmplitude = selectedAggregate.amplitude,
      maximumIncorrectReadyStateAggregateAmplitude = maximumIncorrectState,
      selectedStateAmplitudeRank = rank
    )

    ThresholdSelectionDecision(
      selected = selectedMolecule,
      thresholdTrigger = thresholdTrigger,
      readyPool = poolSnapshot,
      samplingPopulationMoleculeCount = moleculeCount + 1L,
      samplingPopulationEndpointStateCount = augmented.size,
      amplitudeSquaredMass = amplitudeSquaredMass,
      bornRuleNormalizationFactor = normalizationFactor,
      normalizedProbabilitySum = normalizedProbabilitySum,
      normalizedStateProbabilities =
        normalizedWeights.map { case (state, _, probability) => state -> probability },
      selectedStateNormalizedBornProbability = selectedProbability,
      bornRuleRandomDraw = randomDraw
    )
  }
}
