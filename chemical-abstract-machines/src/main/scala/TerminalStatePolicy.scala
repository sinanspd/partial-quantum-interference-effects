package com.sinanspd

import com.sinanspd.qure.circuit.QVec
import spire.implicits._

import scala.util.Random

/**
  * One possible result of a terminal measurement used for post-selection.
  *
  * `probability` is the full-circuit Born probability of measuring `bits`
  * before conditioning. A retained amplitude is therefore multiplied by
  * `1 / sqrt(probability)`.
  */
final case class PostSelectionOutcome(
    bits: Vector[Boolean],
    probability: Double
) {
  require(bits.nonEmpty, "A post-selection outcome cannot be empty")
  require(
    probability > 0d && probability <= 1d && java.lang.Double.isFinite(probability),
    s"Invalid post-selection probability $probability"
  )
}

/**
  * Circuit-generic terminal measurement and post-selection description.
  *
  * This is not specific to a Simon register size. The current Simon circuits
  * populate it with their input register as `retainedQubits`, their oracle
  * output register as `measuredQubits`, and the exact uniform distribution of
  * possible oracle outputs.
  */
final case class PostSelectionSpec(
    retainedQubits: Vector[Int],
    measuredQubits: Vector[Int],
    outcomes: Vector[PostSelectionOutcome],
    description: String
) {
  require(retainedQubits.nonEmpty, "Post-selection must retain at least one qubit")
  require(measuredQubits.nonEmpty, "Post-selection must measure at least one qubit")
  require(outcomes.nonEmpty, "Post-selection must define at least one outcome")
  require(description.nonEmpty, "Post-selection must have a description")
  require(
    outcomes.forall(_.bits.length == measuredQubits.length),
    "Every post-selection outcome must match the measured-register width"
  )
  require(
    outcomes.map(_.bits).distinct.length == outcomes.length,
    "Post-selection outcomes must be unique"
  )
  private val probabilitySum = outcomes.map(_.probability).sum
  require(
    math.abs(probabilitySum - 1d) <= 1e-12d,
    s"Post-selection outcome probabilities sum to $probabilitySum instead of 1"
  )

  def choose(random: Random): PostSelectionOutcome = {
    val draw = random.nextDouble()
    var cumulative = 0d
    outcomes
      .find { outcome =>
        cumulative += outcome.probability
        draw < cumulative
      }
      .getOrElse(outcomes.last)
  }

  def resolve(bits: Vector[Boolean]): PostSelectionOutcome =
    outcomes
      .find(_.bits == bits)
      .getOrElse(
        throw new IllegalArgumentException(
          s"Outcome ${render(bits)} is not valid for post-selection '$description'"
        )
      )

  private def render(bits: Vector[Boolean]): String =
    bits.map(if (_) '1' else '0').mkString
}

final case class ResolvedPostSelection(
    spec: PostSelectionSpec,
    outcome: PostSelectionOutcome
) {
  val amplitudeScale: Double = 1d / math.sqrt(outcome.probability)
}

/**
  * The one terminal policy used by path execution, leaf metrics, and exact
  * reference calculations.
  */
final case class TerminalStatePolicy private (
    retainedQubits: Vector[Int],
    postSelection: Option[ResolvedPostSelection]
) {
  val terminalWidth: Int = retainedQubits.length

  def apply(state: QVec): Option[QVec] =
    projectBits(state.v).map { retained =>
      val scale = postSelection.fold(1d)(_.amplitudeScale)
      QVec(state.prop * scale, retained)
    }

  def projectBits(fullState: Vector[Boolean]): Option[Vector[Boolean]] =
    if (postSelection.forall(matches(fullState, _))) {
      Some(retainedQubits.map(fullState))
    } else {
      None
    }

  private def matches(
      fullState: Vector[Boolean],
      selection: ResolvedPostSelection
  ): Boolean =
    selection.spec.measuredQubits.map(fullState) == selection.outcome.bits
}

object TerminalStatePolicy {
  def select(experiment: ExperimentSpec, random: Random): TerminalStatePolicy =
    resolve(
      experiment,
      experiment.postSelection.map(_.choose(random).bits)
    )

  /** Deterministic selection used only for static, outcome-invariant metrics. */
  def reference(experiment: ExperimentSpec): TerminalStatePolicy =
    resolve(experiment, experiment.postSelection.map(_.outcomes.head.bits))

  def resolve(
      experiment: ExperimentSpec,
      selectedPostSelectionOutcome: Option[Vector[Boolean]]
  ): TerminalStatePolicy = {
    val resolvedPostSelection = experiment.postSelection match {
      case Some(spec) =>
        val bits = selectedPostSelectionOutcome.getOrElse(
          throw new IllegalArgumentException(
            s"${experiment.alias} requires a selected terminal post-selection outcome"
          )
        )
        Some(ResolvedPostSelection(spec, spec.resolve(bits)))
      case None =>
        require(
          selectedPostSelectionOutcome.isEmpty,
          s"${experiment.alias} does not define terminal post-selection"
        )
        None
    }

    val retainedQubits = resolvedPostSelection
      .map(_.spec.retainedQubits)
      .orElse(experiment.resultQubits)
      .getOrElse((0 until experiment.qubitCount).toVector)

    TerminalStatePolicy(retainedQubits, resolvedPostSelection)
  }
}
