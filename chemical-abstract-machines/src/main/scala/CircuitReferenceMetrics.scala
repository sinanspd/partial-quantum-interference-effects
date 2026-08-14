package com.sinanspd

import com.sinanspd.qure.circuit.QVec
import com.sinanspd.qure.circuit.gates._
import spire.implicits._
import spire.math.Complex

import scala.collection.mutable

/**
  * Deterministic full-simulation references for one experiment configuration.
  *
  * `fullInterferenceReactions` is the canonical number of compatible binary
  * additions needed to reduce every admitted endpoint's path contributions:
  *
  *   sum_s (B_s - 1)
  *
  * It is independent of CHAM scheduling and of whether an intermediate
  * addition happens to cancel to zero.
  */
final case class CircuitReferenceMetrics(
    fullInterferenceContributions: BigInt,
    fullInterferenceEndpointStates: Int,
    fullInterferenceReactions: BigInt,
    idealOutputProbabilities: Map[Vector[Boolean], Double]
) {
  require(fullInterferenceContributions > 0, "A reference run must admit a contribution")
  require(fullInterferenceEndpointStates > 0, "A reference run must contain an endpoint")
  require(fullInterferenceReactions >= 0, "Full interference work cannot be negative")
  require(idealOutputProbabilities.nonEmpty, "The ideal output distribution cannot be empty")

  def idealProbability(outcome: Vector[Boolean]): Double =
    idealOutputProbabilities.getOrElse(outcome, 0d)

  def renderedIdealDistribution: String =
    idealOutputProbabilities.toVector
      .sortBy { case (state, _) => renderBits(state) }
      .map {
        case (state, probability) =>
          s"${renderBits(state)}:${java.lang.Double.toString(probability)}"
      }
      .mkString("|")

  private def renderBits(state: Vector[Boolean]): String =
    state.map(if (_) '1' else '0').mkString
}

/**
  * Closed-form reference data for circuits whose exact state space is too
  * large to enumerate during catalog startup.
  */
final case class AnalyticalCircuitReference(
    fullInterferenceContributionsPerCopy: BigInt,
    fullInterferenceEndpointStates: Int,
    idealOutputProbabilities: Map[Vector[Boolean], Double]
) {
  require(
    fullInterferenceContributionsPerCopy > 0,
    "Analytical reference contributions must be positive"
  )
  require(
    fullInterferenceEndpointStates > 0,
    "Analytical reference endpoint count must be positive"
  )
  require(
    idealOutputProbabilities.nonEmpty,
    "Analytical reference distribution cannot be empty"
  )
  require(
    fullInterferenceContributionsPerCopy >= BigInt(fullInterferenceEndpointStates),
    "Analytical reference contributions cannot be fewer than its endpoint states"
  )
  require(
    idealOutputProbabilities.values.forall(probability =>
      probability >= 0d && java.lang.Double.isFinite(probability)
    ),
    "Analytical reference probabilities must be finite and non-negative"
  )
  require(
    math.abs(idealOutputProbabilities.values.sum - 1d) <= 1e-9d,
    "Analytical reference probabilities must sum to one"
  )

  def forInstances(instanceCount: Int): CircuitReferenceMetrics = {
    require(instanceCount > 0, "Reference metrics require at least one circuit instance")
    val contributions =
      fullInterferenceContributionsPerCopy * BigInt(instanceCount)
    CircuitReferenceMetrics(
      fullInterferenceContributions = contributions,
      fullInterferenceEndpointStates = fullInterferenceEndpointStates,
      fullInterferenceReactions =
        contributions - BigInt(fullInterferenceEndpointStates),
      idealOutputProbabilities = idealOutputProbabilities
    )
  }
}

object CircuitReferenceMetrics {
  val fullInterferenceDefinition: String =
    "canonical compatible binary additions sum_s(B_s-1) over terminal-policy-admitted endpoint contributions"

  val idealDistributionDefinition: String =
    "normalized exact full-state Born probabilities after terminal post-selection and observed-register marginalization"

  private final case class StateCell(pathCount: BigInt, amplitude: Complex[Double])

  def calculate(
      experiment: ExperimentSpec,
      selectedPostSelectionOutcome: Option[Vector[Boolean]],
      instanceCount: Int
  ): CircuitReferenceMetrics = {
    require(instanceCount > 0, "Reference metrics require at least one circuit instance")
    val terminalPolicy = TerminalStatePolicy.resolve(
      experiment,
      selectedPostSelectionOutcome
    )
    experiment.analyticalReferenceMetrics match {
      case Some(reference) =>
        return reference.forInstances(instanceCount)
      case None => ()
    }

    val fullStates = experiment.gates.foldLeft(
      Map(experiment.initialState.v -> StateCell(BigInt(1), experiment.initialState.prop))
    ) {
      case (states, gate) => applyGate(states, gate)
    }

    val endpointCounts = mutable.Map.empty[Vector[Boolean], BigInt].withDefaultValue(BigInt(0))
    val outcomeWeights = mutable.Map.empty[Vector[Boolean], Double].withDefaultValue(0d)

    fullStates.foreach {
      case (fullState, cell) =>
        terminalPolicy(QVec(cell.amplitude, fullState)).foreach { terminalState =>
          val endpoint = terminalState.v
          endpointCounts.update(endpoint, endpointCounts(endpoint) + cell.pathCount)
          val observed = experiment.correctOutcomes.observe(endpoint)
          val probabilityWeight =
            terminalState.prop.real * terminalState.prop.real +
              terminalState.prop.imag * terminalState.prop.imag
          outcomeWeights.update(observed, outcomeWeights(observed) + probabilityWeight)
        }
    }

    require(
      endpointCounts.nonEmpty,
      s"${experiment.alias} terminal policy rejected every exact terminal state"
    )
    val normalization = outcomeWeights.values.sum
    require(
      normalization > 0d && java.lang.Double.isFinite(normalization),
      s"${experiment.alias} has an invalid ideal-distribution normalization $normalization"
    )
    require(
      math.abs(normalization - 1d) <= 1e-9d,
      s"${experiment.alias} terminal policy produced total Born mass $normalization instead of 1"
    )

    val copies = BigInt(instanceCount)
    val contributionCounts = endpointCounts.values.map(_ * copies).toVector
    val contributions = contributionCounts.foldLeft(BigInt(0))(_ + _)
    val reactions =
      contributionCounts.foldLeft(BigInt(0))((sum, count) => sum + count - 1)
    val probabilities =
      outcomeWeights.iterator.map {
        case (outcome, weight) => outcome -> (weight / normalization)
      }.toMap

    CircuitReferenceMetrics(
      fullInterferenceContributions = contributions,
      fullInterferenceEndpointStates = endpointCounts.size,
      fullInterferenceReactions = reactions,
      idealOutputProbabilities = probabilities
    )
  }

  private def applyGate(
      states: Map[Vector[Boolean], StateCell],
      gate: Gate
  ): Map[Vector[Boolean], StateCell] = {
    val output = mutable.Map.empty[Vector[Boolean], StateCell]
    val hScale = 1d / math.sqrt(2d)

    def release(
        state: Vector[Boolean],
        pathCount: BigInt,
        amplitude: Complex[Double]
    ): Unit =
      output.get(state) match {
        case Some(previous) =>
          output.update(
            state,
            StateCell(previous.pathCount + pathCount, previous.amplitude + amplitude)
          )
        case None =>
          output.update(state, StateCell(pathCount, amplitude))
      }

    states.foreach {
      case (state, cell) =>
        gate match {
          case H(target) =>
            val sign = if (state(target)) -1d else 1d
            release(state, cell.pathCount, cell.amplitude * (sign * hScale))
            release(
              state.updated(target, !state(target)),
              cell.pathCount,
              cell.amplitude * hScale
            )

          case X(target) =>
            release(state.updated(target, !state(target)), cell.pathCount, cell.amplitude)

          case CX(control, target) =>
            val next =
              if (state(control)) state.updated(target, !state(target)) else state
            release(next, cell.pathCount, cell.amplitude)

          case CCX(control1, control2, target) =>
            val next =
              if (state(control1) && state(control2))
                state.updated(target, !state(target))
              else state
            release(next, cell.pathCount, cell.amplitude)

          case CZ(control, target) =>
            val nextAmplitude =
              if (state(control) && state(target)) cell.amplitude * -1d
              else cell.amplitude
            release(state, cell.pathCount, nextAmplitude)

          case PhaseFlipWhenAllOne(qubits) =>
            val nextAmplitude =
              if (qubits.forall(state)) cell.amplitude * -1d else cell.amplitude
            release(state, cell.pathCount, nextAmplitude)

          case Swap(q1, q2) =>
            val next = state.updated(q1, state(q2)).updated(q2, state(q1))
            release(next, cell.pathCount, cell.amplitude)

          case RZ(thetaDenominator, target) =>
            release(
              state,
              cell.pathCount,
              RotationMath.applyRz(cell.amplitude, thetaDenominator, state(target))
            )

          case CRotate(control, thetaDenominator, target) =>
            release(
              state,
              cell.pathCount,
              RotationMath.applyControlledRotate(
                cell.amplitude,
                thetaDenominator,
                state(control),
                state(target)
              )
            )

          case Rotate(thetaDenominator, target) =>
            release(
              state,
              cell.pathCount,
              RotationMath.applyPhase(cell.amplitude, thetaDenominator, state(target))
            )

          case ModularExponentiation(base, modulus, inputQubits, outputQubits) =>
            val exponent = inputQubits.foldLeft(0) { (value, qubit) =>
              (value << 1) | (if (state(qubit)) 1 else 0)
            }
            val modularResult =
              BigInt(base).modPow(BigInt(exponent), BigInt(modulus)).toInt
            val next = outputQubits.zipWithIndex.foldLeft(state) {
              case (current, (qubit, index)) =>
                val shift = outputQubits.length - index - 1
                current.updated(qubit, ((modularResult >> shift) & 1) == 1)
            }
            release(next, cell.pathCount, cell.amplitude)

          case Measure(_) =>
            throw new UnsupportedOperationException(
              "Reference metrics require measurements to be represented by a terminal policy"
            )

          case unsupported =>
            throw new UnsupportedOperationException(
              s"Reference metrics do not support gate $unsupported"
            )
        }
    }

    output.toMap
  }
}
