package com.sinanspd

import com.sinanspd.qure.circuit.gates._

import scala.collection.mutable

/** Exact pre-interference path counts for one copy of a catalog circuit. */
final case class CircuitLeafMetrics(total: BigInt, correct: BigInt)

object CircuitLeafMetrics {
  private type LeafCounts = Map[Vector[Boolean], BigInt]

  def calculate(experiment: ExperimentSpec): CircuitLeafMetrics =
    experiment.analyticalLeafMetrics.getOrElse(calculateByEnumeration(experiment))

  private def calculateByEnumeration(
      experiment: ExperimentSpec
  ): CircuitLeafMetrics = {
    val terminalCounts = experiment.gates.foldLeft(
      Map(experiment.initialState.v -> BigInt(1))
    ) {
      case (counts, gate) => applyGate(counts, gate)
    }
    val total = terminalCounts.values.foldLeft(BigInt(0))(_ + _)
    val expectedTotal = BigInt(1) << experiment.hadamardCount

    require(
      total == expectedTotal,
      s"${experiment.alias} leaf count $total does not match 2^${experiment.hadamardCount}"
    )

    val selectedOutcomes = experiment.postSelection
      .map(_.outcomes.map(outcome => Some(outcome.bits)))
      .getOrElse(Vector(None))
    val correctBySelectedOutcome = selectedOutcomes.map { selectedOutcome =>
      val terminalPolicy =
        TerminalStatePolicy.resolve(experiment, selectedOutcome)
      terminalCounts.foldLeft(BigInt(0)) {
        case (sum, (state, leaves)) =>
          terminalPolicy.projectBits(state) match {
            case Some(terminalState)
                if experiment.isCorrectTerminalState(terminalState) =>
              sum + leaves
            case _ =>
              sum
          }
      }
    }
    require(
      correctBySelectedOutcome.distinct.length == 1,
      s"${experiment.alias} B_correct depends on the selected post-selection outcome: " +
        correctBySelectedOutcome.distinct.mkString(", ")
    )
    val correct = correctBySelectedOutcome.head

    CircuitLeafMetrics(total = total, correct = correct)
  }

  private def applyGate(counts: LeafCounts, gate: Gate): LeafCounts = {
    val output = mutable.Map.empty[Vector[Boolean], BigInt].withDefaultValue(BigInt(0))

    def release(state: Vector[Boolean], leaves: BigInt): Unit =
      output.update(state, output(state) + leaves)

    counts.foreach {
      case (state, leaves) =>
        gate match {
          case H(target) =>
            release(state, leaves)
            release(state.updated(target, !state(target)), leaves)

          case X(target) =>
            release(state.updated(target, !state(target)), leaves)

          case CX(control, target) =>
            val next =
              if (state(control)) state.updated(target, !state(target)) else state
            release(next, leaves)

          case CCX(control1, control2, target) =>
            val next =
              if (state(control1) && state(control2))
                state.updated(target, !state(target))
              else state
            release(next, leaves)

          case Swap(q1, q2) =>
            release(state.updated(q1, state(q2)).updated(q2, state(q1)), leaves)

          case CZ(_, _) | RZ(_, _) | Rotate(_, _) | CRotate(_, _, _) |
              PhaseFlipWhenAllOne(_) =>
            release(state, leaves)

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
            release(next, leaves)

          case Measure(_) =>
            throw new UnsupportedOperationException(
              "Leaf metrics require measurements to be represented by a terminal policy"
            )

          case unsupported =>
            throw new UnsupportedOperationException(
              s"Leaf metrics do not support gate $unsupported"
            )
        }
    }

    output.toMap
  }
}
