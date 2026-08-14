package com.sinanspd

import com.sinanspd.qure.circuit.gates._
import spire.math.Complex

/**
  * Dependency-free verification executable:
  *
  *   sbt "Test / runMain com.sinanspd.RotationMathVerification"
  */
object RotationMathVerification extends App {
  private val tolerance = 1e-10

  private def magnitudeSquared(value: Complex[Double]): Double =
    value.real * value.real + value.imag * value.imag

  private def close(actual: Double, expected: Double): Boolean =
    math.abs(actual - expected) <= tolerance

  private def close(
      actual: Complex[Double],
      expected: Complex[Double]
  ): Boolean =
    close(actual.real, expected.real) && close(actual.imag, expected.imag)

  private val one = Complex(1d, 0d)
  private val denominators = Vector(-16, -8, -4, -2, -1, 1, 2, 4, 8, 16)

  denominators.foreach { denominator =>
    Vector(false, true).foreach { bit =>
      assert(
        close(
          magnitudeSquared(RotationMath.phaseCoefficient(denominator, bit)),
          1d
        ),
        s"Rotate($denominator) is not unitary for bit $bit"
      )
      assert(
        close(
          magnitudeSquared(RotationMath.rzCoefficient(denominator, bit)),
          1d
        ),
        s"RZ($denominator) is not unitary for bit $bit"
      )
    }
  }

  Vector(false, true).foreach { bit =>
    try {
      RotationMath.phaseCoefficient(0, bit)
      assert(assertion = false, s"Rotate(0) was accepted for bit $bit")
    } catch {
      case _: IllegalArgumentException => ()
    }
    try {
      RotationMath.rzCoefficient(0, bit)
      assert(assertion = false, s"RZ(0) was accepted for bit $bit")
    } catch {
      case _: IllegalArgumentException => ()
    }
    try {
      RotationMath.controlledRotateCoefficient(0, controlIsOne = bit, targetIsOne = bit)
      assert(assertion = false, s"CRotate(0) was accepted for bit $bit")
    } catch {
      case _: IllegalArgumentException => ()
    }
  }

  val t = RotationMath.phaseCoefficient(4, targetIsOne = true)
  val tDagger = RotationMath.phaseCoefficient(-4, targetIsOne = true)
  assert(close(RotationMath.multiply(t, tDagger), one), "T and T-dagger are not inverses")

  private type StateVector = Vector[Complex[Double]]

  private def add(left: Complex[Double], right: Complex[Double]): Complex[Double] =
    Complex(left.real + right.real, left.imag + right.imag)

  private def zeroState(qubits: Int): StateVector =
    Vector(one) ++ Vector.fill((1 << qubits) - 1)(Complex(0d, 0d))

  private def bit(index: Int, qubit: Int, qubits: Int): Boolean =
    (index & (1 << (qubits - qubit - 1))) != 0

  private def flip(index: Int, qubit: Int, qubits: Int): Int =
    index ^ (1 << (qubits - qubit - 1))

  private def applyGate(
      state: StateVector,
      gate: Gate,
      qubits: Int
  ): StateVector = {
    val output = Array.fill(state.length)(Complex(0d, 0d))

    state.indices.foreach { index =>
      gate match {
        case H(target) =>
          val sameSign = if (bit(index, target, qubits)) -1d else 1d
          val same = Complex(
            state(index).real * sameSign / math.sqrt(2d),
            state(index).imag * sameSign / math.sqrt(2d)
          )
          val changed = Complex(
            state(index).real / math.sqrt(2d),
            state(index).imag / math.sqrt(2d)
          )
          output(index) = add(output(index), same)
          val changedIndex = flip(index, target, qubits)
          output(changedIndex) = add(output(changedIndex), changed)

        case X(target) =>
          val changedIndex = flip(index, target, qubits)
          output(changedIndex) = add(output(changedIndex), state(index))

        case CX(control, target) =>
          val changedIndex =
            if (bit(index, control, qubits)) flip(index, target, qubits) else index
          output(changedIndex) = add(output(changedIndex), state(index))

        case Rotate(denominator, target) =>
          val changed = RotationMath.applyPhase(
            state(index),
            denominator,
            bit(index, target, qubits)
          )
          output(index) = add(output(index), changed)

        case CRotate(control, denominator, target) =>
          val changed = RotationMath.applyControlledRotate(
            state(index),
            denominator,
            bit(index, control, qubits),
            bit(index, target, qubits)
          )
          output(index) = add(output(index), changed)

        case unsupported =>
          throw new IllegalArgumentException(s"Verification does not support $unsupported")
      }
    }

    output.toVector
  }

  private val satResult =
    ExperimentCatalog.grover3Sat3QApprox.gates.foldLeft(zeroState(3)) {
      case (state, gate) => applyGate(state, gate, qubits = 3)
    }
  private val satProbabilities = satResult.map(magnitudeSquared)
  private val markedIndex = Integer.parseInt("011", 2)

  assert(
    close(satProbabilities.sum, 1d),
    s"grover-3sat-3q-approx lost normalization: ${satProbabilities.sum}"
  )
  assert(
    satProbabilities(markedIndex) == satProbabilities.max,
    s"grover-3sat-3q-approx does not make 011 the most likely state: $satProbabilities"
  )
  assert(
    close(satProbabilities(markedIndex), 0.5609296083845561),
    s"unexpected grover-3sat-3q-approx marked probability: ${satProbabilities(markedIndex)}"
  )

  // This is the historical seven-controlled-rotation decomposition used by
  // the four-qubit Grover circuit. It must be a phase flip only on |1111>.
  private val controlledPhaseDecomposition = List[Gate](
    CRotate(0, 2, 3),
    CX(0, 1),
    CRotate(1, -2, 3),
    CX(0, 1),
    CRotate(1, 2, 3),
    CX(1, 2),
    CRotate(2, -2, 3),
    CX(0, 2),
    CRotate(2, 2, 3),
    CX(1, 2),
    CRotate(2, -2, 3),
    CX(0, 2),
    CRotate(2, 2, 3)
  )

  (0 until 16).foreach { basisIndex =>
    val initial = Vector.tabulate(16) { index =>
      if (index == basisIndex) one else Complex(0d, 0d)
    }
    val result = controlledPhaseDecomposition.foldLeft(initial) {
      case (state, gate) => applyGate(state, gate, qubits = 4)
    }
    val expected = if (basisIndex == 15) Complex(-1d, 0d) else one

    assert(
      close(result(basisIndex), expected),
      s"controlled-phase decomposition is wrong for ${basisIndex.toBinaryString}: ${result(basisIndex)}"
    )
    assert(
      close(result.map(magnitudeSquared).sum, 1d),
      s"controlled-phase decomposition lost normalization for $basisIndex"
    )
  }

  println("Rotation verification passed.")
}
