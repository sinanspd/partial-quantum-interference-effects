package com.sinanspd

import com.sinanspd.qure.circuit.QVec
import spire.math.Complex

object TerminalStatePolicyVerification extends App {
  private def approximately(left: Double, right: Double): Boolean =
    math.abs(left - right) < 1e-12

  private val simonN3Spec = ExperimentCatalog.simonN3.postSelection.get
  private val simonN3Outcome = simonN3Spec.outcomes.head
  private val simonN3Policy =
    TerminalStatePolicy.resolve(
      ExperimentCatalog.simonN3,
      Some(simonN3Outcome.bits)
    )
  assert(approximately(simonN3Policy.postSelection.get.amplitudeScale, 2d))

  private val simonN5Spec = ExperimentCatalog.simonN5.postSelection.get
  private val simonN5Outcome = simonN5Spec.outcomes.head
  private val simonN5Policy =
    TerminalStatePolicy.resolve(
      ExperimentCatalog.simonN5,
      Some(simonN5Outcome.bits)
    )
  private val simonN5Width = simonN5Spec.retainedQubits.length
  private val expectedSimonN5Scale =
    math.sqrt(simonN5Spec.outcomes.length.toDouble)
  assert(
    approximately(
      simonN5Policy.postSelection.get.amplitudeScale,
      expectedSimonN5Scale
    )
  )

  private val input = Vector.fill(simonN5Width)(false)
  private val unconditionedLeafAmplitude = math.pow(2d, -simonN5Width.toDouble)
  private val accepted = simonN5Policy(
    QVec(
      Complex(unconditionedLeafAmplitude, 0d),
      input ++ simonN5Outcome.bits
    )
  ).get
  assert(accepted.v == input)
  assert(
    approximately(
      accepted.prop.real,
      unconditionedLeafAmplitude * expectedSimonN5Scale
    )
  )
  assert(approximately(accepted.prop.imag, 0d))

  private val rejectedOutput =
    simonN5Outcome.bits.updated(0, !simonN5Outcome.bits(0))
  assert(
    simonN5Policy(
      QVec(Complex(unconditionedLeafAmplitude, 0d), input ++ rejectedOutput)
    ).isEmpty
  )

  private val grover30Policy =
    TerminalStatePolicy.resolve(
      ExperimentCatalog.grover3Sat30,
      selectedPostSelectionOutcome = None
    )
  private val grover30State = Vector.tabulate(30)(index => index % 2 == 0)
  private val grover30Amplitude = Complex(0.25d, -0.5d)
  private val projectedGrover =
    grover30Policy(QVec(grover30Amplitude, grover30State)).get
  assert(projectedGrover.v == grover30State.take(8))
  assert(projectedGrover.prop == grover30Amplitude)
  assert(grover30Policy.postSelection.isEmpty)

  private val grover4Policy =
    TerminalStatePolicy.resolve(
      ExperimentCatalog.grover4Tagged,
      selectedPostSelectionOutcome = None
    )
  private val grover4State = Vector(false, true, false, true)
  assert(
    grover4Policy(QVec(Complex(1d, 0d), grover4State)).get.v ==
      grover4State
  )

  println("Terminal-state policy verification passed.")
}
