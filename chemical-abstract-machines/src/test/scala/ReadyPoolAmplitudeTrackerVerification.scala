package com.sinanspd

import com.sinanspd.qure.circuit.QVec
import spire.math.Complex
import scala.util.Random

object ReadyPoolAmplitudeTrackerVerification extends App {
  private def molecule(amplitude: Double, correct: Boolean): ReadyMolecule =
    ReadyMolecule(
      QVec(Complex(amplitude, 0d), Vector(correct)),
      isCorrect = correct
    )

  val tracker = new ReadyPoolAmplitudeTracker
  val low = molecule(0.125, correct = false)
  val high1 = molecule(0.5, correct = false)
  val high2 = molecule(-0.5, correct = false)
  val correct = molecule(0.9, correct = true)

  tracker.emitted(low)
  tracker.emitted(high1)
  tracker.emitted(high2)
  tracker.emitted(correct)
  assert(tracker.snapshot() == IncorrectReadyPoolSnapshot(0.5, 3L))
  assert(
    tracker.progressSnapshot() ==
      ReadyPoolProgressSnapshot(
        moleculeCount = 4L,
        distinctEndpointStates = 2,
        compatibleEndpointStates = 1
      )
  )

  val firstDecision = tracker.select(
    ReadyMolecule(QVec(Complex(0.1d, 0d), Vector(true)), isCorrect = true),
    FirstThresholdCrossing,
    new Random(1L)
  )
  val selection = firstDecision.readyPool
  assert(selection.readyMoleculeCount == 4L)
  assert(selection.correctReadyMoleculeCount == 1L)
  assert(selection.incorrectReadyMoleculeCount == 3L)
  assert(selection.distinctReadyEndpointStates == 2)
  assert(selection.distinctCorrectReadyEndpointStates == 1)
  assert(selection.distinctIncorrectReadyEndpointStates == 1)
  assert(math.abs(selection.selectedStateAggregateAmplitudeMagnitude - 1d) < 1e-12)
  assert(
    math.abs(selection.maximumIncorrectReadyStateAggregateAmplitude - 0.125d) < 1e-12
  )
  assert(math.abs(selection.selectedVsMaxIncorrectStateAmplitudeMargin - 0.875d) < 1e-12)
  assert(selection.selectedVsMaxIncorrectStateAmplitudeRatio.contains(8d))
  assert(selection.selectedStateAmplitudeRank == 1)
  assert(firstDecision.thresholdTriggerWasSelected)
  assert(firstDecision.bornRuleRandomDraw.isEmpty)
  assert(math.abs(firstDecision.amplitudeSquaredMass - 1.015625d) < 1e-12)
  assert(math.abs(firstDecision.normalizedProbabilitySum - 1d) < 1e-12)
  assert(
    math.abs(
      firstDecision.selectedStateNormalizedBornProbability -
        1d / 1.015625d
    ) < 1e-12
  )

  val bornTracker = new ReadyPoolAmplitudeTracker
  bornTracker.emitted(
    ReadyMolecule(QVec(Complex(1d, 0d), Vector(true)), isCorrect = true)
  )
  val fixedDraw = new Random(2L) {
    override def nextDouble(): Double = 0.75d
  }
  val bornDecision = bornTracker.select(
    ReadyMolecule(QVec(Complex(1d, 0d), Vector(false)), isCorrect = false),
    BornRuleSampling,
    fixedDraw
  )
  assert(bornDecision.selected.state.v == Vector(true))
  assert(bornDecision.selected.isCorrect)
  assert(!bornDecision.thresholdTriggerWasSelected)
  assert(bornDecision.bornRuleRandomDraw.contains(0.75d))
  assert(math.abs(bornDecision.amplitudeSquaredMass - 2d) < 1e-12)
  assert(math.abs(bornDecision.bornRuleNormalizationFactor - 0.5d) < 1e-12)
  assert(math.abs(bornDecision.normalizedProbabilitySum - 1d) < 1e-12)
  assert(math.abs(bornDecision.selectedStateNormalizedBornProbability - 0.5d) < 1e-12)
  assert(bornDecision.renderedNormalizedStateProbabilities == "0:0.5|1:0.5")
  assert(bornDecision.samplingPopulationMoleculeCount == 2L)
  assert(bornDecision.samplingPopulationEndpointStateCount == 2)

  tracker.consumed(high1)
  assert(tracker.snapshot() == IncorrectReadyPoolSnapshot(0.5, 2L))
  assert(tracker.progressSnapshot().hasCompatiblePair)

  tracker.consumed(high2)
  assert(tracker.snapshot() == IncorrectReadyPoolSnapshot(0.125, 1L))
  assert(!tracker.progressSnapshot().hasCompatiblePair)

  tracker.consumed(low)
  assert(tracker.snapshot() == IncorrectReadyPoolSnapshot(0d, 0L))

  println("Ready-pool amplitude tracker verification passed.")
}
