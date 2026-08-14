package com.sinanspd

import scala.util.Random

/** Regression coverage for the repeated-trial seed correlation bug. */
object ExperimentRandomVerification extends App {
  private val baseSeed = 260601922L
  private val javaRandomMultiplier = 0x5DEECE66DL

  private val oldDraws = (0 until 50).map { offset =>
    new Random((baseSeed + offset.toLong) ^ javaRandomMultiplier).nextDouble()
  }.toVector
  private val oldRange = oldDraws.max - oldDraws.min
  assert(
    oldRange < 0.01d,
    s"The regression fixture no longer reproduces the old clustered draws: $oldRange"
  )

  private val trialSeeds = (1 to 50).map { trialIndex =>
    ExperimentRandom.trialSeed(baseSeed, trialIndex)
  }.toVector
  private val bornSeeds = trialSeeds.map(ExperimentRandom.bornRuleSeed)
  private val postSelectionSeeds = trialSeeds.map(ExperimentRandom.postSelectionSeed)
  private val bornDraws = bornSeeds.map(seed => new Random(seed).nextDouble())
  private val repeatedBornDraws = trialSeeds
    .map(ExperimentRandom.bornRuleSeed)
    .map(seed => new Random(seed).nextDouble())

  assert(trialSeeds.distinct.length == trialSeeds.length)
  assert(bornSeeds.distinct.length == bornSeeds.length)
  assert(postSelectionSeeds.distinct.length == postSelectionSeeds.length)
  assert(
    bornSeeds.zip(postSelectionSeeds).forall { case (born, post) => born != post },
    "Born-rule and post-selection streams reused a seed"
  )
  assert(bornDraws == repeatedBornDraws, "Mixed random streams are not reproducible")
  assert(
    bornDraws.max - bornDraws.min > 0.75d,
    s"Born-rule draws are still clustered: ${bornDraws.min} to ${bornDraws.max}"
  )
  assert(
    bornDraws.map(draw => math.min(9, (draw * 10d).toInt)).distinct.length >= 8,
    s"Born-rule draws do not cover enough deciles: $bornDraws"
  )
  private val groverCorrectIntervalHits =
    bornDraws.count(draw => draw >= 0.398393547075d && draw < 0.623508777876d)
  assert(
    groverCorrectIntervalHits >= 5 && groverCorrectIntervalHits <= 20,
    s"Unexpected coverage of the recorded Grover success interval: $groverCorrectIntervalHits"
  )

  println(
    s"Experiment random verification passed; old draw range=$oldRange, " +
      s"mixed draw range=${bornDraws.max - bornDraws.min}, " +
      s"Grover interval hits=$groverCorrectIntervalHits/50."
  )
}
