package com.sinanspd

/** Deterministic, well-mixed seeds for independent experiment random streams. */
private[sinanspd] object ExperimentRandom {
  // Signed forms of the SplitMix64/Stafford Mix13 constants.
  private val GoldenGamma = -7046029254386353131L
  private val MixMultiplier1 = -4658895280553007687L
  private val MixMultiplier2 = -7723592293110705685L

  // ASCII-derived domain separators: BORNRULE, POSTSELE, BOOTSTRP.
  private val BornRuleDomain = 4778128226005634117L
  private val PostSelectionDomain = 5786935667833785413L
  private val BootstrapDomain = 4778124953257267792L

  private def mix64(input: Long): Long = {
    var value = input
    value = (value ^ (value >>> 30)) * MixMultiplier1
    value = (value ^ (value >>> 27)) * MixMultiplier2
    value ^ (value >>> 31)
  }

  def trialSeed(baseSeed: Long, trialIndex: Int): Long = {
    require(trialIndex > 0, "Trial indices must be positive")
    mix64(baseSeed + GoldenGamma * trialIndex.toLong)
  }

  def bornRuleSeed(trialSeed: Long): Long =
    mix64(trialSeed ^ BornRuleDomain)

  def postSelectionSeed(trialSeed: Long): Long =
    mix64(trialSeed ^ PostSelectionDomain)

  def bootstrapSeed(baseSeed: Long, thresholdBits: Long): Long =
    mix64(baseSeed ^ thresholdBits ^ BootstrapDomain)
}
