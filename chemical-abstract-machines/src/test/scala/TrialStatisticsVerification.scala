package com.sinanspd

/** Dependency-free checks for summary statistics and paired tests. */
object TrialStatisticsVerification extends App {
  private def close(actual: Double, expected: Double, tolerance: Double = 1e-6): Unit =
    assert(
      math.abs(actual - expected) <= tolerance,
      s"$actual was not within $tolerance of $expected"
    )

  val summary = TrialStatistics.summarize(Vector(1d, 2d, 3d))
  close(summary.mean, 2d)
  close(summary.standardDeviation, 1d)
  close(summary.confidenceLow95, -0.4841377117, tolerance = 1e-5)
  close(summary.confidenceHigh95, 4.4841377117, tolerance = 1e-5)

  val proportion = TrialStatistics.summarize(
    Vector.fill(25)(1d) ++ Vector.fill(25)(0d),
    proportion = true
  )
  close(proportion.mean, 0.5d)
  assert(proportion.confidenceLow95 < 0.5d && proportion.confidenceHigh95 > 0.5d)
  assert(proportion.confidenceLow95 >= 0d && proportion.confidenceHigh95 <= 1d)

  val pairedT =
    TrialStatistics.pairedTTest(Vector(2d, 4d, 6d), Vector(1d, 2d, 3d))
  close(pairedT.statistic, 3.4641016151, tolerance = 1e-6)
  close(pairedT.pValue, 0.0741799002, tolerance = 1e-5)
  close(pairedT.effectEstimate, 2d)

  val wilcoxon =
    TrialStatistics.wilcoxonSignedRank(Vector(2d, 4d, 6d), Vector(1d, 2d, 3d))
  assert(wilcoxon.pValue >= 0d && wilcoxon.pValue <= 1d)
  close(wilcoxon.effectEstimate, 2d)

  val mcnemar = TrialStatistics.mcnemarExact(
    Vector(true, true, true, true, true),
    Vector(false, false, false, false, false)
  )
  close(mcnemar.pValue, 0.0625d)
  close(mcnemar.effectEstimate, 1d)

  println("Trial statistics verification passed.")
}
