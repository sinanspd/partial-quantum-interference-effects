package com.sinanspd

object OutputDistributionMetricsVerification extends App {
  private def approximately(left: Double, right: Double): Boolean =
    math.abs(left - right) < 1e-12

  val empirical =
    OutputDistributionMetrics.empirical(Vector("0", "0", "0", "1"))
  val reference = Map("0" -> 0.5d, "1" -> 0.5d)
  val distance = OutputDistributionMetrics.distance(empirical, reference)
  assert(approximately(distance.totalVariationDistance, 0.25d))
  assert(
    approximately(
      distance.hellingerFidelity,
      math.sqrt(0.75d * 0.5d) + math.sqrt(0.25d * 0.5d)
    )
  )

  val average = OutputDistributionMetrics.average(
    Vector(Map("0" -> 1d), Map("1" -> 1d))
  )
  assert(average == reference)

  val bootstrap = OutputDistributionMetrics.bootstrap(
    Vector(
      "0" -> reference,
      "0" -> reference,
      "1" -> reference,
      "1" -> reference
    ),
    replicates = 100,
    seed = 260601922L
  )
  assert(bootstrap.replicates == 100)
  assert(bootstrap.totalVariation.standardDeviation >= 0d)
  assert(bootstrap.totalVariation.confidenceLow95 >= 0d)
  assert(bootstrap.totalVariation.confidenceHigh95 <= 1d)

  println("Output-distribution metrics verification passed.")
}
