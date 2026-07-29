package com.sinanspd

import scala.collection.mutable
import scala.util.Random

final case class DistributionDistance(
    totalVariationDistance: Double,
    hellingerDistance: Double,
    hellingerFidelity: Double
)

final case class BootstrapInterval(
    standardDeviation: Double,
    confidenceLow95: Double,
    confidenceHigh95: Double
)

final case class DistributionBootstrap(
    replicates: Int,
    totalVariation: BootstrapInterval,
    hellinger: BootstrapInterval
)

object OutputDistributionMetrics {
  def empirical(outcomes: Vector[String]): Map[String, Double] = {
    require(outcomes.nonEmpty, "An empirical distribution requires observations")
    outcomes
      .groupBy(identity)
      .map { case (outcome, values) => outcome -> values.length.toDouble / outcomes.length.toDouble }
  }

  def average(distributions: Vector[Map[String, Double]]): Map[String, Double] = {
    require(distributions.nonEmpty, "An average distribution requires inputs")
    val sums = mutable.Map.empty[String, Double].withDefaultValue(0d)
    distributions.foreach { distribution =>
      distribution.foreach {
        case (outcome, probability) =>
          sums.update(outcome, sums(outcome) + probability)
      }
    }
    sums.iterator.map {
      case (outcome, sum) => outcome -> (sum / distributions.length.toDouble)
    }.toMap
  }

  def distance(
      empiricalDistribution: Map[String, Double],
      referenceDistribution: Map[String, Double]
  ): DistributionDistance = {
    validateDistribution(empiricalDistribution, "empirical")
    validateDistribution(referenceDistribution, "reference")
    val outcomes = empiricalDistribution.keySet ++ referenceDistribution.keySet
    val absoluteDifference = outcomes.iterator.map { outcome =>
      math.abs(
        empiricalDistribution.getOrElse(outcome, 0d) -
          referenceDistribution.getOrElse(outcome, 0d)
      )
    }.sum
    val squaredRootDifference = outcomes.iterator.map { outcome =>
      val empiricalRoot = math.sqrt(empiricalDistribution.getOrElse(outcome, 0d))
      val referenceRoot = math.sqrt(referenceDistribution.getOrElse(outcome, 0d))
      val difference = empiricalRoot - referenceRoot
      difference * difference
    }.sum
    val fidelity = outcomes.iterator.map { outcome =>
      math.sqrt(
        empiricalDistribution.getOrElse(outcome, 0d) *
          referenceDistribution.getOrElse(outcome, 0d)
      )
    }.sum
    DistributionDistance(
      totalVariationDistance = 0.5d * absoluteDifference,
      hellingerDistance = math.sqrt(0.5d * squaredRootDifference),
      hellingerFidelity = fidelity
    )
  }

  def parse(rendered: String): Map[String, Double] =
    if (rendered.trim.isEmpty) Map.empty
    else {
      rendered.split("\\|", -1).iterator.map { entry =>
        val separator = entry.lastIndexOf(':')
        require(separator > 0, s"Invalid rendered distribution entry '$entry'")
        entry.substring(0, separator) -> entry.substring(separator + 1).toDouble
      }.toMap
    }

  /**
    * Non-parametric paired bootstrap. Resampling each observation together
    * with its trial-specific ideal distribution preserves Simon
    * post-selection mixtures.
    */
  def bootstrap(
      observations: Vector[(String, Map[String, Double])],
      replicates: Int,
      seed: Long
  ): DistributionBootstrap = {
    require(observations.nonEmpty, "Distribution bootstrap requires observations")
    require(replicates > 0, "Distribution bootstrap requires positive replicates")
    val random = new Random(seed)
    val tvd = Vector.newBuilder[Double]
    val hellinger = Vector.newBuilder[Double]

    (0 until replicates).foreach { _ =>
      val sample = Vector.fill(observations.length) {
        observations(random.nextInt(observations.length))
      }
      val metric = distance(
        empirical(sample.map(_._1)),
        average(sample.map(_._2))
      )
      tvd += metric.totalVariationDistance
      hellinger += metric.hellingerDistance
    }

    DistributionBootstrap(
      replicates = replicates,
      totalVariation = interval(tvd.result()),
      hellinger = interval(hellinger.result())
    )
  }

  private def interval(values: Vector[Double]): BootstrapInterval = {
    val sorted = values.sorted
    val mean = values.sum / values.length.toDouble
    val standardDeviation =
      if (values.length == 1) 0d
      else {
        val squared = values.map(value => math.pow(value - mean, 2d)).sum
        math.sqrt(squared / (values.length - 1).toDouble)
      }
    BootstrapInterval(
      standardDeviation = standardDeviation,
      confidenceLow95 = percentile(sorted, 0.025d),
      confidenceHigh95 = percentile(sorted, 0.975d)
    )
  }

  private def percentile(sorted: Vector[Double], probability: Double): Double = {
    val position = probability * (sorted.length - 1).toDouble
    val lower = math.floor(position).toInt
    val upper = math.ceil(position).toInt
    if (lower == upper) sorted(lower)
    else {
      val fraction = position - lower.toDouble
      sorted(lower) * (1d - fraction) + sorted(upper) * fraction
    }
  }

  private def validateDistribution(distribution: Map[String, Double], label: String): Unit = {
    require(distribution.nonEmpty, s"The $label distribution cannot be empty")
    require(
      distribution.values.forall(value => value >= 0d && java.lang.Double.isFinite(value)),
      s"The $label distribution contains an invalid probability"
    )
    val total = distribution.values.sum
    require(
      math.abs(total - 1d) <= 1e-9,
      s"The $label distribution has probability mass $total instead of 1"
    )
  }
}
