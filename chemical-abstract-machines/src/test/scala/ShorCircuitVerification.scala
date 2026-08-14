package com.sinanspd

/**
  * Dependency-free verification executable:
  *
  *   sbt "Test / runMain com.sinanspd.ShorCircuitVerification"
  */
object ShorCircuitVerification extends App {
  private val tolerance = 1e-10

  private def bits(value: Int, width: Int): Vector[Boolean] =
    Vector.tabulate(width) { index =>
      ((value >> (width - index - 1)) & 1) == 1
    }

  private def render(value: Int, width: Int): String =
    bits(value, width).map(if (_) '1' else '0').mkString

  private def approximately(actual: Double, expected: Double): Boolean =
    math.abs(actual - expected) <= tolerance

  /**
    * Exact counting-register distribution for a uniform input of size 2^t and
    * an oracle whose distinct outputs partition the inputs by x mod period.
    */
  private def expectedPeriodDistribution(
      countingQubits: Int,
      period: Int
  ): Vector[Double] = {
    val dimension = 1 << countingQubits
    val normalization = dimension.toDouble * dimension.toDouble

    Vector.tabulate(dimension) { measuredValue =>
      (0 until period).iterator.map { residue =>
        val terms = ((dimension - 1 - residue) / period) + 1
        val halfStepAngle =
          -math.Pi * period.toDouble * measuredValue.toDouble /
            dimension.toDouble
        val denominator = math.sin(halfStepAngle)
        val magnitude =
          if (math.abs(denominator) <= 1e-12d) terms.toDouble
          else math.sin(terms.toDouble * halfStepAngle) / denominator
        magnitude * magnitude / normalization
      }.sum
    }
  }

  private def possiblePostProcessingBins(
      postProcessing: ShorPostProcessing
  ): Set[Int] = {
    val dimension = 1 << postProcessing.countingQubits
    (2 until postProcessing.modulus).iterator.flatMap { denominator =>
      (1 until denominator).iterator.flatMap { numerator =>
        val scaledNumerator = numerator.toLong * dimension.toLong
        val lower = (scaledNumerator / denominator.toLong).toInt
        Iterator(lower, lower + 1)
      }
    }.filter(value => value > 0 && value < dimension).toSet
  }

  private def verify(
      experiment: ExperimentSpec,
      expectedPeriod: Int,
      expectedPostProcessedSuccess: Double,
      expectedLeafMetrics: CircuitLeafMetrics
  ): Unit = {
    val countingQubits =
      experiment.shorPostProcessing
        .map(_.countingQubits)
        .getOrElse(
          throw new IllegalStateException(
            s"${experiment.alias} post-processing is not configured"
          )
        )
    val postProcessing = experiment.shorPostProcessing.get
    val metrics = CircuitReferenceMetrics.calculate(
      experiment,
      selectedPostSelectionOutcome = None,
      instanceCount = 1
    )
    val expected =
      expectedPeriodDistribution(countingQubits, expectedPeriod)

    expected.indices.foreach { measuredValue =>
      val outcome = bits(measuredValue, countingQubits)
      val actualProbability = metrics.idealProbability(outcome)
      val expectedProbability = expected(measuredValue)
      assert(
        approximately(actualProbability, expectedProbability),
        s"${experiment.alias} bin ${render(measuredValue, countingQubits)} " +
          s"has probability $actualProbability, expected $expectedProbability"
      )
    }

    assert(
      approximately(metrics.idealOutputProbabilities.values.sum, 1d),
      s"${experiment.alias} ideal probabilities sum to " +
        metrics.idealOutputProbabilities.values.sum
    )

    val successfulBins =
      possiblePostProcessingBins(postProcessing).filter { measuredValue =>
        postProcessing.process(bits(measuredValue, countingQubits)).success
      }
    val postProcessedSuccessProbability =
      successfulBins.iterator.map { measuredValue =>
        metrics.idealProbability(bits(measuredValue, countingQubits))
      }.sum

    assert(
      approximately(
        postProcessedSuccessProbability,
        expectedPostProcessedSuccess
      ),
      s"${experiment.alias} post-processed success probability is " +
        s"$postProcessedSuccessProbability, expected $expectedPostProcessedSuccess"
    )

    assert(
      experiment.leafMetrics == expectedLeafMetrics,
      s"${experiment.alias} leaf metrics are ${experiment.leafMetrics}, " +
        s"expected $expectedLeafMetrics"
    )

    println(
      s"${experiment.alias} circuit verification passed; " +
        s"post-processed ideal success is $postProcessedSuccessProbability."
    )
  }

  verify(
    ExperimentCatalog.shorN15,
    expectedPeriod = 4,
    expectedPostProcessedSuccess = 0.75d,
    expectedLeafMetrics = CircuitLeafMetrics(total = 256, correct = 112)
  )
  verify(
    ExperimentCatalog.shorN21,
    expectedPeriod = 6,
    expectedPostProcessedSuccess = 0.622612005321576d,
    expectedLeafMetrics =
      CircuitLeafMetrics(
        total = BigInt("4294967296"),
        correct = BigInt("1114112")
      )
  )

  assert(
    ExperimentCatalog.shorN21.qubitCount == 21,
    s"shor-n21 has ${ExperimentCatalog.shorN21.qubitCount} qubits instead of 21"
  )
  assert(
    ExperimentCatalog.shorN21.shorPostProcessing.exists(_.countingQubits == 16),
    "shor-n21 must use a 16-qubit counting register"
  )
  assert(
    ExperimentCatalog.shorN21.gates.exists {
      case ModularExponentiation(2, 21, input, output) =>
        input == (0 until 16).toVector &&
          output == (16 until 21).toVector
      case _ => false
    },
    "shor-n21 does not contain the expected 16+5 modular-exponentiation layout"
  )
}
