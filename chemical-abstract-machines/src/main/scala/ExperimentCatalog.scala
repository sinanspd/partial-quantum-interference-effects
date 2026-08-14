package com.sinanspd

import com.sinanspd.qure.circuit.gates._
import com.sinanspd.qure.circuit.QVec
import spire.math.Complex

import scala.collection.concurrent.TrieMap

/** Logical gates missing from the old Qure release used by this repository. */
final case class PhaseFlipWhenAllOne(qubits: Vector[Int]) extends Gate
final case class ModularExponentiation(
    base: Int,
    modulus: Int,
    inputQubits: Vector[Int],
    outputQubits: Vector[Int]
) extends Gate

sealed trait ExperimentBackend {
  def label: String
}

case object ChamPathSum extends ExperimentBackend {
  override val label: String = "CHAM path generation + CHAM endpoint interference"
}

final case class CorrectOutcomeSet(
    observedQubits: Vector[Int],
    states: Set[Vector[Boolean]],
    description: String
) {
  require(observedQubits.nonEmpty, "A correctness check must observe at least one qubit")
  require(states.nonEmpty, "A correctness check must contain at least one valid state")
  require(
    states.forall(_.length == observedQubits.length),
    "Every correct state must match the observed-register width"
  )

  def observe(sampledState: Vector[Boolean]): Vector[Boolean] =
    observedQubits.map(sampledState)

  def contains(sampledState: Vector[Boolean]): Boolean =
    states.contains(observe(sampledState))

  def renderedStates: String =
    states.toVector.map(_.map(if (_) '1' else '0').mkString).sorted.mkString("{", ", ", "}")
}

final case class ExperimentSpec(
    alias: String,
    description: String,
    qubitCount: Int,
    initialState: QVec,
    gates: List[Gate],
    correctOutcomes: CorrectOutcomeSet,
    defaultInstances: Int,
    backend: ExperimentBackend = ChamPathSum,
    postSelection: Option[PostSelectionSpec] = None,
    shorPostProcessing: Option[ShorPostProcessing] = None,
    resultQubits: Option[Vector[Int]] = None,
    paperTerminalContributions: Option[BigInt] = None,
    markedAmplitudePerInstance: Option[Double] = None,
    analyticalLeafMetrics: Option[CircuitLeafMetrics] = None,
    analyticalReferenceMetrics: Option[AnalyticalCircuitReference] = None,
    requireAllQubitsUsed: Boolean = false,
    notes: Vector[String] = Vector.empty
) {
  val hadamardCount: Int = gates.count(_.isInstanceOf[H])
  private val correctnessCache = TrieMap.empty[Vector[Boolean], Boolean]

  lazy val leafMetrics: CircuitLeafMetrics = CircuitLeafMetrics.calculate(this)

  def isCorrectTerminalState(terminalState: Vector[Boolean]): Boolean = {
    val observedState = correctOutcomes.observe(terminalState)
    correctnessCache.getOrElseUpdate(
      observedState,
      shorPostProcessing
        .map(_.process(observedState).success)
        .getOrElse(correctOutcomes.states.contains(observedState))
    )
  }

  def instancesForThreshold(threshold: Double): Int =
    markedAmplitudePerInstance match {
      case Some(amplitude) =>
        math.max(1, math.ceil(threshold / amplitude - 1e-12).toInt)
      case None =>
        defaultInstances
    }

  def validate(): Unit = {
    require(alias.nonEmpty, "Experiment alias cannot be empty")
    require(qubitCount > 0, s"$alias must have at least one qubit")
    require(
      initialState.v.length == qubitCount,
      s"$alias has $qubitCount qubits but an initial state of ${initialState.v.length} bits"
    )
    require(defaultInstances > 0, s"$alias must use at least one circuit instance")

    val referencedQubits = gates.flatMap {
      case X(q)                              => List(q)
      case H(q)                              => List(q)
      case CX(control, target)               => List(control, target)
      case CCX(control1, control2, target)   => List(control1, control2, target)
      case CZ(control, target)               => List(control, target)
      case Swap(q1, q2)                      => List(q1, q2)
      case CRotate(control, _, target)       => List(control, target)
      case Rotate(_, q)                      => List(q)
      case RZ(_, q)                          => List(q)
      case Measure(q)                        => List(q)
      case PhaseFlipWhenAllOne(qubits)       => qubits.toList
      case ModularExponentiation(_, _, in, out) => in.toList ++ out.toList
      case _                                 => Nil
    }

    require(
      referencedQubits.forall(q => q >= 0 && q < qubitCount),
      s"$alias contains a gate outside its $qubitCount-qubit register"
    )
    if (requireAllQubitsUsed) {
      require(
        referencedQubits.toSet == (0 until qubitCount).toSet,
        s"$alias does not actively use every qubit in its $qubitCount-qubit register"
      )
    }
    resultQubits.foreach { qubits =>
      require(qubits.nonEmpty, s"$alias must expose at least one result qubit")
      require(
        qubits.distinct.length == qubits.length &&
          qubits.forall(q => q >= 0 && q < qubitCount),
        s"$alias has invalid result-qubit indices"
      )
    }
    val terminalWidth = postSelection
      .map(_.retainedQubits.length)
      .orElse(resultQubits.map(_.length))
      .getOrElse(qubitCount)
    require(
      correctOutcomes.observedQubits.distinct.length == correctOutcomes.observedQubits.length &&
        correctOutcomes.observedQubits.forall(q => q >= 0 && q < terminalWidth),
      s"$alias has invalid correctness-check qubit indices"
    )

    postSelection.foreach { selection =>
      require(
        resultQubits.isEmpty,
        s"$alias cannot combine result-qubit projection with post-selection"
      )
      require(
        selection.retainedQubits.distinct.length == selection.retainedQubits.length &&
          selection.measuredQubits.distinct.length == selection.measuredQubits.length &&
          selection.retainedQubits.forall(q => q >= 0 && q < qubitCount) &&
          selection.measuredQubits.forall(q => q >= 0 && q < qubitCount) &&
          selection.retainedQubits.toSet.intersect(selection.measuredQubits.toSet).isEmpty,
        s"$alias has invalid terminal post-selection qubits"
      )
    }
    shorPostProcessing.foreach { postProcessing =>
      require(
        postProcessing.countingQubits == correctOutcomes.observedQubits.length,
        s"$alias Shor post-processing width does not match its observed register"
      )
      require(
        postSelection.isEmpty,
        s"$alias cannot use terminal post-selection and Shor post-processing together"
      )
    }

    val metrics = leafMetrics
    require(metrics.total > 0, s"$alias must create at least one terminal leaf")
    require(
      metrics.total == (BigInt(1) << hadamardCount),
      s"$alias leaf count ${metrics.total} does not match 2^$hadamardCount"
    )
    require(
      metrics.correct >= 0 && metrics.correct <= metrics.total,
      s"$alias has invalid leaf metrics: $metrics"
    )
    analyticalReferenceMetrics.foreach { reference =>
      require(
        reference.fullInterferenceContributionsPerCopy <= metrics.total,
        s"$alias analytical reference admits more contributions than B_total"
      )
      require(
        reference.idealOutputProbabilities.keys.forall(
          _.length == correctOutcomes.observedQubits.length
        ),
        s"$alias analytical reference outcomes do not match the observed-register width"
      )
    }
  }
}

object ExperimentCatalog {
  private val zeroAmplitude = Complex(1d, 0d)

  private def zeroState(qubits: Int): QVec =
    QVec(zeroAmplitude, Vector.fill(qubits)(false))

  private def bits(value: String): Vector[Boolean] =
    value.map {
      case '0' => false
      case '1' => true
      case other => throw new IllegalArgumentException(s"Not a bit: $other")
    }.toVector

  private def allStates(width: Int): Set[Vector[Boolean]] =
    (0 until (1 << width)).map { value =>
      Vector.tabulate(width) { index =>
        ((value >> (width - index - 1)) & 1) == 1
      }
    }.toSet

  private def outcomes(
      observedWidth: Int,
      correctStates: Set[Vector[Boolean]],
      description: String
  ): CorrectOutcomeSet =
    CorrectOutcomeSet(
      observedQubits = (0 until observedWidth).toVector,
      states = correctStates,
      description = description
    )

  private def singleOutcome(state: String, description: String): CorrectOutcomeSet =
    outcomes(state.length, Set(bits(state)), description)

  private def shorFrequencyOutcomes(
      countingQubits: Int,
      period: Int,
      description: String
  ): CorrectOutcomeSet = {
    val bins = 1.until(period).map { numerator =>
      val value =
        math.round(numerator.toDouble * (1 << countingQubits).toDouble / period.toDouble).toInt
      Vector.tabulate(countingQubits) { index =>
        ((value >> (countingQubits - index - 1)) & 1) == 1
      }
    }.toSet
    outcomes(countingQubits, bins, description)
  }

 
  private def inverseQft(register: Vector[Int]): List[Gate] = {
    require(register.nonEmpty, "An inverse QFT register cannot be empty")
    require(
      register.distinct.length == register.length,
      "An inverse QFT register cannot contain duplicate qubits"
    )
    require(
      register.length <= 31,
      "This inverse QFT implementation requires at most 31 qubits"
    )

    val gates = List.newBuilder[Gate]

    register.indices.take(register.length / 2).foreach { leftIndex =>
      gates += Swap(register(leftIndex), register(register.length - leftIndex - 1))
    }

    register.indices.reverse.foreach { targetIndex =>
      ((targetIndex + 1) until register.length).reverse.foreach { controlIndex =>
        val separation = controlIndex - targetIndex
        val denominator = -(1 << (separation - 1))
        gates += CRotate(
          register(controlIndex),
          denominator,
          register(targetIndex)
        )
      }
      gates += H(register(targetIndex))
    }

    gates.result()
  }

  private def basisBits(value: Int, width: Int): Vector[Boolean] =
    Vector.tabulate(width) { index =>
      ((value >> (width - index - 1)) & 1) == 1
    }

  /**
    * Closed-form counting-register distribution for a uniform input of size
    * 2^t and a modular function whose order is `period`.
    */
  private def periodFindingDistribution(
      countingQubits: Int,
      period: Int
  ): Map[Vector[Boolean], Double] = {
    require(countingQubits > 0 && countingQubits < 31)
    require(period > 0)
    val dimension = 1 << countingQubits
    val normalization = dimension.toDouble * dimension.toDouble

    (0 until dimension).iterator.map { measuredValue =>
      val probability = (0 until period).iterator.map { residue =>
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
      basisBits(measuredValue, countingQubits) -> probability
    }.toMap
  }

  private def analyticalShorLeafMetrics(
      postProcessing: ShorPostProcessing
  ): CircuitLeafMetrics = {
    val dimension = 1 << postProcessing.countingQubits
    // A successful bin must lie within half a measurement bin of a rational
    // p/q with q < N. Enumerating the two neighboring integers for those
    // rationals is exhaustive and avoids scanning all 2^t outcomes.
    val possibleBins = (2 until postProcessing.modulus).iterator.flatMap {
      denominator =>
        (1 until denominator).iterator.flatMap { numerator =>
          val scaledNumerator = numerator.toLong * dimension.toLong
          val lower = (scaledNumerator / denominator.toLong).toInt
          Iterator(lower, lower + 1)
        }
    }.filter(value => value > 0 && value < dimension).toSet
    val correctBins = possibleBins.count { measuredValue =>
      postProcessing
        .process(basisBits(measuredValue, postProcessing.countingQubits))
        .success
    }
    CircuitLeafMetrics(
      total = BigInt(dimension) * BigInt(dimension),
      correct = BigInt(correctBins) * BigInt(dimension)
    )
  }

  private def taggedGrover(
      alias: String,
      marked: String,
      iterations: Int,
      instances: Int,
      notes: Vector[String] = Vector.empty
  ): ExperimentSpec = {
    val target = bits(marked)
    val qubits = target.indices.toVector
    val phaseFlip = List(PhaseFlipWhenAllOne(qubits))

    def markTarget: List[Gate] = {
      val zeroBitFlips = target.zipWithIndex.collect {
        case (false, qubit) => X(qubit): Gate
      }.toList
      zeroBitFlips ++ phaseFlip ++ zeroBitFlips.reverse
    }

    def diffusion: List[Gate] =
      qubits.map(H).toList ++
        qubits.map(X).toList ++
        phaseFlip ++
        qubits.reverse.map(X).toList ++
        qubits.map(H).toList

    val circuit =
      qubits.map(H).toList ++
        List.fill(iterations)(markTarget ++ diffusion).flatten
    val theta = math.asin(1d / math.sqrt(math.pow(2d, target.length.toDouble)))
    val markedAmplitude = math.abs(math.sin((2d * iterations + 1d) * theta))

    ExperimentSpec(
      alias = alias,
      description =
        s"${target.length}-qubit tagged-state Grover search for |$marked>, $iterations iteration(s)",
      qubitCount = target.length,
      initialState = zeroState(target.length),
      gates = circuit,
      correctOutcomes = outcomes(
        target.length,
        Set(target),
        s"the Grover marked state |$marked>"
      ),
      defaultInstances = instances,
      markedAmplitudePerInstance = Some(markedAmplitude),
      notes = notes
    )
  }

  private final case class Literal(qubit: Int, positive: Boolean)

  private val grover8SearchQubits = (0 until 5).toVector
  private val grover8Ancillas = Vector(5, 6, 7)
  private val grover8MarkedState = bits("01111")

  /**
    * A proper 3-CNF over five variables with the unique solution 01111.
    *
    * The oracle is compiled to the equivalent marked-state phase operation
    * below. Each clause has three distinct variables; the truth-table
    * validation guards against the compiled oracle and formula diverging.
    */
  private val grover8ThreeSatFormula: Vector[Vector[Literal]] = Vector(
    Vector(Literal(0, positive = false), Literal(1, positive = false), Literal(2, positive = false)),
    Vector(Literal(0, positive = false), Literal(1, positive = true), Literal(2, positive = false)),
    Vector(Literal(0, positive = true), Literal(1, positive = true), Literal(2, positive = false)),
    Vector(Literal(0, positive = false), Literal(1, positive = false), Literal(2, positive = true)),
    Vector(Literal(0, positive = true), Literal(1, positive = false), Literal(2, positive = true)),
    Vector(Literal(0, positive = false), Literal(1, positive = true), Literal(2, positive = true)),
    Vector(Literal(0, positive = true), Literal(1, positive = true), Literal(2, positive = true)),
    Vector(Literal(0, positive = true), Literal(1, positive = false), Literal(3, positive = true)),
    Vector(Literal(0, positive = true), Literal(1, positive = false), Literal(4, positive = true))
  )

  private def satisfiesGrover8ThreeSat(state: Vector[Boolean]): Boolean =
    grover8ThreeSatFormula.forall { clause =>
      clause.exists { literal =>
        state(literal.qubit) == literal.positive
      }
    }

  /**
    * Decomposes a five-controlled phase flip using three clean work ancillas.
    * All three ancillas are returned to zero.
    */
  private val grover8AllOnePhase: List[Gate] = List(
    CCX(0, 1, 5),
    CCX(5, 2, 6),
    CCX(6, 3, 7),
    CZ(7, 4),
    CCX(6, 3, 7),
    CCX(5, 2, 6),
    CCX(0, 1, 5)
  )

  private val grover8Oracle: List[Gate] =
    List(X(0)) ++ grover8AllOnePhase ++ List(X(0))

  private val grover8Diffusion: List[Gate] =
    grover8SearchQubits.map(H).toList ++
      grover8SearchQubits.map(X).toList ++
      grover8AllOnePhase ++
      grover8SearchQubits.reverse.map(X).toList ++
      grover8SearchQubits.map(H).toList

  private def grover8(
      alias: String,
      description: String,
      iterations: Int,
      instances: Int,
      correctnessDescription: String,
      notes: Vector[String] = Vector.empty
  ): ExperimentSpec = {
    require(iterations > 0, "An eight-qubit Grover circuit needs at least one iteration")
    val theta = math.asin(1d / math.sqrt(32d))
    val markedAmplitude = math.abs(math.sin((2d * iterations + 1d) * theta))
    val gates =
      grover8SearchQubits.map(H).toList ++
        List.fill(iterations)(grover8Oracle ++ grover8Diffusion).flatten

    ExperimentSpec(
      alias = alias,
      description = description,
      qubitCount = grover8SearchQubits.length + grover8Ancillas.length,
      initialState = zeroState(grover8SearchQubits.length + grover8Ancillas.length),
      gates = gates,
      correctOutcomes = outcomes(
        observedWidth = 5,
        correctStates = Set(grover8MarkedState),
        description = correctnessDescription
      ),
      defaultInstances = instances,
      resultQubits = Some(grover8SearchQubits),
      markedAmplitudePerInstance = Some(markedAmplitude),
      requireAllQubitsUsed = true,
      notes =
        Vector(
          "Qubits 0-4 are search qubits; qubits 5-7 are active clean oracle/diffusion work ancillas.",
          "Every oracle and diffusion phase operation uncomputes all three ancillas before the next step."
        ) ++ notes
    )
  }

  private def validateGrover8Oracle(): Unit = {
    def applyBasisGate(
        current: (Vector[Boolean], Int),
        gate: Gate
    ): (Vector[Boolean], Int) = {
      val (state, phase) = current
      gate match {
        case X(target) =>
          (state.updated(target, !state(target)), phase)
        case CCX(control1, control2, target) =>
          val next =
            if (state(control1) && state(control2))
              state.updated(target, !state(target))
            else state
          (next, phase)
        case CZ(control, target) =>
          (state, if (state(control) && state(target)) -phase else phase)
        case unsupported =>
          throw new IllegalStateException(
            s"Eight-qubit Grover oracle validation does not support $unsupported"
          )
      }
    }

    val satisfyingStates = allStates(5).filter(satisfiesGrover8ThreeSat)
    require(
      satisfyingStates == Set(grover8MarkedState),
      s"Eight-qubit 3-SAT formula has unexpected solutions: $satisfyingStates"
    )

    allStates(5).foreach { searchState =>
      val initial = searchState ++ Vector.fill(grover8Ancillas.length)(false)
      val (finalState, phase) =
        grover8Oracle.foldLeft((initial, 1))(applyBasisGate)
      val shouldBeMarked = satisfiesGrover8ThreeSat(searchState)

      require(
        finalState == initial,
        s"Eight-qubit Grover oracle did not uncompute its ancillas for $searchState"
      )
      require(
        phase == (if (shouldBeMarked) -1 else 1),
        s"Eight-qubit Grover oracle marked the wrong basis state $searchState"
      )
    }
  }

  private def computeThreeLiteralOr(
      literals: Vector[Literal],
      target: Int,
      work: Int
  ): List[Gate] = {
    require(literals.length == 3, "A 3-SAT clause must contain exactly three literals")
    val positiveLiteralFlips = literals.collect {
      case Literal(qubit, true) => X(qubit): Gate
    }.toList
    val controls = literals.map(_.qubit)

    List(X(target)) ++
      positiveLiteralFlips ++
      List(
        CCX(controls(0), controls(1), work),
        CCX(work, controls(2), target),
        CCX(controls(0), controls(1), work)
      ) ++
      positiveLiteralFlips.reverse
  }

  private val grover30SearchQubits = (0 until 8).toVector
  private val grover30ClauseFlags = (8 until 18).toVector
  private val grover30ClauseWork = Vector(18, 19)
  private val grover30ReductionWork = (20 until 29).toVector
  private val grover30PhaseAncilla = 29
  private val grover30MarkedState = bits("01111111")


  private val grover30ComputeClauses: List[Gate] =
    List(X(8), CX(0, 8)) ++
      (1 until 8).map(q => CX(q, q + 8): Gate).toList ++
      computeThreeLiteralOr(
        Vector(Literal(1, positive = true), Literal(2, positive = true), Literal(3, positive = true)),
        target = 16,
        work = grover30ClauseWork(0)
      ) ++
      computeThreeLiteralOr(
        Vector(Literal(0, positive = false), Literal(4, positive = true), Literal(5, positive = true)),
        target = 17,
        work = grover30ClauseWork(1)
      )

  private val grover30ReduceClauses: List[Gate] =
    List(CCX(grover30ClauseFlags(0), grover30ClauseFlags(1), grover30ReductionWork(0))) ++
      (2 until grover30ClauseFlags.length).map { clauseIndex =>
        CCX(
          grover30ReductionWork(clauseIndex - 2),
          grover30ClauseFlags(clauseIndex),
          grover30ReductionWork(clauseIndex - 1)
        ): Gate
      }.toList

  private val grover30Oracle: List[Gate] =
    grover30ComputeClauses ++
      grover30ReduceClauses ++
      List(
        X(grover30PhaseAncilla),
        CZ(grover30ReductionWork.last, grover30PhaseAncilla),
        X(grover30PhaseAncilla)
      ) ++
      grover30ReduceClauses.reverse ++
      grover30ComputeClauses.reverse

  private val grover30Diffusion: List[Gate] =
    grover30SearchQubits.map(H).toList ++
      grover30SearchQubits.map(X).toList ++
      List(PhaseFlipWhenAllOne(grover30SearchQubits)) ++
      grover30SearchQubits.reverse.map(X).toList ++
      grover30SearchQubits.map(H).toList

  private val grover30MarkedAmplitude = {
    val theta = math.asin(1d / math.sqrt(256d))
    math.sin(3d * theta)
  }

  val grover3Sat30: ExperimentSpec = ExperimentSpec(
    alias = "grover-3sat-30",
    description =
      "30-qubit Grover search with 8 search qubits and 22 active 3-SAT oracle ancillas",
    qubitCount = 30,
    initialState = zeroState(30),
    gates =
      grover30SearchQubits.map(H).toList ++ grover30Oracle ++ grover30Diffusion,
    correctOutcomes = outcomes(
      8,
      Set(grover30MarkedState),
      "the unique satisfying assignment of the constructed 3-SAT oracle"
    ),
    defaultInstances = 1,
    resultQubits = Some(grover30SearchQubits),
    markedAmplitudePerInstance = Some(grover30MarkedAmplitude),
    requireAllQubitsUsed = true,
    notes = Vector(
      "One circuit copy expands to 2^24 = 16,777,216 terminal path contributions.",
      "The default copy count is chosen from the threshold; set instanceCountOverride to reproduce a fixed-copy run."
    )
  )

  private def validateGrover30Oracle(): Unit = {
    def applyBasisGate(
        current: (Vector[Boolean], Int),
        gate: Gate
    ): (Vector[Boolean], Int) = {
      val (state, phase) = current
      gate match {
        case X(target) =>
          (state.updated(target, !state(target)), phase)
        case CX(control, target) =>
          val next = if (state(control)) state.updated(target, !state(target)) else state
          (next, phase)
        case CCX(control1, control2, target) =>
          val next =
            if (state(control1) && state(control2))
              state.updated(target, !state(target))
            else state
          (next, phase)
        case CZ(control, target) =>
          (state, if (state(control) && state(target)) -phase else phase)
        case unsupported =>
          throw new IllegalStateException(
            s"30-qubit oracle validation does not support $unsupported"
          )
      }
    }

    (0 until 256).foreach { value =>
      val searchState = Vector.tabulate(8) { index =>
        ((value >> (7 - index)) & 1) == 1
      }
      val initial = searchState ++ Vector.fill(22)(false)
      val (finalState, phase) =
        grover30Oracle.foldLeft((initial, 1))(applyBasisGate)
      val shouldBeMarked = searchState == grover30MarkedState

      require(
        finalState == initial,
        s"grover-3sat-30 oracle did not uncompute its ancillas for $searchState"
      )
      require(
        phase == (if (shouldBeMarked) -1 else 1),
        s"grover-3sat-30 oracle marked the wrong basis state $searchState"
      )
    }
  }

  private def simon(
      alias: String,
      inputQubits: Int,
      instances: Int,
      paperTotal: Option[BigInt]
  ): ExperimentSpec = {
    val input = (0 until inputQubits).toVector
    val output = input.map(_ + inputQubits)
    val lastInput = input.last

    // f_i(x) = x_i xor x_(n-1), with the final output bit fixed at zero.
    // Therefore f(x) = f(x xor 11...1).
    val oracle = input.dropRight(1).flatMap { q =>
      Vector(CX(q, output(q)), CX(lastInput, output(q)))
    }.toList

    val possibleOutputs = (0 until (1 << (inputQubits - 1))).map { value =>
      Vector.tabulate(inputQubits) { index =>
        index < inputQubits - 1 &&
          ((value >> (inputQubits - 2 - index)) & 1) == 1
      }
    }.toVector
    val allInputStates = allStates(inputQubits)
    val validInputStates =
      allInputStates.filter(state => state.count(identity) % 2 == 0)
    val validProbability = 1d / validInputStates.size.toDouble

    ExperimentSpec(
      alias = alias,
      description = s"Simon period finding with n=$inputQubits and secret 11...1",
      qubitCount = inputQubits * 2,
      initialState = zeroState(inputQubits * 2),
      gates = input.map(H).toList ++ oracle ++ input.map(H).toList,
      correctOutcomes = outcomes(
        inputQubits,
        validInputStates,
        "Simon outputs y satisfying y dot 11...1 = 0 modulo 2"
      ),
      defaultInstances = instances,
      postSelection = Some(
        PostSelectionSpec(
          retainedQubits = input,
          measuredQubits = output,
          outcomes = possibleOutputs.map { bits =>
            PostSelectionOutcome(
              bits,
              probability = 1d / possibleOutputs.length.toDouble
            )
          },
          description = "Simon oracle-output measurement"
        )
      ),
      analyticalLeafMetrics = Some(
        CircuitLeafMetrics(
          total = BigInt(1) << (inputQubits * 2),
          correct = BigInt(1) << inputQubits
        )
      ),
      analyticalReferenceMetrics = Some(
        AnalyticalCircuitReference(
          fullInterferenceContributionsPerCopy =
            BigInt(1) << (inputQubits + 1),
          fullInterferenceEndpointStates = 1 << inputQubits,
          idealOutputProbabilities =
            validInputStates.iterator.map(_ -> validProbability).toMap
        )
      ),
      paperTerminalContributions = paperTotal
    )
  }

  val deutschJozsa: ExperimentSpec = ExperimentSpec(
    alias = "deutsch-jozsa",
    description = "two-qubit Deutsch-Jozsa example from the original runner",
    qubitCount = 2,
    initialState = zeroState(2),
    gates = List(X(1), H(0), H(1), CX(0, 1), CX(0, 1), H(0)),
    correctOutcomes = CorrectOutcomeSet(
      observedQubits = Vector(0),
      states = Set(Vector(false)),
      description = "Deutsch-Jozsa's input-register result for the constant oracle"
    ),
    defaultInstances = 1
  )

  val grover4Tagged: ExperimentSpec = taggedGrover(
    alias = "grover-4-tagged",
    marked = "0000",
    iterations = 1,
    instances = 2,
    notes = Vector(
      "Threshold-aware selection uses one copy through threshold 0.5 and two copies for higher paper thresholds."
    )
  ).copy(paperTerminalContributions = Some(BigInt(4096)))

  val grover3Sat: ExperimentSpec = grover8(
    alias = "grover-3sat",
    description =
      "eight-qubit 3-SAT Grover circuit with five search qubits and three work ancillas",
    iterations = 1,
    instances = 2,
    correctnessDescription =
      "the unique satisfying assignment 01111 of the validated five-variable 3-CNF",
    notes = Vector(
      "One copy has 15 Hadamards and therefore 32,768 terminal leaves."
    )
  )

  val grover3Sat3QApprox: ExperimentSpec = ExperimentSpec(
    alias = "grover-3sat-3q-approx",
    description = "legacy three-qubit compiled 3-SAT approximation",
    qubitCount = 3,
    initialState = zeroState(3),
    gates = List(
      H(0), H(1), H(2), X(2), CX(1, 2), Rotate(-4, 2), CX(0, 2), Rotate(4, 2), CX(1, 2),
      Rotate(4, 1), Rotate(-4, 2), CX(0, 2), CX(0, 1), Rotate(4, 2), CX(0, 1), Rotate(4, 2),
      Rotate(4, 0), Rotate(-4, 1), X(2), CX(0, 1),
      H(0), H(1), H(2), X(0), X(1), X(2),
      CX(1, 2), Rotate(-4, 2), CX(0, 2), Rotate(4, 2), CX(1, 2), Rotate(4, 1), Rotate(-4, 2),
      CX(0, 2), CX(0, 1), Rotate(4, 0), Rotate(-4, 1), Rotate(4, 1), X(2), H(2), CX(0, 1),
      X(0), X(1), X(2), H(0), H(1),
      CX(1, 2), Rotate(-4, 2), CX(0, 2), Rotate(4, 2), CX(1, 2), Rotate(4, 1), Rotate(-4, 2),
      CX(0, 2), CX(0, 1), Rotate(4, 0), Rotate(-4, 1), Rotate(4, 2), X(0), CX(0, 1),
      H(0), H(1), H(2), X(0), X(1), X(2), H(0), H(1), H(2)
    ),
    correctOutcomes = singleOutcome(
      "011",
      "the satisfying assignment encoded by the original groverSAT011 circuit"
    ),
    defaultInstances = 1,
    notes = Vector(
      "Preserved only for comparison with the original groverSAT011 source.",
      "It is not the paper's claimed five-search-plus-three-ancilla circuit.",
      "Rotate(4) and Rotate(-4) are evaluated as exact unitary T and T-dagger phase gates."
    )
  )

  val grover5Iteration1: ExperimentSpec = grover8(
    alias = "grover-5-r1",
    description =
      "eight-qubit Grover search for 01111 with five search qubits, three ancillas, and one iteration",
    iterations = 1,
    instances = 2,
    correctnessDescription = "the Grover marked state |01111>"
  )

  val grover5Iteration2: ExperimentSpec = grover8(
    alias = "grover-5-r2",
    description =
      "eight-qubit Grover search for 01111 with five search qubits, three ancillas, and two iterations",
    iterations = 2,
    instances = 2,
    correctnessDescription = "the Grover marked state |01111>",
    notes = Vector("This expands to more than 33 million paths per circuit copy.")
  )

  val grover5Iteration3: ExperimentSpec = grover8(
    alias = "grover-5-r3",
    description =
      "eight-qubit Grover search for 01111 with five search qubits, three ancillas, and three iterations",
    iterations = 3,
    instances = 2,
    correctnessDescription = "the Grover marked state |01111>",
    notes = Vector("This expands to more than 34 billion paths per circuit copy.")
  )

  val simonN3: ExperimentSpec =
    simon(
      "simon-n3",
      inputQubits = 3,
      instances = 3,
      paperTotal = Some(BigInt(192))
    )

  val simonN5: ExperimentSpec =
    simon(
      "simon-n5",
      inputQubits = 5,
      instances = 5,
      paperTotal = Some(BigInt(5120))
    )

  val simonN15: ExperimentSpec =
    simon(
      "simon-n15",
      inputQubits = 15,
      instances = 7,
      paperTotal = None
    ).copy(
      notes = Vector(
        "Seven copies have a maximum per-state amplitude of 7/128; choose a threshold at or below 0.0546875 or override the copy count."
      )
    )

  val shorN15: ExperimentSpec = ExperimentSpec(
    alias = "shor-n15",
    description = "Shor period-finding circuit for N=15",
    qubitCount = 8,
    initialState = zeroState(8),
    gates = List[Gate](
      H(0), H(1), H(2), H(3), X(0), X(1), X(2), X(3),
      ModularExponentiation(2, 15, Vector(0, 1, 2, 3), Vector(4, 5, 6, 7))
    ) ++ inverseQft(Vector(0, 1, 2, 3)),
    correctOutcomes = shorFrequencyOutcomes(
      countingQubits = 4,
      period = 4,
      description = "reference nonzero phase-estimation bins for ord_15(2) = 4"
    ),
    shorPostProcessing = Some(
      ShorPostProcessing(
        modulus = 15,
        base = 2,
        countingQubits = 4,
        maxIntermediateConvergents = ExperimentConfig.shorMaxIntermediateConvergents,
        maxDenominatorMultiple = ExperimentConfig.shorMaxDenominatorMultiple
      )
    ),
    // 10 * 2^8 = 2,560, matching Table II.
    defaultInstances = 10,
    paperTerminalContributions = Some(BigInt(2560))
  )

  private val shorN21CountingQubits = 16
  private val shorN21WorkQubits = 5
  private val shorN21PostProcessing = ShorPostProcessing(
    modulus = 21,
    base = 2,
    countingQubits = shorN21CountingQubits,
    maxIntermediateConvergents = ExperimentConfig.shorMaxIntermediateConvergents,
    maxDenominatorMultiple = ExperimentConfig.shorMaxDenominatorMultiple
  )

  val shorN21: ExperimentSpec = ExperimentSpec(
    alias = "shor-n21",
    description = "21-qubit Shor period-finding circuit for N=21",
    qubitCount = shorN21CountingQubits + shorN21WorkQubits,
    initialState = zeroState(shorN21CountingQubits + shorN21WorkQubits),
    gates =
      (0 until shorN21CountingQubits).map(H).toList ++
        List[Gate](
          ModularExponentiation(
            2,
            21,
            (0 until shorN21CountingQubits).toVector,
            (shorN21CountingQubits until
              shorN21CountingQubits + shorN21WorkQubits).toVector
          )
        ) ++
        inverseQft((0 until shorN21CountingQubits).toVector),
    correctOutcomes = shorFrequencyOutcomes(
      countingQubits = shorN21CountingQubits,
      period = 6,
      description = "reference nonzero nearest phase-estimation bins for ord_21(2) = 6"
    ),
    shorPostProcessing = Some(shorN21PostProcessing),
    defaultInstances = 6,
    paperTerminalContributions = Some(BigInt(10485760)),
    analyticalLeafMetrics = Some(
      analyticalShorLeafMetrics(shorN21PostProcessing)
    ),
    analyticalReferenceMetrics = Some(
      AnalyticalCircuitReference(
        fullInterferenceContributionsPerCopy =
          BigInt(1) << (2 * shorN21CountingQubits),
        fullInterferenceEndpointStates =
          (1 << shorN21CountingQubits) * 6,
        idealOutputProbabilities =
          periodFindingDistribution(shorN21CountingQubits, period = 6)
      )
    ),
    requireAllQubitsUsed = true,
    notes = Vector(
      "Qubits 0-15 are the counting register and qubits 16-20 store 2^x mod 21.",
      "Six copies are required for a saturated endpoint amplitude to exceed a threshold of 0.9."
    )
  )

  val all: Vector[ExperimentSpec] = Vector(
    deutschJozsa,
    grover4Tagged,
    grover3Sat,
    grover3Sat3QApprox,
    grover3Sat30,
    grover5Iteration1,
    grover5Iteration2,
    grover5Iteration3,
    simonN3,
    simonN5,
    simonN15,
    shorN15,
    shorN21
  )

  private val byAlias = all.map(spec => spec.alias -> spec).toMap
  require(byAlias.size == all.size, "Experiment aliases must be unique")
  private def normalizedAlias(alias: String): String =
    alias.filter(_.isLetterOrDigit).toLowerCase(java.util.Locale.ROOT)
  private val byNormalizedAlias =
    all.groupBy(spec => normalizedAlias(spec.alias))
  require(
    byNormalizedAlias.values.forall(_.size == 1),
    "Experiment aliases must remain unique after removing punctuation and case"
  )
  validateGrover8Oracle()
  validateGrover30Oracle()
  all.foreach(_.validate())

  def apply(alias: String): ExperimentSpec =
    byAlias
      .get(alias)
      .orElse(byNormalizedAlias.get(normalizedAlias(alias)).flatMap(_.headOption))
      .getOrElse {
        throw new IllegalArgumentException(
          s"Unknown circuit alias '$alias'. Available aliases: ${aliases.mkString(", ")}"
        )
      }

  val aliases: Vector[String] = all.map(_.alias)
}
