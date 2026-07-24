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

final case class SimonPostSelection(
    inputQubits: Int,
    possibleOracleOutputs: Vector[Vector[Boolean]]
)

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
    simonPostSelection: Option[SimonPostSelection] = None,
    shorPostProcessing: Option[ShorPostProcessing] = None,
    resultQubits: Option[Vector[Int]] = None,
    paperTerminalContributions: Option[BigInt] = None,
    markedAmplitudePerInstance: Option[Double] = None,
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
    val terminalWidth = simonPostSelection
      .map(_.inputQubits)
      .orElse(resultQubits.map(_.length))
      .getOrElse(qubitCount)
    require(
      correctOutcomes.observedQubits.distinct.length == correctOutcomes.observedQubits.length &&
        correctOutcomes.observedQubits.forall(q => q >= 0 && q < terminalWidth),
      s"$alias has invalid correctness-check qubit indices"
    )

    simonPostSelection.foreach { selection =>
      require(
        selection.inputQubits * 2 == qubitCount,
        s"$alias Simon post-selection does not match its register size"
      )
      require(
        selection.possibleOracleOutputs.nonEmpty &&
          selection.possibleOracleOutputs.forall(_.length == selection.inputQubits),
        s"$alias has an invalid set of Simon oracle outputs"
      )
    }
    shorPostProcessing.foreach { postProcessing =>
      require(
        postProcessing.countingQubits == correctOutcomes.observedQubits.length,
        s"$alias Shor post-processing width does not match its observed register"
      )
      require(
        simonPostSelection.isEmpty,
        s"$alias cannot use Simon post-selection and Shor post-processing together"
      )
    }

    val metrics = leafMetrics
    require(metrics.total > 0, s"$alias must create at least one terminal leaf")
    require(
      metrics.correct >= 0 && metrics.correct <= metrics.total,
      s"$alias has invalid leaf metrics: $metrics"
    )
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

  // The first eight clauses are unit clauses written in valid 3-CNF form as
  // (l ∨ l ∨ l), and are compiled to the equivalent single-literal circuit.
  // Together they force the unique assignment |01111111>. The final two
  // clauses are redundant genuine three-literal clauses; they exercise both
  // dedicated clause-work ancillas without changing the unique solution.
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

  private def simon(alias: String, inputQubits: Int, instances: Int, paperTotal: Int): ExperimentSpec = {
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

    ExperimentSpec(
      alias = alias,
      description = s"Simon period finding with n=$inputQubits and secret 11...1",
      qubitCount = inputQubits * 2,
      initialState = zeroState(inputQubits * 2),
      gates = input.map(H).toList ++ oracle ++ input.map(H).toList,
      correctOutcomes = outcomes(
        inputQubits,
        allStates(inputQubits).filter(state => state.count(identity) % 2 == 0),
        "Simon outputs y satisfying y dot 11...1 = 0 modulo 2"
      ),
      defaultInstances = instances,
      simonPostSelection = Some(SimonPostSelection(inputQubits, possibleOutputs)),
      paperTerminalContributions = Some(BigInt(paperTotal))
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

  val grover3Sat: ExperimentSpec = ExperimentSpec(
    alias = "grover-3sat",
    description = "3-SAT Grover circuit used by the paper implementation",
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
    paperTerminalContributions = Some(BigInt(262144)),
    notes = Vector(
      "This is the three-logical-qubit approximation kept in the original source; the paper reports eight physical qubits.",
      "Rotate(4) and Rotate(-4) are evaluated as exact unitary T and T-dagger phase gates."
    )
  )

  val grover5Iteration1: ExperimentSpec = taggedGrover(
    alias = "grover-5-r1",
    marked = "01111",
    iterations = 1,
    instances = 2
  )

  val grover5Iteration2: ExperimentSpec = taggedGrover(
    alias = "grover-5-r2",
    marked = "01111",
    iterations = 2,
    instances = 2,
    notes = Vector("This expands to more than 33 million paths per circuit copy.")
  )

  val grover5Iteration3: ExperimentSpec = taggedGrover(
    alias = "grover-5-r3",
    marked = "01111",
    iterations = 3,
    instances = 2,
    notes = Vector("This expands to more than 34 billion paths per circuit copy.")
  )

  val simonN3: ExperimentSpec =
    simon("simon-n3", inputQubits = 3, instances = 3, paperTotal = 192)

  val simonN5: ExperimentSpec =
    simon("simon-n5", inputQubits = 5, instances = 5, paperTotal = 5120)

  val shorN15: ExperimentSpec = ExperimentSpec(
    alias = "shor-n15",
    description = "Shor period-finding circuit for N=15",
    qubitCount = 8,
    initialState = zeroState(8),
    gates = List(
      H(0), H(1), H(2), H(3), X(0), X(1), X(2), X(3),
      ModularExponentiation(2, 15, Vector(0, 1, 2, 3), Vector(4, 5, 6, 7)),
      H(0), CRotate(0, -2, 1), Swap(0, 1), CRotate(1, -4, 2), Swap(1, 2),
      H(1), CRotate(2, -8, 3), Swap(2, 3), CRotate(1, -2, 2), Swap(1, 2),
      CRotate(2, -4, 3), Swap(2, 3), H(2), CRotate(2, -2, 3), Swap(2, 3), H(3)
    ),
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

  val shorN21: ExperimentSpec = ExperimentSpec(
    alias = "shor-n21",
    description = "compiled Shor period-finding circuit for N=21",
    qubitCount = 8,
    initialState = zeroState(8),
    gates = List(
      H(0), H(1), H(2), CX(1, 5), H(3), CX(1, 7), CX(2, 5), CX(0, 7),
      CX(5, 3), RZ(-4, 3), CX(7, 3), RZ(4, 3), CX(5, 3), RZ(-4, 3), RZ(4, 5),
      CX(7, 3), CX(7, 5), RZ(4, 3), RZ(-4, 5), RZ(4, 7),
      H(3), CX(7, 5), CX(0, 7), CX(1, 7), H(7), CX(3, 7), RZ(-4, 7), CX(0, 7),
      RZ(4, 7), CX(3, 7), RZ(4, 3), RZ(-4, 7), CX(0, 7), CX(0, 3), RZ(4, 7),
      RZ(4, 0), RZ(-4, 3), H(7), CX(0, 3), CX(3, 5), CX(7, 5), CX(0, 5), X(7),
      CX(2, 7), CX(1, 7), CX(0, 7), CX(0, 2), CX(2, 0), CX(0, 2), H(0), RZ(-4, 0),
      CX(0, 1), RZ(4, 1), CX(0, 1), RZ(-8, 0), RZ(-4, 1), CX(0, 2), H(1),
      RZ(8, 2), RZ(-4, 1), CX(0, 2), RZ(-8, 2), CX(1, 2), RZ(4, 2),
      CX(1, 2), RZ(-4, 2), H(2)
    ),
    correctOutcomes = shorFrequencyOutcomes(
      countingQubits = 4,
      period = 6,
      description = "reference nonzero nearest phase-estimation bins for ord_21(2) = 6"
    ),
    shorPostProcessing = Some(
      ShorPostProcessing(
        modulus = 21,
        base = 2,
        countingQubits = 4,
        maxIntermediateConvergents = ExperimentConfig.shorMaxIntermediateConvergents,
        maxDenominatorMultiple = ExperimentConfig.shorMaxDenominatorMultiple
      )
    ),
    defaultInstances = 5,
    paperTerminalContributions = Some(BigInt(10485760))
  )

  val all: Vector[ExperimentSpec] = Vector(
    deutschJozsa,
    grover4Tagged,
    grover3Sat,
    grover3Sat30,
    grover5Iteration1,
    grover5Iteration2,
    grover5Iteration3,
    simonN3,
    simonN5,
    shorN15,
    shorN21
  )

  private val byAlias = all.map(spec => spec.alias -> spec).toMap
  require(byAlias.size == all.size, "Experiment aliases must be unique")
  validateGrover30Oracle()
  all.foreach(_.validate())

  def apply(alias: String): ExperimentSpec =
    byAlias.getOrElse(
      alias,
      throw new IllegalArgumentException(
        s"Unknown circuit alias '$alias'. Available aliases: ${aliases.mkString(", ")}"
      )
    )

  val aliases: Vector[String] = all.map(_.alias)
}
