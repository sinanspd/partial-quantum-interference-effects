# Approximate path-sum experiments

This repository implements the experiments from
[Half the Interference, Most of the Answer](https://arxiv.org/abs/2606.01922).
The runnable experiments live in one catalog and can use either fully-CHAM or
stream-to-CHAM execution.

## Select and run an experiment

Edit this variable in
[`ExperimentConfig.scala`](src/main/scala/ExperimentConfig.scala):

```scala
val circuitAlias: String = "grover-4-tagged"
val outcomeSelectionMode: OutcomeSelectionMode = BornRuleSampling
```

For fully-CHAM path generation and endpoint interference, run:

```text
sbt run
```

For bounded FS2 path generation with CHAM used only for endpoint interference,
run:

```text
sbt "runMain com.sinanspd.Main"
```

Both entry points use the same catalog, threshold and instance selection,
Simon post-selection, Shor post-processing, sampled-state recorder, and `B_*`
metrics. `Main` is the higher-throughput backend: it builds at most a
worker-sized prefix frontier, lazily streams the remaining paths, and emits
only completed terminal molecules into CHAM. The old hard-coded circuits and
unbounded recursive path expansion previously in `Main` have been removed.

There is intentionally no circuit command-line argument. The threshold,
instance override, worker count, scheduling jitter, and random seed are in the
same config file so a source revision records the complete experiment setup.
Set `preflightOnly` to `true` to print the chosen copy count and path estimate
without emitting any circuit molecules.

Canonical aliases are shown below. Selection is tolerant of case and
punctuation, so `grover3Sat`, `GROVER_3SAT`, and `grover-3sat` resolve to the
same catalog entry. The repeated runner resolves the alias before it creates a
batch or starts child JVMs, so an unknown name now fails once with the available
alias list instead of producing a batch of missing trials.

The streaming backend can add a small, FS2-only timing variance to its bounded
frontier branches:

```scala
val fs2BranchJitterMillis: Int = 5
val fs2BranchJitterSeedOverride: Option[Long] = None
```

Each frontier branch waits a uniformly selected integer delay from zero
through `fs2BranchJitterMillis` before traversing the rest of its circuit.
This changes the order in which completed endpoints reach CHAM without adding
a sleep to every generated leaf. `None` chooses a fresh seed for each
experiment JVM; `Some(seed)` reproduces the delay schedule. The configured
maximum, chosen seed, and realized minimum, maximum, and mean delays are
persisted in every streaming result. 

## Circuit aliases

| Alias | Experiment | Copies | Backend |
|---|---|---:|---|
| `deutsch-jozsa` | two-qubit Deutsch-Jozsa example | 1 | both |
| `grover-4-tagged` | four-qubit tagged-state Grover | automatic, 1–2 | both |
| `grover-3sat` | 5 search qubits plus 3 active work ancillas, one iteration | automatic, 1–2 | both |
| `grover-3sat-3q-approx` | legacy three-qubit compiled approximation | 1 | both |
| `grover-3sat-30` | 8 search qubits plus 22 active oracle ancillas | automatic, 1–5 | both |
| `grover-5-r1` | same 8-qubit layout, marked `01111`, one iteration | automatic, 1–2 | both |
| `grover-5-r2` | same 8-qubit layout, two iterations | automatic, 1–2 | both |
| `grover-5-r3` | same 8-qubit layout, three iterations | automatic, 1 | both |
| `simon-n3` | Simon with a 6-qubit register | 3 | both |
| `simon-n5` | Simon with a 10-qubit register | 5 | both |
| `simon-n15` | Simon with a 30-qubit register | 7 | both |
| `shor-n15` | Shor period finding for 15 | 10 | both |
| `shor-n21` | 21-qubit Shor period finding for N=21 | 6 | both |



All backends and static/reference metrics use one shared terminal-state policy.
Ordinary Grover circuits retain the complete terminal register; the 30-qubit
Grover circuit projects its fully uncomputed ancillas away without changing
amplitudes. Simon conditions on one oracle-output measurement and applies the
general post-selection rule `alpha_conditioned = alpha / sqrt(P(outcome))`.
The catalog describes the possible outcomes and their probabilities, so the
normalization is derived rather than hard-coded by circuit alias or register
size. For these uniform Simon oracles the factor is
`sqrt(number of oracle outcomes)`: 2 at `n=3`, 4 at `n=5`, and 128 at `n=15`.
The `simon-n15` alias preserves the exploratory 30-qubit variant separately;
its seven configured copies can only develop a per-state amplitude of
`7 / 128`, so use an attainable threshold or explicitly increase its copy
count.


For tagged Grover aliases, leaving `instanceCountOverride` as `None` chooses the
minimum number of copies whose final marked-state amplitude can reach the
configured threshold. Set `Some(n)` to reproduce a fixed-copy experiment.

## The corrected 8-qubit Grover experiment

`grover-3sat` and `grover-5-r1` through `grover-5-r3` now execute with the
physical layout described for the larger Grover experiment:

- qubits 0–4 are the five search qubits;
- qubits 5–7 are active clean work ancillas;
- the three ancillas decompose the five-controlled oracle and diffusion phase
  operations into `CCX` and `CZ` gates;
- every phase operation uncomputes all three ancillas to zero;
- the sampled terminal state is projected onto qubits 0–4.

The 3-SAT alias uses a nine-clause 3-CNF with three distinct variables in every
clause and the unique satisfying assignment `01111`. At catalog startup, all
32 search inputs are checked against the formula and the compiled reversible
oracle. The validation requires the oracle to flip the phase of exactly the
satisfying assignment and restore every ancilla for every input.

The legacy `groverSAT011` circuit remains available as
`grover-3sat-3q-approx`. It is no longer labeled as the paper's eight-qubit
circuit. The historical Table I values `B_total=262,144` and
`B_correct=32,768` are not forced into the new circuit: they do not equal the
leaf counts of one five-search-plus-three-ancilla Grover circuit under the
current definitions. Counts are instead derived from the gates that actually
run.

## The 30-qubit experiment

`grover-3sat-30` is a literal CHAM circuit, not an analytical shortcut:

- qubits 0–7 are the search register;
- qubits 8–17 store ten 3-SAT clause results;
- qubits 18–19 are clause-work ancillas;
- qubits 20–28 reversibly reduce the clause flags;
- qubit 29 applies the oracle phase;
- the unique satisfying assignment is `01111111`;
- the oracle is uncomputed, returning all 22 ancillas to zero before diffusion.
- sampled output is projected back to the eight search bits for a concise result.

The catalog exhaustively validates all 256 search inputs at startup. It checks
that only `01111111` receives a negative phase and that every ancilla is reset.
The formula is deliberately structured—eight repeated-literal unit clauses and
two redundant three-literal clauses—so this benchmarks simulator scaling and
interference rather than classical SAT-instance difficulty.

One Grover iteration contains 24 Hadamards and therefore creates
`2^24 = 16,777,216` terminal paths per copy. The automatic copy counts for the
paper thresholds are:

| Threshold | Copies | Maximum terminal paths |
|---:|---:|---:|
| 0.1 | 1 | 16,777,216 |
| 0.3 | 2 | 33,554,432 |
| 0.5 | 3 | 50,331,648 |
| 0.7 | 4 | 67,108,864 |
| 0.9 | 5 | 83,886,080 |

These are intentionally large runs. The actual selected count may be lower
because CHAM stops after the first ready molecule crosses the threshold.

## Leaf metrics

Every catalog entry calculates two exact, static pre-interference leaf counts:

- `B_total` is the number of terminal leaves a complete simulation creates;
- `B_correct` is the subset whose projected outcome passes that circuit's
  correctness evaluator.

A Hadamard creates two leaves. Basis-preserving and diagonal gates, including
`X`, controlled gates, `RZ`, `Rotate`, and `CRotate`, transform existing leaves
without creating new ones. The calculation propagates integer path
multiplicities through the actual circuit, rather than assuming the correct
states are uniformly represented. For Shor, a leaf is correct when classical
post-processing of its observed counting state recovers the non-trivial
factors.

The catalog's exact per-copy values are:

| Alias | `B_total` | `B_correct` |
|---|---:|---:|
| `deutsch-jozsa` | 8 | 4 |
| `grover-4-tagged` | 4,096 | 256 |
| `grover-3sat` | 32,768 | 1,024 |
| `grover-3sat-3q-approx` | 32,768 | 4,096 |
| `grover-3sat-30` | 16,777,216 | 65,536 |
| `grover-5-r1` | 32,768 | 1,024 |
| `grover-5-r2` | 33,554,432 | 1,048,576 |
| `grover-5-r3` | 34,359,738,368 | 1,073,741,824 |
| `simon-n3` | 64 | 8 |
| `simon-n5` | 1,024 | 32 |
| `simon-n15` | 1,073,741,824 | 32,768 |
| `shor-n15` | 256 | 112 |
| `shor-n21` | 4,294,967,296 | 1,114,112 |

`shor-n21` is a literal 21-qubit experiment: qubits 0–15 form the counting
register and qubits 16–20 store `2^x mod 21`. Its derived counts therefore
differ from the smaller circuit used for the paper's historical Table II row.

The values printed and persisted for an experiment run are multiplied by the
actual number of circuit copies. `B_active` is the cumulative number of
terminal molecules admitted to the ready/interference stage when the threshold
is first crossed. `B_activecorrect` is its correct-outcome subset. Both counters
are updated and snapshotted under the same lock as threshold selection, so they
describe that selection boundary rather than the later CHAM shutdown state.
Simon paths rejected by the configured post-selection are not marked active.
They remain part of `B_total`, while `B_correct` counts correct leaves admitted
by the selected terminal condition. The catalog verifies that this count is
independent of which valid post-selection outcome is chosen.

The old `paperTerminalContributions` field is retained only as a paper-reference
diagnostic. It is not substituted for these exact counts and can differ when
the catalog's executable circuit differs from an older paper circuit.

## Outcome-selection modes

`ExperimentConfig.outcomeSelectionMode` switches between two source-configured
modes:

- `FirstThresholdCrossing` preserves the original behavior. The first endpoint
  molecule whose magnitude reaches the threshold is selected.
- `BornRuleSampling` uses the first crossing only as the early-stop boundary.
  It adds that trigger to the tracked ready soup, coherently aggregates
  molecules by endpoint state, and draws from the resulting truncated state.

The Born mode does not assume that squared amplitudes sum to one. It calculates

```text
Z = sum_s |A_s|^2
p_s = |A_s|^2 / Z
```

and draws from the normalized `p_s` values. This handles probability mass above
or below one due to multiple circuit copies, early stopping, and truncation.
It is a Born-rule sample of the available approximate state, not a full-
simulation Born sample.

Every result persists the mode, threshold-trigger state and amplitude, `Z`,
`1/Z`, the normalized probability sum, the selected state's normalized
probability, the random draw and seed, and whether the trigger itself was
selected. `samplingPopulationBornDistribution` stores the complete normalized
truncated distribution used for the draw.

Endpoint interference reactions are guarded by endpoint bitstring. Molecules
from different states remain in the ready pool and are never consumed merely
to be re-emitted; this prevents the Chymyst scheduler from repeatedly selecting
the same incompatible pair. The streaming backend also observes normal FS2
completion. If generation finishes, every compatible endpoint pair has
reacted, and no molecule reached the threshold, the child exits with a
descriptive failed-trial error instead of waiting forever. It does not silently
sample at full completion because that would change the configured
first-threshold-crossing boundary.

## Sampling-boundary amplitude and reaction metrics

Both backends persist the following measurements at the same threshold
selection boundary as the `B_*` snapshot:

- `thresholdTriggerAmplitude.*` identifies the molecule that crossed the
  threshold. `developedAmplitudeMagnitude` and `amplitude.*` describe the
  state actually selected; in Born mode this is the available complex
  aggregate for the drawn endpoint and may differ from the trigger.
- `maxIncorrectReadyAmplitudeAtSampling` is the maximum magnitude among
  tracked incorrect ready molecules. Correctness uses the experiment's normal
  evaluator, including Shor factor recovery. It is zero when no incorrect
  ready molecule is tracked. `incorrectReadyMoleculesAtSampling` records the
  corresponding tracked population.
- `interferenceReactionsAtSampling` counts completed compatible endpoint
  combinations before sampling. It includes constructive and destructive
  interference but deliberately excludes encounters between molecules with
  different endpoint states. The older
  `selectedAtInterferenceReactions` field is retained as an equal-valued
  compatibility alias.
- `I_full` is a deterministic full-interference baseline. For every endpoint
  state with `B_s` terminal contributions admitted by the terminal policy, it
  counts `B_s - 1` compatible binary additions. Thus
  `I_full = sum_s(B_s - 1)`, independent of CHAM scheduling and cancellation
  order. `interferenceCompletionFraction` is the observed reaction count
  divided by this baseline, and `interferenceReductionFraction` is one minus
  that value.
- `selectedStateAggregateAmplitude.*` is the complex sum of the selected
  candidate and all tracked ready molecules with the same endpoint.
  `maxIncorrectReadyStateAggregateAmplitudeAtSampling`, the selected-versus-
  incorrect margin and ratio, and `selectedStateAmplitudeRank` compare whole
  endpoint states rather than individual molecules.
- `readyMoleculesAtSampling`, its correct/incorrect split, and the three
  distinct-endpoint counts describe the instantaneous tracked ready soup. They
  exclude the threshold trigger itself. The corresponding
  `samplingPopulation*` counts include it.

Chymyst 0.2.0 exposes its soup only as a string, which is unsuitable for
multi-million-molecule runs. The incorrect-amplitude metric therefore uses a
compact amplitude multiset updated whenever ready molecules are emitted or
consumed. Chymyst may dispatch reaction inputs immediately before the user
reaction body unregisters them, so the snapshot can conservatively include
such just-dispatched inputs. Result files explicitly record
`maxIncorrectReadyAmplitudeAtSamplingExact=false` and the full semantics in
`incorrectReadyPoolSnapshotSemantics`; the developed amplitude and interference
reaction count are exact at the application-level selection boundary. The
state-level and instantaneous-pool fields use the same shadow multiset and are
therefore explicitly marked `readyPoolStateSnapshotExact=false`. Born-rule
sampling uses that same instrumented population, so this limitation applies to
its truncated probability distribution as well.

## Exact output-distribution reference

Each run also calculates the exact full-state Born distribution for the
selected circuit. It applies Simon post-selection when present, marginalizes
onto the circuit's observed register, and normalizes the result. The complete
distribution is persisted in `idealOutputDistribution`, while
`idealSampledOutcomeProbability` records the exact probability of that trial's
sampled outcome.

The exact distribution is calculated with a compact state-vector dynamic
program for the modest circuits and a validated closed form for the much
larger Simon state spaces. Neither approach generates all path leaves, and
both are used only as measurement references; the experimental sample and
threshold behavior still come from the selected FS2/CHAM execution backend.

## Correct outcomes and Shor post-processing

For Deutsch-Jozsa, Grover, and Simon, correctness is direct membership in the
configured state set. For Shor, correctness is based on classical factor
recovery from the sampled counting-register value. After sampling, the runner
prints an `OUTCOME CHECK` banner and records the observed outcome, evaluation
method, and `isCorrect` result.

| Alias | Observed register | Success test |
|---|---|---|
| `deutsch-jozsa` | input qubit 0 | `{0}` |
| `grover-4-tagged` | all 4 output bits | `{0000}` |
| `grover-3sat` | five search qubits; three ancillas projected away | `{01111}` |
| `grover-3sat-3q-approx` | all 3 output bits | `{011}` |
| `grover-3sat-30` | 8-bit search output | `{01111111}` |
| `grover-5-r1`, `grover-5-r2`, `grover-5-r3` | all 5 output bits | `{01111}` |
| `simon-n3` | 3-bit post-selected input register | `{000, 011, 101, 110}` |
| `simon-n5` | 5-bit post-selected input register | all 16 even-parity strings |
| `simon-n15` | 15-bit post-selected input register | all 16,384 even-parity strings |
| `shor-n15` | first 4 counting bits | construct non-trivial factors `3 x 5` |
| `shor-n21` | first 16 counting bits | construct non-trivial factors `3 x 7` |

For Simon, valid strings satisfy `y dot 11...1 = 0 mod 2`.

For Shor, the runner applies this pipeline to the measured phase `y / 2^t`:

1. generate continued-fraction convergents and a bounded set of intermediate
   convergents within half a measurement bin;
2. try at most eight multiples of each reconstructed denominator;
3. verify candidate exponents using `a^r mod N = 1`;
4. for an even exponent, compute `gcd(a^(r/2) - 1, N)` and
   `gcd(a^(r/2) + 1, N)`;
5. set `isCorrect=true` only if the result contains two non-trivial factors
   whose product is `N`.

The intermediate-convergent and multiplier searches are deliberately bounded.
This recovers denominator-divisor cases without replacing the quantum result
with an exhaustive classical order search. Their limits are
`shorMaxIntermediateConvergents` and `shorMaxDenominatorMultiple` in
`ExperimentConfig.scala`.

The paper's reference frequency bins remain in the catalog as diagnostics.
They are `{0100, 1000, 1100}` for the four-counting-qubit `N=15` circuit.
For the 16-counting-qubit `N=21` circuit, the five nearest nonzero order-six
bins are `{0010101010101011, 0101010101010101, 1000000000000000,
1010101010101011, 1101010101010101}`. They no longer directly determine
correctness. With the configured bounds, the post-processable sets are:

| Alias | Post-processable sampled states |
|---|---|
| `shor-n15` | `{0011, 0100, 0101, 1000, 1011, 1100, 1101}` |
| `shor-n21` | 17 derived 16-bit bins |

The result file also records the phase estimate, rational candidates, tested
exponents, recovered order, factor pair, search bounds, and post-processing
message. This makes successful and failed Shor classifications reproducible.

## Repeated trials and statistical output

Repeated experiments are also configured in `ExperimentConfig.scala`, without
program arguments:

```scala
val repeatedTrialCount: Int = 50
val repeatedTrialThresholds: Vector[Double] = Vector(threshold)
val repeatedTrialBackend: TrialExecutionBackend = StreamingPathBackend
val repeatedTrialJvmMaxHeap: String = "32G"
val repeatedTrialTimeoutMinutes: Long = 24L * 60L
val distributionBootstrapReplicates: Int = 5000
val repeatedTrialBatchLabel: Option[String] = None
```

Use `StreamingPathBackend` to run `Main`, or `FullyChamBackend` to run `Cham2`.

Run the configured batch with:

```text
sbt "runMain com.sinanspd.RepeatedExperimentRunner"
```

Each trial starts the configured backend in a fresh JVM, so no CHAM pool,
scheduler, counters, stream fibers, or sampled-state recorder can leak between
trials. The runner prints the trial and log path before waiting. A child that
exceeds `repeatedTrialTimeoutMinutes` is terminated and retained as a failed
row; interrupting the runner also terminates its current child. Trial indices
use reproducible SplitMix64-style mixed seeds rather than adjacent Java
`Random` seeds. Each trial derives independent Born-rule and post-selection
streams, while the same trial index remains matched across thresholds so
threshold comparisons stay paired. A batch is written below
`target/experiments/batches/<alias>-<UTC timestamp>/`:

- `trials.csv` has one row per trial and threshold, including the sampled
  state, developed and maximum-incorrect amplitudes, correctness, all four
  `B_*` values, normalized ratios, seed, timing, compatible interference
  reaction counts, process exit status, and complete Shor diagnostics;
- `summary.csv` reports the count, mean, sample standard deviation, and 95%
  confidence interval for correctness, leaf metrics, reduction ratios, and
  runtime. Correctness uses a Wilson binomial interval; continuous metrics use
  a Student-t interval;
- `distribution-quality.csv` compares the empirical sampled-output
  distribution with the mean trial-specific exact reference distribution. It
  reports total variation distance, Hellinger distance/fidelity, and
  non-parametric paired-bootstrap standard deviations and 95% intervals;
- `distribution-details.csv` reports the empirical count and probability,
  exact reference probability, and difference for every observed outcome;
- `pairwise-comparisons.csv` reports paired t-tests and Wilcoxon signed-rank
  tests for `B_active` versus `B_total`, `B_activecorrect` versus `B_correct`,
  interference reactions versus `I_full`, and matched continuous metrics
  across configured thresholds. Cross-threshold correctness uses an exact
  McNemar test;
- `trials/` contains the complete machine-readable result from every run, and
  `logs/` contains its full process output.

The runner warns when fewer than the reviewer's requested 50 trials are
configured. Failed or missing child results remain visible in `trials.csv` and
are excluded from summary and hypothesis-test calculations.
