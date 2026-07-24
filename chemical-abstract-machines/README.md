# Approximate path-sum experiments

This repository implements the experiments from
[Half the Interference, Most of the Answer](https://arxiv.org/abs/2606.01922).
The runnable experiments live in one catalog and use one entry point.

## Select and run an experiment

Edit this variable in
[`ExperimentConfig.scala`](src/main/scala/ExperimentConfig.scala):

```scala
val circuitAlias: String = "grover-4-tagged"
```

Then run:

```text
sbt run
```

There is intentionally no circuit command-line argument. The threshold,
instance override, worker count, scheduling jitter, and random seed are in the
same config file so a source revision records the complete experiment setup.
Set `preflightOnly` to `true` to print the chosen copy count and path estimate
without emitting any circuit molecules.

The selected state is printed in a prominent banner and saved under
`target/experiments/<alias>-sampled-state.txt`. The file also records the number
of generated terminal contributions, CHAM interference reactions, the four
`B_*` leaf metrics, correctness, and elapsed time at selection.

## Circuit aliases

| Alias | Experiment | Copies | Backend |
|---|---|---:|---|
| `deutsch-jozsa` | two-qubit Deutsch-Jozsa example | 1 | CHAM |
| `grover-4-tagged` | four-qubit tagged-state Grover | automatic, 1–2 | CHAM |
| `grover-3sat` | paper implementation's 3-SAT circuit | 1 | CHAM |
| `grover-3sat-30` | 8 search qubits plus 22 active oracle ancillas | automatic, 1–5 | CHAM |
| `grover-5-r1` | five search qubits, marked `01111`, one iteration | automatic, 1–2 | CHAM |
| `grover-5-r2` | same circuit, two iterations | automatic, 1–2 | CHAM |
| `grover-5-r3` | same circuit, three iterations | automatic, 1 | CHAM |
| `simon-n3` | Simon with a 6-qubit register | 3 | CHAM |
| `simon-n5` | Simon with a 10-qubit register | 5 | CHAM |
| `shor-n15` | Shor period finding for 15 | 10 | CHAM |
| `shor-n21` | Shor period finding for 21 | 5 | CHAM |

The multiple copies are deliberate. Ready endpoint molecules from every copy
share the same CHAM pool, allowing their amplitudes to interfere and accumulate
past thresholds that one normalized circuit cannot reach. Molecules that still
have gates are tagged separately as `commit` molecules, so they cannot
participate in endpoint reactions or be sampled.

The old Qure rotation names now have exact unitary implementations. A signed
denominator `d` represents an angle `pi / d`: `Rotate` is a phase gate,
`RZ` is a Z-axis rotation, and negative denominators are their inverses.
`CRotate` retains the historical half-angle controlled-phase convention used by
the compiled circuits. The previous hard-coded decimal approximations and the
non-unitary `0.25`/`4.0` amplitude scaling have been removed.

For tagged Grover aliases, leaving `instanceCountOverride` as `None` chooses the
minimum number of copies whose final marked-state amplitude can reach the
configured threshold. Set `Some(n)` to reproduce a fixed-copy experiment.

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
| `grover-3sat` | 32,768 | 4,096 |
| `grover-3sat-30` | 16,777,216 | 65,536 |
| `grover-5-r1` | 32,768 | 1,024 |
| `grover-5-r2` | 33,554,432 | 1,048,576 |
| `grover-5-r3` | 34,359,738,368 | 1,073,741,824 |
| `simon-n3` | 64 | 32 |
| `simon-n5` | 1,024 | 512 |
| `shor-n15` | 256 | 112 |
| `shor-n21` | 1,024 | 448 |

The values printed and persisted for an experiment run are multiplied by the
actual number of circuit copies. `B_active` is the cumulative number of
terminal molecules admitted to the ready/interference stage when the threshold
is first crossed. `B_activecorrect` is its correct-outcome subset. Both counters
are updated and snapshotted under the same lock as threshold selection, so they
describe that selection boundary rather than the later CHAM shutdown state.
Simon paths rejected by the configured post-selection are not marked active,
but remain part of the full-simulation `B_total`/`B_correct` counts.

The old `paperTerminalContributions` field is retained only as a paper-reference
diagnostic. It is not substituted for these exact counts and can differ when
the catalog's executable circuit differs from an older paper circuit.

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
| `grover-3sat` | all 3 output bits | `{011}` |
| `grover-3sat-30` | 8-bit search output | `{01111111}` |
| `grover-5-r1`, `grover-5-r2`, `grover-5-r3` | all 5 output bits | `{01111}` |
| `simon-n3` | 3-bit post-selected input register | `{000, 011, 101, 110}` |
| `simon-n5` | 5-bit post-selected input register | all 16 even-parity strings |
| `shor-n15` | first 4 counting bits | construct non-trivial factors `3 x 5` |
| `shor-n21` | first 4 counting bits | construct non-trivial factors `3 x 7` |

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

The paper's reference frequency bins remain in the catalog as diagnostics:
`{0100, 1000, 1100}` for `N=15`, and
`{0011, 0101, 1000, 1011, 1101}` for `N=21`. They no longer directly determine
correctness. With the configured bounds, the complete post-processable 4-bit
sets are:

| Alias | Post-processable sampled states |
|---|---|
| `shor-n15` | `{0011, 0100, 0101, 1000, 1011, 1100, 1101}` |
| `shor-n21` | `{0011, 0101, 0111, 1000, 1001, 1011, 1101}` |

The result file also records the phase estimate, rational candidates, tested
exponents, recovered order, factor pair, search bounds, and post-processing
message. This makes successful and failed Shor classifications reproducible.

## Repeated trials and statistical output

Repeated experiments are also configured in `ExperimentConfig.scala`, without
program arguments:

```scala
val repeatedTrialCount: Int = 50
val repeatedTrialThresholds: Vector[Double] = Vector(threshold)
val repeatedTrialJvmMaxHeap: String = "32G"
val repeatedTrialBatchLabel: Option[String] = None
```

Run the configured batch with:

```text
sbt "runMain com.sinanspd.RepeatedExperimentRunner"
```

Each trial starts `Cham2` in a fresh JVM, so no CHAM pool, scheduler, counters,
or sampled-state recorder can leak between trials. Trial indices use matched
random seeds across thresholds, which makes threshold comparisons paired. A
batch is written below
`target/experiments/batches/<alias>-<UTC timestamp>/`:

- `trials.csv` has one row per trial and threshold, including the sampled
  state, correctness, all four `B_*` values, normalized ratios, seed, timing,
  reaction counts, process exit status, and complete Shor diagnostics;
- `summary.csv` reports the count, mean, sample standard deviation, and 95%
  confidence interval for correctness, leaf metrics, reduction ratios, and
  runtime. Correctness uses a Wilson binomial interval; continuous metrics use
  a Student-t interval;
- `pairwise-comparisons.csv` reports paired t-tests and Wilcoxon signed-rank
  tests for `B_active` versus `B_total`, `B_activecorrect` versus `B_correct`,
  and matched continuous metrics across configured thresholds. Cross-threshold
  correctness uses an exact McNemar test;
- `trials/` contains the complete machine-readable result from every run, and
  `logs/` contains its full process output.

The runner warns when fewer than the reviewer's requested 50 trials are
configured. Failed or missing child results remain visible in `trials.csv` and
are excluded from summary and hypothesis-test calculations.
