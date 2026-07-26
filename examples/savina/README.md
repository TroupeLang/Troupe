# Savina benchmarks ported to Troupe

The complete 30-benchmark Savina actor suite (Imam & Sarkar, AGERE 2014)
ported to Troupe. Each benchmark is a program-relative module under
`benchmarks/`, with its own message-protocol datatype, exporting a
`descriptor` (name, benchmark function, correctness check, default
sizes), built on the core actor primitives (`spawn`, `send`, `receive`,
`self`). `Savina.trp` imports those 30 modules and re-exports `all`, the
descriptor list in canonical suite order; `runall.trp` and each thin
per-benchmark driver import it with `import "./Savina"`. The whole import
graph is recompiled on every run, so there is no artifact to publish, but
each program's `<main>.deps.json` pins its modules by content hash and
has to be re-pinned with `make benchmark-deps` after a module or compiler
change — a stale pin is a compile error. Each
benchmark file here is a thin driver selecting one descriptor, with that
benchmark's semantics, deviations, and check documented in its header
comment.

## Running the whole evaluation

```bash
./local.sh examples/savina/runall.trp --io-root out              # all 30
./local.sh examples/savina/runall.trp --io-root out -- sieve uct # a subset
```

Nothing needs publishing first: the descriptors (`Savina.trp`) and the shared
report renderer (`SavinaReport.trp`) are both program-relative modules,
recompiled with the whole import graph on every run. The renderer is shared
across suites in different directories through an upward path (e.g.
`import "../../savina/SavinaReport"`), which content-addressed identity makes
sound — a module's identity is the hash of its code, not its location.

`runall.trp` runs the selected benchmarks, prints measurement lines as
it goes, and writes `savina-results.txt`, `savina-report.md`, and
`savina-charts.svg` (small-multiple scaling charts) under the io-root —
the whole pipeline, including chart rendering, is Troupe
(`SavinaReport.trp`). `analysis/mkreport.trp` regenerates the report
and charts from a previously collected results file.

## Running one benchmark

```bash
./local.sh examples/savina/pingpong.trp              # default sizes
./local.sh examples/savina/pingpong.trp -- 5000      # explicit sizes
```

Each benchmark runs one untimed warmup plus three timed repetitions per size
and prints one line per repetition:

```
pingpong n=5000 rep=0 ms=470 ok=true
```

`ok` reports the benchmark's correctness check (documented in each file's
header comment); a run with `ok=false` is a bug, not a data point. Timing
uses `getTime` (millisecond precision). Absolute numbers reflect a monitored
runtime with per-operation information-flow tracking and are not competitive
with unmonitored actor runtimes; the value of the measurements is relative
scaling behavior.

## Benchmarks (tranche 1 — spanning subset)

Ported first to establish the conventions and surface runtime gaps.

| File                | Savina benchmark          | Group          | Size parameter        | Check                              |
|---------------------|---------------------------|----------------|-----------------------|-------------------------------------|
| `pingpong.trp`      | Ping Pong                 | microbenchmark | round trips           | pong count = n                     |
| `counting.trp`      | Counting Actor            | microbenchmark | messages              | count = n                          |
| `threadring.trp`    | Thread Ring               | microbenchmark | token hops (R = 100)  | final holder = n mod R             |
| `fibonacci.trp`     | Fibonacci                 | microbenchmark | n (2*fib(n)-1 actors) | value = iterative reference        |
| `big.trp`           | Big                       | concurrency    | workers (100 pings each) | total pongs = 100w              |
| `philosophers.trp`  | Dining Philosophers       | concurrency    | rounds per philosopher (P = 20) | total eaten = 20n        |
| `prodcons.trp`      | Producer-Consumer         | concurrency    | items per producer (4+4, capacity 10) | checksum: exactly-once |
| `trapezoid.trp`     | Trapezoidal Approximation | parallelism    | trapezoids (8 workers) | integral of 4/(1+x^2) within 1e-6 of pi |

Deviations from the originals are documented in each file's header comment
(e.g. `trapezoid.trp` substitutes an integrand computable with Troupe's
math built-ins).

## Benchmarks (tranche 2 — portable messaging)

| File            | Savina benchmark               | Group          | Size parameter               | Check |
|-----------------|--------------------------------|----------------|------------------------------|-------|
| `fjthrput.trp`  | Fork Join (throughput)         | microbenchmark | messages per worker (8 workers) | counts sum to 8n          |
| `fjcreate.trp`  | Fork Join (actor creation)     | microbenchmark | actors spawned               | replies sum to n              |
| `chameneos.trp` | Chameneos                      | microbenchmark | meetings (10 creatures)      | meeting counts sum to 2n      |
| `cigsmok.trp`   | Cigarette Smokers              | concurrency    | rounds (3 smokers)           | smoke counts sum to n         |
| `barber.trp`    | Sleeping Barber                | concurrency    | customers (room capacity 4)  | served = n                    |
| `concdict.trp`  | Concurrent Dictionary          | concurrency    | ops per worker (10 workers)  | read-after-write per worker; server op count |
| `concsll.trp`   | Concurrent Sorted Linked List  | concurrency    | ops per worker (8 workers)   | snapshot sorted; length = novel inserts |
| `sieve.trp`     | Sieve of Eratosthenes          | parallelism    | candidate limit              | prime count = reference table |
| `quicksort.trp` | Quicksort                      | parallelism    | list length                  | sorted, length and sum preserved |

## Benchmarks (tranche 3 — pipelines and master/worker)

| File              | Savina benchmark           | Group       | Size parameter            | Check |
|-------------------|----------------------------|-------------|---------------------------|-------|
| `radixsort.trp`   | Radix Sort                 | parallelism | values (16 bit-actors)    | sink: sorted, count, checksum |
| `filterbank.trp`  | Filter Bank                | parallelism | samples (4 FIR branches)  | total matches sequential reference |
| `bitonicsort.trp` | Bitonic Sort               | parallelism | list length (power of 2)  | equals List.sort element-wise |
| `facloc.trp`      | Online Facility Location   | parallelism | points streamed           | counts conserve; all points covered by a facility |
| `nqueenk.trp`     | N-Queens (all solutions)   | parallelism | board size                | total = known solution count |

## Benchmarks (tranche 4 — synchronous-emulation, priority, and array-shaped)

With tranche 4 the port covers the complete 30-benchmark Savina suite.
The array-shaped benchmarks use `lib/Vector.trp` (immutable vectors with
logarithmic indexed access — Troupe has no constant-time arrays; each
file documents the deviation), and `piprecision.trp` uses `lib/BigInt.trp`.

| File               | Savina benchmark               | Group       | Size parameter          | Check |
|--------------------|--------------------------------|-------------|-------------------------|-------|
| `logmap.trp`       | Logistic Map Series            | concurrency | terms (10 series)       | sum equals sequential reference exactly |
| `banking.trp`      | Bank Transaction               | concurrency | transfers (20 accounts) | money conserved; all transfers complete |
| `astar.trp`        | A-star Search                  | parallelism | grid width (4 workers)  | distance equals sequential reference |
| `uct.trp`          | Unbalanced Cobwebbed Tree      | parallelism | tree nodes              | allocation/completion conserve; all urgent probes answered |
| `recmatmul.trp`    | Recursive Matrix Multiplication | parallelism | matrix dimension       | equals sequential product |
| `apsp.trp`         | All-Pairs Shortest Path        | parallelism | vertices (6 workers)    | equals sequential Floyd-Warshall |
| `sor.trp`          | Successive Over-Relaxation     | parallelism | grid width (actor per cell) | exact float equality with sequential Jacobi |
| `piprecision.trp`  | Precise Pi Computation         | parallelism | decimal digits (max 100) | digits match built-in pi reference |

`uct.trp` demonstrates the priority-receive emulation (a self-sent marker
behind FIFO delivery serves queued urgent messages without blocking);
`logmap.trp` and `banking.trp` note that synchronous rendezvous is the
ordinary ask pattern in Troupe, unlike the Akka originals.

## Labeled variants (IFC dimension)

Three benchmarks have information-flow variants with no counterpart in the
original suite. Each checks its security property at run time with
`levelOf`/`flowsTo` and prints the observed labels; they demonstrate what
Troupe adds over label-free actor runtimes.

| File                     | Demonstrates                                                        |
|--------------------------|---------------------------------------------------------------------|
| `pingpong-labeled.trp`   | A confidential payload taints the reply derived from it — exactly (`{alice}` in, `{alice}` out) — while a public control payload stays public. |
| `prodcons-labeled.trp`   | Per-producer provenance survives buffering: the consumer's sum is labeled exactly `{alice,bob}`, the join of the producers' labels. |
| `counting-labeled.trp`   | A secret-dependent *send* leaks through message presence; counting such messages requires raised mailbox clearance and a ranged receive (`rcv`), and the count comes out tainted. A public-pc `receive` cannot even observe the message. |

`counting-labeled.trp` prints an expected runtime warning about mailbox
clearance not being restored at process exit; see its header comment.
