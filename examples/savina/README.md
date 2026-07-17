# Savina benchmarks ported to Troupe

Ports of benchmarks from the Savina actor-benchmark suite (Imam & Sarkar,
AGERE 2014) to Troupe, produced by the Savina port study (see
`_dev_planning/tier2-libraries/study-savina.md`). Each file is a
self-contained program built on the core actor primitives (`spawn`, `send`,
`receive`, `self`) and the `Bench` library (`lib/Bench.trp`) for timing and
verification.

## Running

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
