# Cross-language benchmarks

Compares Troupe against other language implementations on kernels written to be
transcribable one-to-one. Currently Troupe against CPython 3.

For benchmarking Troupe against itself (across commits, across implementation
strategies), see the Savina, AWFY, CLBG and data-structure suites in the sibling
directories; they share the descriptor architecture and the `SavinaReport`
renderer. This suite differs in one respect only: it crosses a process boundary,
so its driver is shell.

## Suites

| Suite     | Benchmarks | Troupe side                          | Python side                              |
|-----------|------------|--------------------------------------|------------------------------------------|
| `kernels` | 2          | `Crosslang.trp` (written here)       | `python/kernels.py` (written here)       |
| `awfy`    | 8          | `../awfy/Awfy.trp` (reused unchanged)| `python/awfy/` (vendored upstream, MIT)  |

`kernels` is the pair of hand-written kernels this suite started from: `fib`
(call-dominated) and `listsum` (allocation-dominated). They exist to isolate two
specific costs, not to be representative.

`awfy` is Are-We-Fast-Yet, the suite designed for cross-language comparison
(Marr, Daloze, Mössenböck, DLS 2016). Neither side is a transcription written
for this comparison: the Troupe side is the existing single-language suite,
imported across directories and reused unchanged, and the Python side is
upstream's own implementation, vendored verbatim. See `python/awfy/PROVENANCE.md`
for what was vendored, from which commit, and under which licenses.

Read the AWFY ratios with the representation caveat below — they are dominated
by it, and are not a measure of runtime speed.

## Running

```sh
./run.sh                          # kernels suite, 3 reps, results in out/
./run.sh --suite awfy             # the Are-We-Fast-Yet micros
./run.sh --reps 5 fib             # one benchmark, 5 reps
./run.sh --out /tmp/x --reverse   # reverse language order (drift control)
./run.sh --no-compare             # row files only, skip the report
```

`run.sh` writes three files per suite to the output directory:

| File                             | Content                                           |
|----------------------------------|---------------------------------------------------|
| `crosslang-<suite>-troupe.txt`   | Troupe measurement rows, `#` header of provenance |
| `crosslang-<suite>-python3.txt`  | Python measurement rows, same format              |
| `crosslang-<suite>-report.md`    | The comparison report                             |

The `awfy` suite takes several minutes: at its default sizes one Troupe
repetition of the whole suite is over two minutes, and `run.sh` runs a warmup
plus three.

Results are machine-specific and are not committed. Re-render a report from
saved row files without re-measuring:

```sh
./local.sh examples/benchmarks/crosslang/compare.trp --io-root out \
    --suppress-local-info-message --suppress-main-thread-finished-message \
    -- python3=crosslang-python3.txt troupe=crosslang-troupe.txt
```

Each side can also be run alone; both emit rows on stdout and nothing else:

```sh
./local.sh examples/benchmarks/crosslang/runall.trp \
    --suppress-local-info-message --suppress-main-thread-finished-message \
    -- reps=5 fib
cd python && python3 runall.py reps=5 fib
```

## Layout

| File                     | Role                                                       |
|--------------------------|------------------------------------------------------------|
| `Crosslang.trp`          | `kernels` descriptors, Troupe side (program-relative module) |
| `runall.trp`             | Troupe runner; emits rows on stdout                        |
| `python/kernels.py`      | `kernels` in Python, sharing the `check` values            |
| `python/awfy/`           | Vendored upstream AWFY Python sources, unmodified          |
| `python/awfy_kernels.py` | Adapter: AWFY classes → descriptors (kept outside `awfy/`) |
| `python/bench.py`        | Python timing harness; mirrors `lib/Bench.trp`             |
| `python/runall.py`       | Python runner; same arguments and row format as `runall.trp` |
| `startup.trp`            | A program that does nothing, for measuring fixed startup   |
| `python/startup.py`      | Its Python counterpart                                     |
| `run.sh`                 | Cross-binary driver: interleaving, headers, startup, report |
| `compare.trp`            | Reads N row files, writes the comparison report            |

The adapter sits outside `python/awfy/` on purpose: refreshing the vendored copy
is then a straight overwrite with nothing to re-apply.

## The row format

One line per timed run, identical on both sides — it is `lib/Bench`'s own
format, unchanged, so nothing in the existing harness had to be adapted:

```
<name> n=<size> rep=<i> ms=<elapsed> ok=<bool>
```

Lines beginning with `#` are header metadata (language, version, machine,
startup cost, invocation) and are skipped by the parser, which is what makes a
row file interpretable without the command line that produced it. Any other
line shape is ignored.

Rep numbering is 0-based on both sides, because `Bench.collectRows` numbers reps
with `List.range`, which is `[0 .. n-1]`.

Timing precision differs and is not reconciled: `time.perf_counter` has
nanosecond resolution while Troupe's `getTime` is whole milliseconds, so Python
rows carry one decimal place and Troupe rows carry integers. Both parse on the
Troupe side because `stringToInt` is implemented with `parseFloat`
(`rt/src/builtins/stringToInt.mts:11`). Sizes are chosen to keep every timed run
well above ten milliseconds on both sides, which bounds what the coarser clock
can cost.

## Parity: what "the same benchmark" means here

The policy is Are-We-Fast-Yet's: **the same algorithm, written idiomatically in
each language**, with no language-specific tricks. Where the natural data
representation differs between the two sides, the descriptor comment says so on
both sides, and the difference is part of what that row measures.

This is not a detail. In the `kernels` suite the two benchmarks differ in their
Troupe/Python ratio by roughly a factor of eight, and the reason is
representation:

| Benchmark | Shape                    | Representation                                     |
|-----------|--------------------------|----------------------------------------------------|
| `fib`     | Call-dominated           | Identical on both sides — integers, no structures  |
| `listsum` | Allocation-dominated     | Troupe cons list vs Python list — differs          |

`fib` is the closest thing here to a like-for-like runtime comparison, because
no data structure stands between the two languages. `listsum` measures the
natural way to build and fold a sequence in each language, which is a fair
question but a different one. Quoting either number alone as "how much slower
Troupe is" misrepresents it; the report prints the geometric mean together with
the min and max precisely so the spread stays visible.

### The AWFY suite is dominated by mutable arrays

AWFY's benchmarks are built on indexed arrays with in-place update. Python has
those natively. **Troupe has no mutable arrays**, so `Awfy.trp` models them with
`lib/Vector` (immutable, O(log n) indexed update) or with native lists where the
array is only built and traversed. Every indexed write that costs Python O(1)
costs Troupe O(log n) plus an allocation.

This is a complexity difference, not a constant factor, and it grows with size.
The AWFY ratios are therefore **not a measure of runtime speed** — they measure
what the absence of mutable arrays costs on array-shaped code, which is a real
and useful question but a different one. Per-benchmark figures are the only
readable form; the suite geomean mixes benchmarks whose ratios span orders of
magnitude and should not be quoted.

The measured ordering tracks exactly that. `mandelbrot`, which barely touches a
data structure, comes out around 8x — the same territory as the `kernels` suite.
`sieve`, which is nothing but in-place writes into a flag array, comes out around
1000x. Everything else falls between, ordered by how much indexed mutation it
does.

`fib` in the `kernels` suite remains the cleanest available like-for-like
comparison, precisely because nothing stands between the two languages.

Two safeguards enforce parity mechanically:

- **Shared check values.** Each kernel validates against the same closed form on
  both sides, computed by a different algorithm than the one under test. If the
  two languages agree on the answer, they ran the same computation.
- **Benchmark-set cross-check.** `run.sh` asks both languages for their
  benchmark list and refuses to measure if the two disagree, so a kernel added
  on one side only is caught before anything is timed. Sizes are not
  cross-checked this way — `compare.trp` reports any `(benchmark, size)` cell
  present in one language and absent from the other.

## Methodology

- **Two axes, reported separately.** Kernel time is measured in-process, so it
  excludes startup; startup is measured on its own as a whole-process mean over
  20 runs of a program that does nothing. The two differ by more than an order
  of magnitude and mixing them would answer neither question.
- **Minimum over repetitions**, after one untimed warmup per size. The minimum
  is the run least disturbed by whatever else the machine was doing, which is
  the right summary for a ratio between two languages measured in the same
  window. The full rows are kept, so a median can be recomputed.
- **Interleaved per benchmark** (Troupe `fib`, Python `fib`, Troupe `listsum`,
  …) so drift over the measurement window hits both languages equally rather
  than landing on whichever ran second.
- **Reverse-order control.** `--reverse` inverts the language order. A real
  difference reproduces; a warmup or position artifact does not. Run it on any
  result worth believing.
- **Quiet machine.** Allocation-heavy rows inflate several-fold under
  background load. No builds or test suites concurrent with timing.
- **A ten-millisecond floor.** `compare.trp` marks any size where a language
  measured under 10 ms with `*`: Troupe's `getTime` is whole milliseconds, so a
  4 ms cell carries 25% quantisation before any noise. Several AWFY sizes are
  below the floor, because those sizes come from the single-language suite and
  were never tuned against wall-clock. A zero-millisecond baseline makes the
  ratio undefined; it is reported as `-` and excluded from the geometric mean
  rather than treated as infinite.

## Adding a benchmark

To the `kernels` suite:

1. Add a descriptor to `Crosslang.trp` — `{name, bench, check, sizes}` — with a
   comment stating the representation policy for that kernel.
2. Add the counterpart to `python/kernels.py` with the **same name, the same
   sizes, and the same check values**, and the matching comment.
3. Choose sizes so the fastest language's smallest timed run still exceeds ten
   milliseconds.
4. Run `./bin/troupec --update-deps examples/benchmarks/crosslang/runall.trp`
   (or `make benchmark-deps`) if the import graph changed.
5. Run `./run.sh <name>` and confirm `ok=true` on both sides before believing
   any timing.

## Adding a suite

A suite is a pair of descriptor lists, one per language, registered by name. Add
the Troupe list to the `suite` case in `runall.trp` (importing an existing
suite's module across directories is enough — module identity is the hash of the
code, not the path), the Python list to `SUITES` in `python/runall.py`, and
nothing else: `run.sh` takes `--suite` and names its files after it, and
`compare.trp` is suite-agnostic.

Both runners answer `list`, and `run.sh` refuses to measure if the two sides
disagree on the benchmark set — so a suite wired up on one side only fails
immediately rather than producing a half-populated table.

## Adding a language

`compare.trp` takes any number of `label=path` row files and gives every
non-baseline language its own pair of columns, so a third language needs no
change to the report. What it needs is a harness that emits the row format, a
runner supporting `list`, `reps=N` and name selection, and three additions to
`run.sh`: a `run_<lang>` function, its version capture, and its arm in the
interleaving loop.
