# Vendored: Are-We-Fast-Yet, Python implementation

Upstream: <https://github.com/smarr/are-we-fast-yet>, path `benchmarks/Python/`,
commit `74306fec151070fd07157cefeacf19e7e0bcdc89`.

Source suite: Marr, Daloze, Mössenböck, "Cross-Language Compiler Benchmarking:
Are We Fast Yet?" (DLS 2016).

These files are **unmodified upstream sources**. The adapter that presents them
as Troupe-style benchmark descriptors is `../awfy_kernels.py`, deliberately kept
outside this directory so that refreshing the vendored copy is a straight
overwrite with nothing to re-apply.

## What is here, and why only this much

The eight micro benchmarks that `examples/benchmarks/awfy/Awfy.trp` implements,
plus what they import:

| File                   | Role                                                      |
|------------------------|-----------------------------------------------------------|
| `benchmark.py`         | Base class; defines `inner_benchmark_loop`                |
| `bounce.py`            | Micro                                                     |
| `list.py`              | Micro                                                     |
| `mandelbrot.py`        | Micro (overrides `inner_benchmark_loop`)                  |
| `permute.py`           | Micro                                                     |
| `queens.py`            | Micro                                                     |
| `sieve.py`             | Micro                                                     |
| `storage.py`           | Micro                                                     |
| `towers.py`            | Micro                                                     |
| `som/random.py`        | The suite's PRNG, used by `bounce` and `storage`          |
| `som/__init__.py`      | Package marker                                            |

Not vendored: the AWFY macro benchmarks (CD, DeltaBlue, Havlak, Json, Richards),
`nbody` (no Troupe counterpart yet), the suite's own `harness.py` and `run.py`,
and the `som` collection classes those need. The macros and their dependencies
would be dead weight until Troupe implements them.

`harness.py` is deliberately not used even for the micros that are here. It
applies its own iteration policy, whereas the point of this suite is that both
languages are measured by the *same* policy — one untimed warmup, then N timed
reps, as `lib/Bench.trp` defines it. `awfy_kernels.py` drives the benchmark
classes directly instead.

## Licensing

The upstream `LICENSE.md` and `AUTHORS.md` are copied here verbatim. Licenses
differ per benchmark and the authoritative statement is the header comment of
each file:

| Files                                                    | License                      |
|----------------------------------------------------------|------------------------------|
| `bounce`, `list`, `permute`, `queens`, `sieve`, `storage`, `towers`, `benchmark`, `som/*` | MIT (SOM class library) |
| `mandelbrot`                                             | BSD 3-Clause (CLBG)          |

Both permit redistribution with the notices retained, which is why the header
comments must survive any future re-vendoring.

## Refreshing

Overwrite the files from a fresh checkout of upstream, update the commit hash
above, and re-run the suite. If a canonical result changes, `awfy_kernels.py`
and `examples/benchmarks/awfy/Awfy.trp` both need the new value — the run will
say so, because `check` fails rather than silently measuring the wrong thing.
