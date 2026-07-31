"""awfy_kernels -- the vendored Are-We-Fast-Yet micros as benchmark descriptors.

Adapts the upstream benchmark classes in awfy/ (unmodified; see
awfy/PROVENANCE.md) to the {name, bench, check, sizes} shape the rest of this
suite uses, so they run under the same harness, with the same warmup and
repetition policy, as everything else.

Counterpart: examples/benchmarks/awfy/Awfy.trp on the Troupe side. Names, sizes
and expected values are identical to that file by construction -- every value
below is the one the upstream class itself verifies against, and Awfy.trp was
transcribed from the same sources.

Two things this adapter has to get right, both of them parity concerns:

1. `Benchmark.inner_benchmark_loop(n)` runs the kernel n times AND verifies each
   run inside the loop, returning a bool. Awfy.trp instead runs the kernel n
   times and checks the value once, outside the timed region. Timing the two
   would not be timing the same thing, so this adapter does not call
   `inner_benchmark_loop`: it runs `benchmark()` n times itself, keeps the last
   value, and leaves verification to `check`. That matches Awfy.trp exactly.

2. `mandelbrot` is the exception upstream and here. It overrides
   `inner_benchmark_loop`, its `benchmark()` raises, and its size is the grid
   size rather than a repetition count -- so it has canonical results only at
   the sizes the suite documents (1, 500, 750). It is driven through its
   `_mandelbrot`/`_verify_result` statics, the only way to obtain the value
   itself rather than a pass/fail bool.
"""

import os
import sys

# The vendored sources use absolute imports (`from benchmark import Benchmark`,
# `from som.random import Random`), so their own directory has to be importable.
_AWFY_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "awfy")
if _AWFY_DIR not in sys.path:
    sys.path.insert(0, _AWFY_DIR)

from bounce import Bounce                                  # noqa: E402
from list import List                                      # noqa: E402
from mandelbrot import Mandelbrot                           # noqa: E402
from permute import Permute                                # noqa: E402
from queens import Queens                                  # noqa: E402
from sieve import Sieve                                    # noqa: E402
from storage import Storage                                # noqa: E402
from towers import Towers                                  # noqa: E402


def _repeat(cls):
    """Run the benchmark n times, returning the last value.

    A fresh instance per timed run, because several of these benchmarks keep
    mutable state on the instance (queens, storage) and reusing one would let a
    later run start from an earlier run's heap.
    """
    def bench(n):
        instance = cls()
        result = None
        for _ in range(n):
            result = instance.benchmark()
        return result
    return bench


def _mandelbrot_bench(size):
    return Mandelbrot._mandelbrot(size)


def _mandelbrot_check(size, result):
    return Mandelbrot._verify_result(result, size)


# Sizes are repetition counts (the outer repetition AWFY's own harness applies),
# except mandelbrot, where size is the grid size. Both match Awfy.trp.
ALL = [
    {"name": "bounce",
     "bench": _repeat(Bounce),
     "check": lambda n, r: r == 1331,
     "sizes": [50, 200, 800]},

    {"name": "list",
     "bench": _repeat(List),
     "check": lambda n, r: r == 10,
     "sizes": [10, 50, 200]},

    {"name": "mandelbrot",
     "bench": _mandelbrot_bench,
     "check": _mandelbrot_check,
     "sizes": [1, 500, 750]},

    {"name": "permute",
     "bench": _repeat(Permute),
     "check": lambda n, r: r == 8660,
     "sizes": [20, 80, 320]},

    {"name": "queens",
     "bench": _repeat(Queens),
     "check": lambda n, r: r is True,
     "sizes": [5, 20, 80]},

    {"name": "sieve",
     "bench": _repeat(Sieve),
     "check": lambda n, r: r == 669,
     "sizes": [1, 3, 10]},

    {"name": "storage",
     "bench": _repeat(Storage),
     "check": lambda n, r: r == 5461,
     "sizes": [10, 40, 160]},

    {"name": "towers",
     "bench": _repeat(Towers),
     "check": lambda n, r: r == 8191,
     "sizes": [10, 40, 160]},
]
