"""bench -- the Python side of the cross-language timing harness.

Mirrors lib/Bench.trp so that both languages are measured the same way: for
each size, one untimed warmup run followed by `reps` timed runs, each result
validated by the benchmark's own `check`, each run reported as one row:

    <name> n=<size> rep=<i> ms=<elapsed> ok=<bool>

Two deliberate correspondences with lib/Bench.trp:

  * Rep numbering is 0-based, because Bench.collectRows numbers reps with
    List.range, which is [0 .. n-1] (lib/List.trp).
  * `ok` is spelled "true"/"false", because that is how Troupe's toString
    renders a boolean, and compare.trp parses one format for both sides.

Timing precision differs and cannot be made to match: time.perf_counter has
nanosecond resolution, while Troupe's getTime is whole milliseconds. Python
rows therefore carry one decimal place and Troupe rows carry integers. Both
parse on the Troupe side, because stringToInt is implemented with parseFloat
(rt/src/builtins/stringToInt.mts:11). The asymmetry is harmless as long as
sizes keep every timed run well above ten milliseconds on both sides, which
is the same rule lib/Bench already imposes.
"""

import sys
from time import perf_counter


def time_call(f, arg):
    """Time one call. Returns (result, elapsed_ms)."""
    t0 = perf_counter()
    r = f(arg)
    return r, (perf_counter() - t0) * 1000.0


def collect_rows(name, sizes, reps, bench, check, out=sys.stdout):
    """Run `bench` over `sizes`, printing one row per timed run.

    Returns the measurements as a list of (size, rep, ms, ok) tuples, oldest
    first -- the same shape Bench.collectRows returns.
    """
    rows = []
    for size in sizes:
        bench(size)                                   # untimed warmup
        for i in range(reps):
            r, ms = time_call(bench, size)
            ok = check(size, r)
            print("%s n=%d rep=%d ms=%.1f ok=%s"
                  % (name, size, i, ms, "true" if ok else "false"), file=out)
            rows.append((size, i, ms, ok))
    return rows
