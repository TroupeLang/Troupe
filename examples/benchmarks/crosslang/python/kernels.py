"""kernels -- the Python counterparts of the Crosslang.trp descriptors.

Parity policy: same algorithm, written idiomatically for Python (the
Are-We-Fast-Yet rule). Where the natural data representation differs from
Troupe's, the comment on that kernel says so; the difference is part of what
the row measures.

Each kernel's `check` is the same closed form used on the Troupe side,
computed by a different algorithm than the one under test. Both languages
therefore validate against the same values, which is how this suite
establishes that the two sides ran the same computation.

`sizes` must match Crosslang.trp exactly. Nothing enforces that here;
compare.trp reports any (benchmark, size) cell present in one language and
absent from the other.
"""

import sys

# fib recurses to depth n only (34 here), but the default limit of 1000 is
# close enough to the sizes a caller might pass that raising it is worth the
# line. It does not affect timing.
sys.setrecursionlimit(100000)


# ==================== fib ====================
# Call-dominated, allocation-free. Same shape as Crosslang.trp: the recursion
# bottoms out at 1 for x <= 2, making fib(n) the n-th Fibonacci number under
# F(1) = F(2) = 1.
def _fib(x):
    return _fib(x - 1) + _fib(x - 2) if x > 2 else 1


def _fib_expected(n):
    """Iterative Fibonacci -- a different algorithm from the recursion above."""
    a, b, i = 0, 1, 1
    while i < n:
        a, b, i = b, a + b, i + 1
    return b


# ==================== listsum ====================
# Allocation-dominated: build a sequence of n integers, then fold it.
#
# REPRESENTATION DIFFERS from the Troupe side, deliberately. Troupe builds a
# cons list -- one labelled heap cell per element; this builds a Python list --
# one contiguous array that amortises its growth. Both are the idiomatic way to
# express the task in their language, so this follows the parity policy, but the
# two do not allocate alike and the resulting ratio is not runtime speed alone.
def _listsum(n):
    xs = []
    for i in range(n, 0, -1):
        xs.append(i)
    total = 0
    for x in xs:
        total += x
    return total


ALL = [
    {"name": "fib",
     "bench": _fib,
     "check": lambda n, r: r == _fib_expected(n),
     "sizes": [30, 32, 34]},

    {"name": "listsum",
     "bench": _listsum,
     "check": lambda n, r: r == n * (n + 1) // 2,
     "sizes": [200000, 400000, 800000]},
]
