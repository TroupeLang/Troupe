#!/bin/sh
#
# run.sh — drive the cross-language comparison across language runtimes.
#
# This is the one inherently shell-level layer of the benchmarking
# architecture, because it spans separate language binaries: the Troupe
# runtime exposes no process-spawn builtin, so a Troupe program cannot start
# python3. Everything downstream of the row files is Troupe again
# (compare.trp).
#
# What it does:
#
#   * asks both languages for their benchmark list and refuses to run if the
#     two disagree, so a kernel added on one side only is caught before
#     anything is measured;
#   * interleaves the languages per benchmark (Troupe fib, python3 fib, Troupe
#     listsum, ...) so machine drift over the measurement window hits both
#     equally, rather than landing entirely on whichever ran second;
#   * measures each language's fixed startup cost separately from kernel time,
#     since the two differ by more than an order of magnitude and mixing them
#     would answer neither question;
#   * writes one row file per language, each headed by `#` comment lines
#     recording the machine, the versions and the invocation, so a row file is
#     interpretable without the command line that produced it.
#
# Usage:
#
#   ./run.sh                          # every benchmark, 3 reps, results in out/
#   ./run.sh --suite awfy             # the Are-We-Fast-Yet micros
#   ./run.sh --reps 5 fib             # one benchmark, 5 reps
#   ./run.sh --out /tmp/x --reverse   # reverse language order (drift control)
#   ./run.sh --no-compare             # row files only, skip the report
#
# Suites: `kernels` (default) is the pair of hand-written cross-language
# kernels; `awfy` is the Are-We-Fast-Yet micros, the Troupe side reusing
# examples/benchmarks/awfy/Awfy.trp and the Python side the vendored upstream
# sources under python/awfy/.
#
# --reverse is the position control: a real language difference reproduces
# with the order inverted, a warmup or position artifact does not. Run it on
# any result worth believing.
#
# Results are machine-specific and are not committed; out/ is gitignored.

set -e

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
TROUPE_ROOT=$(cd "$SCRIPT_DIR/../../.." && pwd)

OUT="$TROUPE_ROOT/out"
REPS=3
SUITE=kernels
REVERSE=false
COMPARE=true
BENCHMARKS=""

while [ $# -gt 0 ]; do
    case "$1" in
        --out)        OUT=$2; shift 2 ;;
        --reps)       REPS=$2; shift 2 ;;
        --suite)      SUITE=$2; shift 2 ;;
        --reverse)    REVERSE=true; shift ;;
        --no-compare) COMPARE=false; shift ;;
        -h|--help)    sed -n '2,48p' "$0"; exit 0 ;;
        -*)           echo "unknown option: $1" >&2; exit 2 ;;
        *)            BENCHMARKS="$BENCHMARKS $1"; shift ;;
    esac
done

command -v python3 >/dev/null 2>&1 || { echo "python3 not found" >&2; exit 1; }
mkdir -p "$OUT"

TROUPE_ROWS="$OUT/crosslang-$SUITE-troupe.txt"
PYTHON_ROWS="$OUT/crosslang-$SUITE-python3.txt"
REPORT="$OUT/crosslang-$SUITE-report.md"

TROUPE_FLAGS="--suppress-local-info-message --suppress-main-thread-finished-message"

run_troupe() {   # run_troupe <args...>
    ( cd "$TROUPE_ROOT" && ./local.sh examples/benchmarks/crosslang/runall.trp \
        $TROUPE_FLAGS -- "suite=$SUITE" "$@" )
}

run_python() {   # run_python <args...>
    ( cd "$SCRIPT_DIR/python" && python3 runall.py "suite=$SUITE" "$@" )
}

# --- benchmark set: one source of truth per language, cross-checked ---------

TROUPE_LIST=$(run_troupe mode=list)
PYTHON_LIST=$(run_python mode=list)

if [ "$TROUPE_LIST" != "$PYTHON_LIST" ]; then
    echo "benchmark sets differ between languages -- refusing to measure." >&2
    echo "  troupe : $(echo "$TROUPE_LIST" | tr '\n' ' ')" >&2
    echo "  python3: $(echo "$PYTHON_LIST" | tr '\n' ' ')" >&2
    exit 3
fi

[ -n "$BENCHMARKS" ] || BENCHMARKS=$TROUPE_LIST

# --- fixed startup cost -----------------------------------------------------
# /usr/bin/time resolves to 10ms, which is far too coarse for a 20ms process,
# so time a batch of N and divide. This measures the whole path a user pays:
# for Troupe that includes compiling the program, which is why startup.trp
# goes through local.sh rather than a pre-compiled artifact.

STARTUP_N=20

startup_ms() {   # startup_ms <command...>; prints milliseconds per run
    real=$( { /usr/bin/time -p sh -c '
        n=$1; shift
        i=0
        while [ $i -lt $n ]; do "$@" >/dev/null 2>&1; i=$((i+1)); done
    ' _ "$STARTUP_N" "$@" ; } 2>&1 | awk '/^real/ {print $2}' )
    echo "$real $STARTUP_N" | awk '{printf "%.1f", $1 * 1000 / $2}'
}

echo "measuring startup cost ($STARTUP_N runs per language)..." >&2
TROUPE_STARTUP=$(startup_ms sh -c "cd '$TROUPE_ROOT' && ./local.sh examples/benchmarks/crosslang/startup.trp $TROUPE_FLAGS")
PYTHON_STARTUP=$(startup_ms python3 "$SCRIPT_DIR/python/startup.py")

# --- row file headers -------------------------------------------------------

TROUPE_VERSION=$(cd "$TROUPE_ROOT" && git rev-parse --short HEAD 2>/dev/null || echo unknown)
NODE_VERSION=$(node --version 2>/dev/null || echo unknown)
PYTHON_VERSION=$(python3 -c 'import sys; print(sys.version.split()[0])')
PYTHON_JIT=$(python3 -c 'import sys; print("on" if getattr(sys,"_jit",None) and sys._jit.is_available() else "off")')
MACHINE=$(uname -sm)
CPU=$(sysctl -n machdep.cpu.brand_string 2>/dev/null || echo unknown)

write_header() {   # write_header <file> <language> <version-detail> <startup-ms>
    {
        echo "# language: $2"
        echo "# version: $3"
        echo "# startup-ms: $4"
        echo "# startup-method: mean of $STARTUP_N whole-process runs doing no work"
        echo "# machine: $MACHINE / $CPU"
        echo "# suite: $SUITE"
        echo "# reps: $REPS"
        echo "# benchmarks: $(echo "$BENCHMARKS" | tr '\n' ' ' | sed 's/^ *//;s/ *$//')"
        echo "# order: $([ "$REVERSE" = true ] && echo 'python3 first' || echo 'troupe first')"
        echo "# rows: <name> n=<size> rep=<i> ms=<elapsed> ok=<bool>"
    } > "$1"
}

write_header "$TROUPE_ROWS"  troupe  "$TROUPE_VERSION on node $NODE_VERSION" "$TROUPE_STARTUP"
write_header "$PYTHON_ROWS"  python3 "$PYTHON_VERSION (jit $PYTHON_JIT)"     "$PYTHON_STARTUP"

# --- interleaved measurement ------------------------------------------------

for b in $BENCHMARKS; do
    if [ "$REVERSE" = true ]; then
        echo "  python3 $b" >&2; run_python "reps=$REPS" "$b" >> "$PYTHON_ROWS"
        echo "  troupe  $b" >&2; run_troupe "reps=$REPS" "$b" >> "$TROUPE_ROWS"
    else
        echo "  troupe  $b" >&2; run_troupe "reps=$REPS" "$b" >> "$TROUPE_ROWS"
        echo "  python3 $b" >&2; run_python "reps=$REPS" "$b" >> "$PYTHON_ROWS"
    fi
done

echo "wrote $TROUPE_ROWS" >&2
echo "wrote $PYTHON_ROWS" >&2

# --- report -----------------------------------------------------------------
# The baseline is whichever language is named first, so ratios read as
# "N times the baseline". python3 is the baseline here because the question
# this suite exists to answer is how much slower Troupe is than it.

if [ "$COMPARE" = true ]; then
    ( cd "$TROUPE_ROOT" && ./local.sh examples/benchmarks/crosslang/compare.trp \
        --io-root "$OUT" $TROUPE_FLAGS \
        -- "python3=$(basename "$PYTHON_ROWS")" "troupe=$(basename "$TROUPE_ROWS")" ) \
      | tee "$REPORT"
    echo "wrote $REPORT" >&2
fi
