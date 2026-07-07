#!/bin/sh
# Whole-corpus round-trip check for the troupe-ir-sexp format.
#
# For every .trp under the given roots (default: tests/rt/pos), compile it,
# print its IR as troupe-ir-sexp, re-parse it, and compare the position-erased
# ASTs (troupec --verify-ir-sexp). Reports a pass/fail tally and exits non-zero
# if any program's round-trip fails.
#
# Usage: compiler/test/ir-sexp-test/roundtrip-corpus.sh [ROOT ...]

set -u

# Locate the repository root (two levels up from this script's directory).
SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$SCRIPT_DIR/../../.." && pwd)
TROUPEC="$ROOT/bin/troupec"

if [ ! -x "$TROUPEC" ]; then
  echo "error: compiler not built at $TROUPEC (run 'make compiler')" >&2
  exit 2
fi

if [ "$#" -gt 0 ]; then
  ROOTS="$@"
else
  ROOTS="$ROOT/tests/rt/pos"
fi

pass=0
fail=0
failed_list=""

for r in $ROOTS; do
  for f in $(find "$r" -name '*.trp' | sort); do
    # Skip empty files (e.g. placeholder .trp fixtures): not programs.
    [ -s "$f" ] || continue
    out=$("$TROUPEC" --verify-ir-sexp "$f" 2>&1)
    if echo "$out" | grep -q "round-trip OK"; then
      pass=$((pass + 1))
    else
      fail=$((fail + 1))
      failed_list="$failed_list\n  $f\n    $(echo "$out" | tail -1)"
    fi
  done
done

echo "troupe-ir-sexp corpus round-trip: $pass passed, $fail failed"
if [ "$fail" -ne 0 ]; then
  printf "failures:%b\n" "$failed_list" >&2
  exit 1
fi
exit 0
