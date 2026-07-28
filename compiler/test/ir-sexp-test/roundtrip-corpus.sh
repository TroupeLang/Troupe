#!/bin/sh
# Whole-corpus round-trip check for the troupe-ir-sexp format.
#
# For every .trp under the given roots (default: tests/rt/pos), compile it,
# print its IR as troupe-ir-sexp with and without source positions, re-parse
# each, and compare the ASTs (troupec --verify-ir-sexp).
#
# A file that does not compile at all is reported as SKIPPED, not failed: the
# corpus contains module components (under modsrc/) whose pins live with the
# main program that imports them, so compiling one standalone stops in import
# processing before any IR exists. Those files say nothing about the format
# either way. Skipped files are always listed, so a compile regression shows up
# as a growing skip list instead of hiding among expected failures.
#
# Exits non-zero only when a program that compiled failed to round-trip.
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
skip=0
failed_list=""
skipped_list=""

for r in $ROOTS; do
  for f in $(find "$r" -name '*.trp' | sort); do
    # Skip empty files (e.g. placeholder .trp fixtures): not programs.
    [ -s "$f" ] || continue
    out=$("$TROUPEC" --verify-ir-sexp "$f" 2>&1)
    if echo "$out" | grep -q "round-trip OK"; then
      pass=$((pass + 1))
    elif echo "$out" | grep -q "round-trip FAILED"; then
      fail=$((fail + 1))
      failed_list="$failed_list\n  $f\n    $(echo "$out" | tail -1)"
    else
      # Did not compile: nothing was printed, so nothing was round-tripped.
      skip=$((skip + 1))
      skipped_list="$skipped_list\n  $f\n    $(echo "$out" | tail -1)"
    fi
  done
done

echo "troupe-ir-sexp corpus round-trip: $pass passed, $fail failed, $skip skipped (did not compile)"
if [ "$skip" -ne 0 ]; then
  printf "skipped:%b\n" "$skipped_list"
fi
if [ "$fail" -ne 0 ]; then
  printf "failures:%b\n" "$failed_list" >&2
  exit 1
fi
exit 0
