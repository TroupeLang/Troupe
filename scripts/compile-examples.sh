#!/bin/sh
# Compile every example and benchmark, and report the ones that do not.
#
# The golden suite covers tests/ only, so nothing else notices when a change to
# a library, or to a module's content hash, stops an example compiling. Run from
# the repository root; `make test/examples` does that.
#
# Every .trp is compiled, module components included -- most of them compile
# perfectly well on their own. A component that itself imports another module
# has no dependencies file to resolve that pin against, so it is compiled by
# each program that imports it and counted there instead; the compiler says
# "no pin for module" when it is reached on its own, and that message is what
# identifies it. A stale pin reports differently ("has content hash ... but the
# dependencies file pins") and fails wherever it appears -- which is what a
# component's program not being re-pinned looks like.
set -e

cd "$(dirname "$0")/.."
compiler=./bin/troupec
out=$(mktemp)
err=$(mktemp)
trap 'rm -f "$out" "$err"' EXIT

failed=0
compiled=0
via_importer=0

for f in $(find examples -name '*.trp' -not -path '*/out/*' | sort); do
  if "$compiler" "$f" -o "$out" > "$err" 2>&1; then
    compiled=$((compiled + 1))
  elif grep -q 'no pin for module' "$err"; then
    # Compiled by the programs that import it, not here.
    via_importer=$((via_importer + 1))
  else
    failed=$((failed + 1))
    echo "FAIL $f"
    sed 's/^/     /' "$err" | head -3
  fi
done

echo "examples: $compiled compiled here, $via_importer compiled through their importers, $failed failed"
[ "$failed" -eq 0 ]
