#!/usr/bin/env bash
#
# Differential Haskell<->TypeScript lattice harness (Steps 3 + 4c).
#
# Generates a fixed-seed corpus, prepends the committed boundary/regression
# cases, then runs both the Haskell judge (compiler `dclabels --judge`) and the
# TypeScript judge (rt/built/proptests/tools/judge.mjs) over it and diffs their
# verdict streams. Two case kinds share the corpus:
#
#   * cnf: `{"x":[[..]],"y":[[..]]}` -- compares `cnfImplies`/`cnfEq` (Haskell)
#     against `implies`/`equals` (TS). A divergence means the compiler and
#     runtime disagree about permitted flows -- a soundness bug by construction.
#   * v1:  `{"kind":"v1","raw":..,"canon":..}` -- both judges answer whether the
#     two V1 label strings denote the same label (Haskell `v1LabelEq`, TS
#     `fromV1String(..).equals(..)`). A divergence is a V1-parsing soundness bug.
#
# On any mismatch the script prints the first differing case (line number +
# content) and exits nonzero.
#
# Corpus and verdict files live in a `mktemp -d` dir, never in the repo.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

KNOWN="$ROOT/scripts/differential/known-cases.jsonl"
GEN_JS="$ROOT/rt/built/proptests/tools/gen-corpus.mjs"
TS_JUDGE="$ROOT/rt/built/proptests/tools/judge.mjs"

for f in "$GEN_JS" "$TS_JUDGE"; do
    if [ ! -f "$f" ]; then
        echo "Missing $f -- build the runtime first: /usr/bin/make rt" >&2
        exit 2
    fi
done

# Resolve the Haskell judge binary (built by `stack build :dclabels`).
HS_JUDGE="$(cd "$ROOT/compiler" && stack path --local-install-root 2>/dev/null)/bin/dclabels"
if [ ! -x "$HS_JUDGE" ]; then
    echo "Missing $HS_JUDGE -- build it first: (cd compiler && stack build :dclabels)" >&2
    exit 2
fi

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

CORPUS="$TMP/corpus.jsonl"
ALL="$TMP/all.jsonl"
HS_OUT="$TMP/hs-verdicts.txt"
TS_OUT="$TMP/ts-verdicts.txt"

# Committed boundary cases first, then the generated corpus. Both are diffed;
# the headline count reports the generated corpus size (the known cases are
# additional regression anchors gated by the same diff).
node "$GEN_JS" > "$CORPUS"
cat "$KNOWN" "$CORPUS" > "$ALL"
TOTAL_N="$(grep -c . "$ALL" | tr -d ' ')"
V1_N="$(grep -c '"kind":"v1"' "$ALL" | tr -d ' ' || true)"
CNF_N="$((TOTAL_N - V1_N))"

node "$TS_JUDGE" < "$ALL" > "$TS_OUT"
"$HS_JUDGE" --judge < "$ALL" > "$HS_OUT"

if ! diff -q "$HS_OUT" "$TS_OUT" > /dev/null; then
    # First differing line number from unified diff hunk headers.
    LINENO_DIFF="$(diff "$HS_OUT" "$TS_OUT" | grep -m1 -oE '^[0-9]+' || true)"
    : "${LINENO_DIFF:=1}"
    echo "MISMATCH: Haskell and TypeScript judges disagree." >&2
    echo "First differing verdict at line $LINENO_DIFF:" >&2
    echo "  case:    $(sed -n "${LINENO_DIFF}p" "$ALL")" >&2
    echo "  haskell: $(sed -n "${LINENO_DIFF}p" "$HS_OUT")" >&2
    echo "  ts:      $(sed -n "${LINENO_DIFF}p" "$TS_OUT")" >&2
    exit 1
fi

echo "$TOTAL_N/$TOTAL_N agree (cnf=$CNF_N, v1=$V1_N)"
