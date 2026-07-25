#!/usr/bin/env bash
# Differential parse harness for the operator-chain grammar rework.
#
# Snapshots the post-pattern-elimination stage dump (out.nopats) of every
# compilable .trp under lib/, tests/, examples/, and compares later compiler
# states against the snapshot. The grammar rework must not change any existing
# program's parse: byte-identical dumps or failure.
#
#   scripts/parse-differential.sh baseline   capture snapshots (pristine build)
#   scripts/parse-differential.sh check      recompile and diff against snapshots
#
# Baselines live outside the repository tree (they are large and belong to the
# planning area): _dev_planning/custom-operators/.baselines/ of the main
# checkout, overridable via BASELINE_DIR. Files that do not compile standalone
# (module-dependent programs, negative tests) are recorded in the manifest at
# baseline time and skipped consistently; a file that compiled at baseline but
# fails later is an error, as is any dump difference.

set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BASELINE_DIR="${BASELINE_DIR:-/Users/aslan/Prime/Troupe/_dev_planning/custom-operators/.baselines}"
TROUPEC="$ROOT/bin/troupec"
MODE="${1:-}"

[ "$MODE" = baseline ] || [ "$MODE" = check ] || {
  echo "usage: $0 baseline|check" >&2; exit 2; }

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

manifest="$BASELINE_DIR/manifest.txt"        # lines: OK <rel> | SKIP <rel>
mkdir -p "$BASELINE_DIR/dumps"

# Stable key for a repo-relative path.
key() { printf '%s' "$1" | tr '/' '_' ; }

compile_dump() {                             # $1 = absolute .trp; stdout = dump path or empty
  local f="$1" cwd="$WORK/c"
  rm -rf "$cwd"; mkdir -p "$cwd"
  ( cd "$cwd" && "$TROUPEC" -v "$f" -o "$cwd/x.js" >/dev/null 2>&1 ) || return 1
  [ -f "$cwd/out/out.nopats" ] || return 1
  printf '%s' "$cwd/out/out.nopats"
}

fail=0 checked=0 skipped=0

if [ "$MODE" = baseline ]; then
  : > "$manifest"
  while IFS= read -r f; do
    rel="${f#"$ROOT"/}"
    if dump="$(compile_dump "$f")" && [ -n "$dump" ]; then
      cp "$dump" "$BASELINE_DIR/dumps/$(key "$rel").nopats"
      echo "OK $rel" >> "$manifest"; checked=$((checked+1))
    else
      echo "SKIP $rel" >> "$manifest"; skipped=$((skipped+1))
    fi
  done < <(find "$ROOT/lib" "$ROOT/tests" "$ROOT/examples" -name '*.trp' | sort)
  echo "baseline: $checked snapshots, $skipped skipped -> $BASELINE_DIR"
else
  [ -f "$manifest" ] || { echo "no baseline manifest at $manifest" >&2; exit 2; }
  while read -r status rel; do
    [ "$status" = OK ] || continue
    f="$ROOT/$rel"
    if dump="$(compile_dump "$f")" && [ -n "$dump" ]; then
      if ! cmp -s "$dump" "$BASELINE_DIR/dumps/$(key "$rel").nopats"; then
        echo "DIFF $rel"; fail=$((fail+1))
      fi
    else
      echo "FAIL-TO-COMPILE $rel (compiled at baseline)"; fail=$((fail+1))
    fi
    checked=$((checked+1))
  done < "$manifest"
  echo "check: $checked compared, $fail mismatches"
  [ "$fail" -eq 0 ] || exit 1
fi
