#!/bin/bash
#
# Compare what the native compiler emits against what the WebAssembly build of
# the same source emits, over a corpus of programs.
#
# The pipeline from parse to emitted JavaScript is pure, and both builds come
# from one source tree, so the emitted .js is expected to be *byte-identical*.
# That makes this a differential oracle rather than a golden test: there is
# nothing to update and no judgement to make, and any difference at all is a
# failure by construction.
#
# Two things had to be true before that expectation could hold, and both are
# worth knowing if this ever starts failing:
#
#   - The gzip header's OS byte, which zlib fills in from the platform it was
#     compiled for, is normalized by IRBlob.setGzipOS. Without that, every
#     serialized blob in the output differs in exactly one byte.
#   - Source positions travel inside blobs, so both compilers must be invoked
#     on the same spelling of the path. Each program is staged as ./p.trp in
#     each compiler's own directory for that reason -- not as a convenience.
#
# Usage:  scripts/wasm-differential.sh [corpus-dir] [limit]
#
# Needs bin/troupec (make compiler), bin/troupec.wasm (make compiler-wasm) and
# wasmtime from the ghc-wasm-meta toolchain.
set -u

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
CORPUS=${1:-$ROOT/tests/rt/pos}
LIMIT=${2:-100000}
GHC_WASM_ENV=${GHC_WASM_ENV:-$HOME/.ghc-wasm/env}

NATIVE=$ROOT/bin/troupec
WASM=$ROOT/bin/troupec.wasm
WORK=$ROOT/out/wasm-differential

for f in "$NATIVE" "$WASM"; do
  [ -x "$f" ] || { echo "missing $f -- run 'make compiler' and 'make compiler-wasm'" >&2; exit 1; }
done
[ -f "$GHC_WASM_ENV" ] || { echo "no wasm toolchain at $GHC_WASM_ENV" >&2; exit 1; }
# shellcheck disable=SC1090
. "$GHC_WASM_ENV"

same=0; differ=0; wasmfail=0; skipped=0
rm -rf "$WORK"; mkdir -p "$WORK/native" "$WORK/wasm"

for f in $(find "$CORPUS" -name '*.trp' | sort | head -"$LIMIT"); do
  name=${f#"$CORPUS"/}
  # Programs importing a program-relative module need their module tree and a
  # deps file staged alongside; this harness stages one file.
  grep -q '^import "' "$f" && { skipped=$((skipped+1)); continue; }

  cp "$f" "$WORK/native/p.trp"; cp "$f" "$WORK/wasm/p.trp"
  rm -f "$WORK/native/p.js" "$WORK/wasm/p.js"

  # A program the native compiler rejects is not a differential case.
  ( cd "$WORK/native" && "$NATIVE" p.trp -o p.js ) >/dev/null 2>&1 \
    || { skipped=$((skipped+1)); continue; }

  # $TROUPE and the library tree are mounted because getExecutablePath under
  # WASI cannot locate the install root; getTroupeHome falls through to the
  # environment variable, which is the documented fallback.
  ( cd "$WORK/wasm" && wasmtime run --dir . --dir "$ROOT::/troupe" \
        --env TROUPE=/troupe "$WASM" p.trp -o p.js ) >/dev/null 2>&1 \
    || { wasmfail=$((wasmfail+1)); echo "  wasm failed to compile: $name"; continue; }

  if cmp -s "$WORK/native/p.js" "$WORK/wasm/p.js"; then
    same=$((same+1))
  else
    differ=$((differ+1))
    echo "  DIFFERS: $name"
    cmp -l "$WORK/native/p.js" "$WORK/wasm/p.js" | head -5
  fi
done

echo
echo "identical: $same   differing: $differ   wasm-failed: $wasmfail   not compared: $skipped"
[ "$differ" -eq 0 ] && [ "$wasmfail" -eq 0 ]
