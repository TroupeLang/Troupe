#!/bin/sh
#
# The Troupe half of the IR interchange check.
#
# `ir-sexp-conformance-test` runs the interchange laws inside the Haskell
# implementation; this runs them inside the second one. trp-compiler/conformance.trp
# reads every document in the conformance corpus and checks, with Troupe's own
# structural equality:
#
#   D  (datum layer)  parse (print d) = d
#   L1 (round trip)   decode (encode x) = x, positions included
#
# Decoding a Haskell-written document at all is the text half of L2; the value
# half is observed by handing this implementation's re-print back to troupec,
# which is what the corpus commit records.
#
# Run from anywhere:  ./scripts/ir-sexp-troupe-conformance.sh
# Needs bin/troupec, rt/built and lib/out built.

set -e

root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"

for f in bin/troupec rt/built/troupe.mjs; do
    if [ ! -e "$f" ]; then
        echo "Error: $f missing; run 'make all'" >&2
        exit 1
    fi
done

corpus=compiler/test/ir-sexp-conformance/data
docs=$(ls "$corpus"/*.sexp)
if [ -z "$docs" ]; then
    echo "Error: no documents in $corpus" >&2
    exit 1
fi

tmp=$(mktemp)
trap 'rm -f "$tmp" "$tmp.js"' EXIT
./bin/troupec trp-compiler/conformance.trp -m --output="$tmp.js"

# --io-root is the repository root, so the program receives the corpus paths as
# it sees them here; the documents themselves are read-only inputs.
node rt/built/troupe.mjs -f="$tmp.js" --localonly \
     --suppress-local-info-message --suppress-main-thread-finished-message \
     --io-root="$root" -- $docs
