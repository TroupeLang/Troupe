#!/bin/bash
# Creates the peer ids in ids/ (an existing id is kept), fills them into the files of src/
# (written to gen/) and compiles the programs to built/.
set -e
D=$(cd "$(dirname "$0")" && pwd)
W=$(cd "$D/../.." && pwd)
export TROUPE=$W
mkdir -p "$D/ids" "$D/gen" "$D/built"
for n in relay alice bob carol; do
  [ -f "$D/ids/$n.json" ] || node "$W/p2p-tools/built/mkid.mjs" --outfile="$D/ids/$n.json"
done
id() { node -p "require('$D/ids/$1.json').id"; }
R=$(id relay); A=$(id alice); B=$(id bob); C=$(id carol)
for f in "$D"/src/*; do
  sed -e "s/@@RELAY@@/$R/g" -e "s/@@ALICE@@/$A/g" -e "s/@@BOB@@/$B/g" -e "s/@@CAROL@@/$C/g" \
    "$f" > "$D/gen/$(basename "$f")"
done
for f in "$D"/gen/*.trp; do
  echo "compiling $(basename "$f")"
  (cd "$D/built" && "$W/bin/troupec" -m "$f" --output="$D/built/$(basename "$f" .trp).js")
done
