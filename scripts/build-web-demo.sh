#!/bin/bash
#
# Assembles the browser demo as a static site under build/web-demo: the page
# and its script, the sample programs, the four bundles (make browser), the
# compiler (make compiler-wasm), the compiled libraries with their
# interfaces, the service module and the terminal's files. The directory is
# what examples/web-demo/serve.mjs serves and what a static host publishes.
set -eu

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
SITE="$ROOT/build/web-demo"
DEMO="$ROOT/examples/web-demo"

for f in bin/troupec.wasm build/browser/troupe-worker.mjs build/browser/compiler-worker.mjs \
         build/browser/troupe-page.mjs build/browser/troupe-editor.mjs trp-rt/out/service.js node_modules/@xterm/xterm/lib/xterm.js; do
  if [ ! -f "$ROOT/$f" ]; then
    echo "build-web-demo: $f is missing (make compiler-wasm, make browser, npm install)" >&2
    exit 1
  fi
done

rm -rf "$SITE"
mkdir -p "$SITE/lib/out" "$SITE/trp-rt/out" "$SITE/xterm/lib" "$SITE/xterm/css"
cp "$DEMO/index.html" "$DEMO/demo.mjs" "$DEMO/theme.mjs" "$DEMO/style.css" "$DEMO/favicon.svg" "$SITE/"
cp -R "$DEMO/samples" "$SITE/samples"
cp "$ROOT"/build/browser/*.mjs "$SITE/"
cp "$ROOT/bin/troupec.wasm" "$SITE/troupec.wasm"
cp "$ROOT"/lib/out/*.js "$ROOT"/lib/out/*.exports "$SITE/lib/out/"
cp "$ROOT/trp-rt/out/service.js" "$SITE/trp-rt/out/"
cp "$ROOT/node_modules/@xterm/xterm/lib/xterm.js" "$ROOT/node_modules/@xterm/addon-fit/lib/addon-fit.js" "$SITE/xterm/lib/"
cp "$ROOT/node_modules/@xterm/xterm/css/xterm.css" "$SITE/xterm/css/"

# The library interfaces the compiler mounts, as a manifest the page reads.
(cd "$ROOT/lib/out" && ls *.exports | sort | node -e '
  const names = require("fs").readFileSync(0, "utf8").trim().split("\n");
  process.stdout.write(JSON.stringify(names));
') > "$SITE/lib-exports.json"

# GitHub Pages runs Jekyll over a site unless told not to.
touch "$SITE/.nojekyll"

echo "web demo assembled in $SITE ($(du -sh "$SITE" | cut -f1))"
