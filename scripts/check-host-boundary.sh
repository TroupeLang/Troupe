#!/bin/bash
#
# The runtime core imports nothing of Node's. Every contact with the platform
# goes through a registered host interface (CodeStore, ConfigStore, Terminal,
# ResultChannel, CompilerService, NetworkProvider and the rest under rt/src),
# and the node host under rt/src/hosts/node is the one place that imports
# Node's modules, together with the node side of the native modules, the
# libp2p stack, the node entry point, and the harnesses and tools.
#
# A core module that imports one of these breaks every other host at import
# time, before any program runs. This check fails the build on the import.
set -u

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT/rt/src"

HOST_PATHS='^\./(hosts/|ffi/node/|p2p/|proptests/|tools/|dev/|troupe\.mts)'
NODE_IMPORT="from '(node:[a-z_/]+|fs|path|os|net|zlib|readline|child_process|url|util|assert|console|yargs|winston)'"

violations=$(grep -rnE --include='*.mts' --include='*.mjs' "$NODE_IMPORT" . | grep -vE "$HOST_PATHS")

if [ -n "$violations" ]; then
  echo "runtime core imports a Node module (rt/src, outside the host directories):" >&2
  echo "$violations" >&2
  exit 1
fi
echo "host boundary: no Node import in the runtime core"
