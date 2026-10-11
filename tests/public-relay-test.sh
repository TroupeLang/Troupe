#!/bin/bash
set -euo pipefail
# Runs the basic-echo pair through each given relay, in relay-only mode, with
# throwaway identities. Defaults to the two public relays. Not part of make ci.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TROUPE_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
export TROUPE="${TROUPE:-$TROUPE_ROOT}"

RELAYS=("$@")
if [[ ${#RELAYS[@]} -eq 0 ]]; then
    RELAYS=(
        "/dns4/relay-2.troupe-lang.net/tcp/5555/ws/p2p/12D3KooWSg5fxhEZ4SMtVCwfj81iBUNo15o3d3rEriTAVE6Lubgp"
        "/dns4/relay-sfo.troupe-lang.net/tcp/5555/ws/p2p/12D3KooWS5pvNcKKGevDutCU4RGAbzAtGgVt7PkQdGse8FK9jZKo"
    )
fi

TEST_DIR="$TROUPE_ROOT/tests/rt/multinode-tests/basic-echo"
MKID="$TROUPE_ROOT/p2p-tools/built/mkid.mjs"
if [[ ! -f "$MKID" ]]; then
    echo "ERROR: p2p-tools not built. Run 'make p2p-tools' first."
    exit 1
fi

TEMP_DIR=$(mktemp -d)
trap 'rm -rf "$TEMP_DIR"' EXIT

mkid() {
    node "$MKID" --outfile="$TEMP_DIR/$1.json" >/dev/null
    node -p "JSON.parse(require('fs').readFileSync('$TEMP_DIR/$1.json')).id"
}
SERVER_ID=$(mkid server)
CLIENT_ID=$(mkid client)
echo "{\"client\":\"$CLIENT_ID\",\"server\":\"$SERVER_ID\"}" > "$TEMP_DIR/aliases.json"
trustmap() {
    echo "[{\"id\":\"$1\",\"level\":{\"confidentiality\":[],\"integrity\":[[]]}}]" > "$TEMP_DIR/$2-trustmap.json"
}
trustmap "$CLIENT_ID" server
trustmap "$SERVER_ID" client

node_args() {
    echo --id "$TEMP_DIR/$1.json" --aliases "$TEMP_DIR/aliases.json" \
         --trustmap "$TEMP_DIR/$1-trustmap.json" --relay "$2" --relay-only --no-color
}

failed=0
for relay in "${RELAYS[@]}"; do
    echo "=== $relay"
    timeout 60 "$TROUPE_ROOT/network.sh" "$TEST_DIR/echo-server.trp" --port 6889 \
        $(node_args server "$relay") > "$TEMP_DIR/server.out" 2>&1 &
    server_pid=$!
    sleep 2
    set +e
    timeout 60 "$TROUPE_ROOT/network.sh" "$TEST_DIR/echo-client.trp" --port 6890 \
        $(node_args client "$relay") > "$TEMP_DIR/client.out" 2>&1
    client_code=$?
    wait $server_pid
    server_code=$?
    set -e
    if [[ $client_code -eq 0 && $server_code -eq 0 ]]; then
        echo "PASS"
    else
        echo "FAIL: client exit $client_code, server exit $server_code"
        echo "--- server"; cat "$TEMP_DIR/server.out"
        echo "--- client"; cat "$TEMP_DIR/client.out"
        failed=1
    fi
done
exit $failed
