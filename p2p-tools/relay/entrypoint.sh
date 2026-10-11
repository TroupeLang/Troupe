#!/bin/bash
set -euo pipefail

KEYS_DIR="/app/keys"
ID_FILE="${KEYS_DIR}/relay.id"
PRIV_FILE="${KEYS_DIR}/relay.priv"
PUBLIC_PORT="${RELAY_PUBLIC_PORT:-5555}"

# Generate keys on first run. mkid refuses to overwrite, so clear a partial pair first.
if [ ! -f "$ID_FILE" ] || [ ! -f "$PRIV_FILE" ]; then
    echo "=== First run: generating relay keys ==="
    mkdir -p "$KEYS_DIR"
    rm -f "$ID_FILE" "$PRIV_FILE"
    node /app/p2p-tools/built/mkid.mjs --privkeyfile="$PRIV_FILE" --idfile="$ID_FILE" --verbose
    echo "=== Keys generated ==="
fi

PEER_ID=$(cat "$ID_FILE")
echo ""
echo "============================================"
echo "  Troupe 2 relay"
echo ""
echo "  Peer ID:"
echo "  ${PEER_ID}"
echo ""
echo "  Multiaddr (use the host's IP or name):"
echo "  /ip4/<HOST_IP>/tcp/${PUBLIC_PORT}/ws/p2p/${PEER_ID}"
echo "  /dns4/<HOST_NAME>/tcp/${PUBLIC_PORT}/ws/p2p/${PEER_ID}"
echo "============================================"
echo ""

# exec so that signals reach node directly
exec node /app/p2p-tools/relay/relay.mjs --port=5555 --id-file="$ID_FILE" --priv-file="$PRIV_FILE"
