#!/bin/bash
# Hostile-peer test runner.
#
# Each test under tests/rt/hostile-peer/<name>/ pits a raw libp2p attacker
# against one victim Troupe node. Unlike the multinode tests (cooperating nodes,
# golden output), the oracle here is survival: after the attack the victim must
# still be running, exit through its own guard with the declared code, log the
# expected drop, and show no crash signature.
#
# A test dir contains:
#   victim.trp   the node under attack (prints "VICTIM: ready", then blocks;
#                a spawned guard exits with the survival code)
#   attack.mjs   one or more hostile peers; argv[2] is the victim multiaddr
#   test.json    victim port + survival exit code + attack list + oracle
#
# The victim identity is generated per run (no committed keys); ids, logs, and
# compiled artifacts are removed afterward.

set -uo pipefail

. "$(dirname "${BASH_SOURCE[0]}")/troupe-env.sh"

TESTS_DIR="$TROUPE_ROOT/tests/rt/hostile-peer"
PATTERN="${1:-}"
PASS=0
FAIL=0

run_one() {
  local dir="$1"
  local name cfg
  name=$(basename "$dir")
  cfg="$dir/test.json"
  [[ -f "$cfg" ]] || return 0
  if [[ -n "$PATTERN" && "$name" != *"$PATTERN"* ]]; then return 0; fi
  echo "Running hostile-peer test: $name"

  local port survive script extra
  port=$(jq -r '.victim.port' "$cfg")
  survive=$(jq -r '.victim.survive_exit_code' "$cfg")
  script=$(jq -r '.victim.script' "$cfg")
  extra=$(jq -r '.victim.extra_argv // ""' "$cfg")

  local idfile="$dir/victim-id.json"
  local vlog="$dir/victim.log"
  rm -f "$idfile" "$vlog"

  if ! node "$TROUPE_ROOT/p2p-tools/built/mkid.mjs" --outfile="$idfile" >/dev/null 2>&1; then
    echo "  [FAIL] could not generate victim identity (run 'make p2p-tools')"
    FAIL=$((FAIL+1)); return
  fi
  local peerid addr
  peerid=$(jq -r '.id' "$idfile")
  addr="/ip4/127.0.0.1/tcp/$port/p2p/$peerid"

  # Start the victim.
  ( "$TROUPE_ROOT/network.sh" "$dir/$script" --id "$idfile" --port "$port" $extra --no-color \
      > "$vlog" 2>&1; echo "VICTIM_EXIT=$?" >> "$vlog" ) &
  local vpid=$!

  # Wait for the victim to be ready to accept connections.
  local ready=false i
  for ((i=0; i<60; i++)); do
    if grep -q "VICTIM: ready" "$vlog" 2>/dev/null; then ready=true; break; fi
    if ! kill -0 "$vpid" 2>/dev/null; then break; fi
    sleep 0.5
  done
  if [[ "$ready" != true ]]; then
    echo "  [FAIL] victim never became ready"
    sed 's/^/    /' "$vlog" 2>/dev/null
    kill "$vpid" 2>/dev/null; wait "$vpid" 2>/dev/null
    rm -f "$idfile" "$vlog"; FAIL=$((FAIL+1)); return
  fi

  # Run each attack in turn.
  local nattacks j as
  nattacks=$(jq -r '.attacks | length' "$cfg")
  for ((j=0; j<nattacks; j++)); do
    as=$(jq -r ".attacks[$j]" "$cfg")
    node "$dir/$as" "$addr" >/dev/null 2>&1 || true
  done

  # Oracle part 1: the victim is still alive right after the attacks.
  sleep 1
  local survived_attack=true
  kill -0 "$vpid" 2>/dev/null || survived_attack=false

  # Let the victim reach its own guard and record the exit code.
  wait "$vpid" 2>/dev/null
  local exitcode
  exitcode=$(grep -o 'VICTIM_EXIT=[0-9]*' "$vlog" | tail -1 | cut -d= -f2)

  # Oracle part 2: clean survival exit, expected drop logs, no crash signature.
  local ok=true reasons=""
  [[ "$survived_attack" == true ]] || { ok=false; reasons+=" died-during-attack"; }
  [[ "$exitcode" == "$survive" ]] || { ok=false; reasons+=" exit=$exitcode(want=$survive)"; }
  local sig req
  while IFS= read -r sig; do
    [[ -z "$sig" ]] && continue
    grep -qF "$sig" "$vlog" && { ok=false; reasons+=" crash-signature:'$sig'"; }
  done < <(jq -r '.oracle.victim_must_not_contain // [] | .[]' "$cfg")
  while IFS= read -r req; do
    [[ -z "$req" ]] && continue
    grep -qF "$req" "$vlog" || { ok=false; reasons+=" missing-log:'$req'"; }
  done < <(jq -r '.oracle.victim_log_contains // [] | .[]' "$cfg")

  if [[ "$ok" == true ]]; then
    echo "  [PASS] victim survived the attack (exit $exitcode)"
    PASS=$((PASS+1))
  else
    echo "  [FAIL]$reasons"
    echo "  --- victim log ---"
    sed 's/^/    /' "$vlog"
    FAIL=$((FAIL+1))
  fi

  rm -f "$idfile" "$vlog"
}

echo "Troupe Hostile-Peer Test Suite"
echo "=============================="
shopt -s nullglob
for d in "$TESTS_DIR"/*/; do
  run_one "$d"
done

echo
echo "Hostile-peer tests: $PASS passed, $FAIL failed"
[[ "$FAIL" -eq 0 ]]
