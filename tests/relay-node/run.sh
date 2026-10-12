#!/bin/bash
# run.sh local [CASE...]         the relay node on 127.0.0.1:PORT and the clients here
# run.sh remote HOST PORT CASE   the clients here; the relay node of CASE runs on HOST
# run.sh relay CASE PORT         the relay node of CASE in the foreground (to run on HOST)
# CASE is no-gater, list, time-limit, labels, limits, remove-end, remove-keep or idle. local runs
# all but idle by default. PORT (default 7560) is the relay node's WebSocket port; the relay
# node's TCP port is PORT+1 and the clients alice, bob and carol listen on PORT+2 to PORT+4.
# CLIENT_DEBUG (default libp2p:circuit-relay*) is the clients' DEBUG.
D=$(cd "$(dirname "$0")" && pwd)
W=$(cd "$D/../.." && pwd)
export TROUPE=$W
LOG=${LOG:-$(mktemp -d /tmp/relay-node.XXXXXX)}
T0=$(python3 -c 'import time; print(time.time())')

stamp() { python3 -c "import time; print('%.2f' % (time.time() - $T0))"; }
say() { echo "[t=$(stamp)s] $*"; }

relay_command() { # relay_command CASE PORT: sets CMD to the relay node's command line
  local program=relay-$1 trust=trust-bot.json extra=()
  case $1 in
    no-gater|list|time-limit|limits|remove-end|remove-keep) ;;
    labels) trust=trust-mixed.json ;;
    idle) program=relay-list ;;
    *) echo "unknown case: $1" >&2; exit 2 ;;
  esac
  CMD=("$W/rt/troupe" "$D/built/$program.js" --id "$D/ids/relay.json" --start-relay "$2"
       --port $(($2 + 1)) --aliases "$D/gen/aliases.json" --trustmap "$D/gen/$trust"
       "${extra[@]}")
}

start() { # start NAME CMD...: runs CMD in the background; stdout to NAME.out, stderr to NAME.err
  local name=$1; shift
  say "start $name: $*"
  "$@" > "$LOG/$name.out" 2> "$LOG/$name.err" &
}

client() { # client NAME PROGRAM
  local offset; case $1 in alice) offset=2 ;; bob) offset=3 ;; carol) offset=4 ;; esac
  start "$1" env "DEBUG=${CLIENT_DEBUG:-libp2p:circuit-relay*}" "$W/rt/troupe" "$D/built/$2.js" \
    --id "$D/ids/$1.json" --port $((PORT + offset)) --relay "$RELAY_ADDR" --relay-only \
    --aliases "$D/gen/aliases.json" --trustmap "$D/gen/trust-bot.json"
}

running() { pgrep -f -- "--id $D/ids/$1.json" > /dev/null; }

wait_exit() { # wait_exit NAME SECONDS
  for _ in $(seq 1 $(($2 * 4))); do
    running "$1" || { say "$1 ended"; return; }
    sleep 0.25
  done
  say "$1 still running after $2 s"
}

wait_for() { # wait_for NAME PATTERN SECONDS: waits for PATTERN in NAME's stdout
  for _ in $(seq 1 $(($3 * 4))); do
    grep -q "$2" "$LOG/$1.out" 2>/dev/null && { say "$1 printed '$2'"; return; }
    sleep 0.25
  done
  say "$1 did not print '$2' within $3 s"
}

stop_all() {
  say "stopping every node started here"
  pkill -f "$D/built/"; sleep 1; pkill -9 -f "$D/built/"
  echo "node processes left: $(pgrep -f "$D/built/" | wc -l | tr -d ' ')"
}

show() { # show NAME [all]: NAME's stdout, and its stderr filtered (or whole with "all")
  local filter='reserv|denied|PERMISSION|NO_RESERVATION|gater|status|error|abort|timeout|clos|disconnect|succeeded'
  echo; echo "===== $1: stdout ====="; cat "$LOG/$1.out"
  echo "===== $1: stderr, $(wc -l < "$LOG/$1.err" | tr -d ' ') lines$([ "$2" = all ] || echo ", lines matching $filter") ====="
  if [ "$2" = all ]; then cut -c1-400 "$LOG/$1.err"; else grep -iE "$filter" "$LOG/$1.err" | cut -c1-400; fi
}

expect() { echo "EXPECTED: $*"; }

case_no-gater() {
  expect "bob and alice PERMISSION_DENIED; alice's dial to bob fails (NO_RESERVATION); alice times out (exit 124)"
  client bob echo-server
  client alice echo-client
  wait_exit alice 40
}

case_list() {
  expect "bob and alice reserve; alice prints 'CLIENT: reply hello'; the relay program receives the connection question (alice, bob); carol PERMISSION_DENIED"
  client bob echo-server
  client alice echo-client
  wait_exit alice 40
  client carol client-idle
  wait_exit carol 20
}

case_time-limit() {
  expect "alice PERMISSION_DENIED about 2 s after requesting the reservation, before its own 5 s"
  client alice client-idle
  wait_exit alice 20
}

case_labels() {
  expect "the relay prints the three ids at the pc of its connectionGater call, {}, although its trust map puts alice at integrity alice and leaves carol out; all three reserve"
  client bob client-idle; sleep 2
  client alice client-idle; sleep 2
  client carol client-idle
  wait_exit carol 20
}

case_limits() {
  expect "setLimits OK; bob reserves, its reservation expires 20 s later and the relay lists none; bob's refresh 30 s after the reservation is a new question and a new 20 s reservation"
  client bob client-hold
  wait_exit bob 80
}

case_remove() { # bob serves echo; alice sends 20 requests, one a second
  client bob echo-server
  client alice echo-client-loop
  wait_exit alice 45
}

case_remove-end() {
  expect "the relay removes bob's reservation with endCircuits = true; the relayed connection ends and the relay answers alice's next dial to bob through it with NO_RESERVATION; on loopback alice then dials bob directly, at the address identify gave it, and gets all 20 replies"
  case_remove
}

case_remove-keep() {
  expect "the relay removes bob's reservation with endCircuits = false; the relay lists no reservation for bob, and alice gets all 20 replies over the open relayed connection"
  case_remove
}

case_idle() {
  expect "bob and alice reserve, stay idle 600 s, then alice prints 'CLIENT: reply hello'; after the relay node's SIGTERM both report the loss of the relay"
  client bob echo-server
  client alice echo-client-late
  wait_for alice "CLIENT: reply" 700
  if [ "$MODE" = local ]; then
    say "SIGTERM to the relay node"; pkill -TERM -f "troupe.mjs -f=$D/built/relay-"
  else
    say "waiting for the relay node on $HOST to stop (send it SIGTERM there)"
    for _ in $(seq 1 480); do nc -z "$HOST" "$PORT" 2>/dev/null || break; sleep 0.25; done
    say "relay port $HOST:$PORT closed"
  fi
  sleep 30
}

run_case() { # run_case CASE: the clients of CASE, then the observed lines
  echo; echo "########## case $1 ($MODE)"
  "case_$1"
  stop_all
  [ "$MODE" = local ] && show relay
  for n in bob alice carol; do
    [ -f "$LOG/$n.out" ] && show $n $([ "$1" = idle ] && echo all)
  done
  rm -f "$LOG"/*.out "$LOG"/*.err
}

MODE=$1
case $MODE in
  relay)
    relay_command "$2" "$3"
    echo "\$ DEBUG='libp2p:circuit-relay:server*' ${CMD[*]}"
    DEBUG='libp2p:circuit-relay:server*' exec "${CMD[@]}" ;;
  local)
    shift; PORT=${PORT:-7560}
    RELAY_ADDR=/ip4/127.0.0.1/tcp/$PORT/ws/p2p/$(node -p "require('$D/gen/aliases.json').relay")
    CASES=("$@")
    [ ${#CASES[@]} -eq 0 ] && CASES=(no-gater list time-limit labels limits remove-end remove-keep)
    for c in "${CASES[@]}"; do
      relay_command "$c" $PORT
      start relay env 'DEBUG=libp2p:circuit-relay:server*' "${CMD[@]}"
      wait_for relay "RELAY: " 20
      run_case "$c"
    done ;;
  remote)
    HOST=$2; PORT=$3
    RELAY_ADDR=/ip4/$HOST/tcp/$PORT/ws/p2p/$(node -p "require('$D/gen/aliases.json').relay")
    nc -z "$HOST" "$PORT" || { echo "no relay node listening on $HOST:$PORT"; exit 1; }
    run_case "$4" ;;
  *) sed -n 2,7p "$0"; exit 2 ;;
esac
