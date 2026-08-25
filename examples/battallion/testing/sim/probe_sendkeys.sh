#!/bin/bash
# probe_sendkeys.sh -- what `send-keys -l` does with arguments the generator can produce:
# a lone ";" (tmux's own command separator), quotes, backslashes, "~", "$", and a
# multibyte character; plus what `-H` sends. Uses `cat -v` in the pane so the bytes show.
set -u
SESS="btsk-$$"
WORK="$(mktemp -d)"
cleanup(){ tmux kill-session -t "$SESS" 2>/dev/null; rm -rf "$WORK"; }
trap cleanup EXIT

tmux new-session -d -x 80 -y 12 -s "$SESS" "cat -v > $WORK/out.bin"
sleep 0.4

send() { tmux send-keys -t "$SESS" -l "$1"; sleep 0.15; }

send ';'
send 'a;b'
send "it's"
send 'back\slash'
send '~$#'
send 'λ'
tmux send-keys -t "$SESS" -H 65 c3a9 e9
sleep 0.3
tmux send-keys -t "$SESS" C-d
sleep 0.5

echo "-- pane (cat -v echo):"
tmux capture-pane -p -t "$SESS" | sed -n '1,4p'
echo "-- raw bytes received:"
od -c "$WORK/out.bin" | head -6
tmux kill-session -t "$SESS" 2>/dev/null
