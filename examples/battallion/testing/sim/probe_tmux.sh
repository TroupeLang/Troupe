#!/bin/bash
# probe_tmux.sh -- verify this tmux behaves the way the simulator assumes.
#
# Checks, in order:
#   1. new-session -d -x/-y gives a pane of exactly that geometry
#   2. remain-on-exit keeps the pane and exposes #{pane_dead_status}
#   3. send-keys -l delivers literal text (incl. multibyte) as raw bytes
#   4. send-keys <NamedKey> delivers the escape sequences Key.trp decodes
#   5. capture-pane -p returns the screen (trailing blank rows dropped)
#   6. display-message -p '#{cursor_x} #{cursor_y}' tracks the cursor
#   7. resize-window -x -y reaches the program as SIGWINCH -> TTY_RESIZE
#
# Run:  bash probe_tmux.sh
set -u

SESS="btprobe-$$"
WORK="$(mktemp -d)"
ROOT="${BT_ROOT:-$(cd "$(dirname "$0")/../../../.." && pwd)}"
BT="${BT_JS:-${BT_SCRATCH:-/tmp/battallion-sim}/bt-current.js}"

pass=0; fail=0
ok()   { echo "  PASS  $1"; pass=$((pass+1)); }
bad()  { echo "  FAIL  $1"; fail=$((fail+1)); }
check(){ if [ "$2" = "$3" ]; then ok "$1"; else bad "$1 (want [$3] got [$2])"; fi; }

cleanup() { tmux kill-session -t "$SESS" 2>/dev/null; rm -rf "$WORK"; }
trap cleanup EXIT

printf 'hello world\nsecond line\nthird\n' > "$WORK/probe.txt"

echo "== tmux $(tmux -V)"

# ---------------------------------------------------------------- 1, 2: geometry + exit status
echo "-- probe A: geometry, remain-on-exit, exit status (cat, no editor)"
tmux new-session -d -x 40 -y 10 -s "$SESS" 'sh -c "exit 7"'
tmux set-option -t "$SESS" remain-on-exit on >/dev/null
tmux new-window -t "$SESS" -n geom 'sleep 30'
check "pane width"  "$(tmux display-message -p -t "$SESS:geom" '#{pane_width}')"  "40"
check "pane height" "$(tmux display-message -p -t "$SESS:geom" '#{pane_height}')" "10"
tmux kill-session -t "$SESS" 2>/dev/null

# remain-on-exit must be set *before* the process exits; set it as a session option at creation
tmux new-session -d -x 40 -y 10 -s "$SESS" -e FOO=bar 'sh -c "exit 7"' \; set-option -t "$SESS" remain-on-exit on 2>/dev/null
sleep 0.4
st="$(tmux display-message -p -t "$SESS" '#{pane_dead} #{pane_dead_status}' 2>/dev/null)"
if [ "$st" = "1 7" ]; then ok "remain-on-exit exposes exit status ($st)"
else bad "remain-on-exit / pane_dead_status (got [$st], want [1 7])"; fi
tmux kill-session -t "$SESS" 2>/dev/null

# ---------------------------------------------------------------- 3..7 against the real editor
echo "-- probe B: the editor under tmux"
tmux new-session -d -x 40 -y 10 -s "$SESS" -c "$ROOT" \
  "node $ROOT/rt/built/troupe.mjs -f=$BT --localonly --io-root $WORK -- probe.txt 2>$WORK/err.log"
tmux set-option -t "$SESS" remain-on-exit on >/dev/null
sleep 3

cap() { tmux capture-pane -p -t "$SESS"; }
cur() { tmux display-message -p -t "$SESS" '#{cursor_x} #{cursor_y}'; }

first="$(cap | sed -n 1p)"
check "first frame row 1"    "$first" "hello world"
check "status row"           "$(cap | sed -n 10p)" "probe.txt  NORMAL  1,1"
check "tilde filler row 6"   "$(cap | sed -n 6p)"  "~"
check "cursor at origin"     "$(cur)" "0 0"

# 3: literal send-keys, ASCII
tmux send-keys -t "$SESS" -l 'jjll'
sleep 0.6
check "hjkl motion -> cursor" "$(cur)" "2 2"
check "status after motion"   "$(cap | sed -n 10p)" "probe.txt  NORMAL  3,3"

# named keys
tmux send-keys -t "$SESS" Up Up Left
sleep 0.6
check "named Up/Left keys"    "$(cur)" "1 0"

tmux send-keys -t "$SESS" End
sleep 0.4
check "named End"             "$(cur)" "11 0"
tmux send-keys -t "$SESS" Home
sleep 0.4
check "named Home"            "$(cur)" "0 0"

# 3b: literal multibyte through send-keys -l  (insert mode)
tmux send-keys -t "$SESS" -l 'i'
sleep 0.3
tmux send-keys -t "$SESS" -l 'héllo·λ'
sleep 0.8
row1="$(cap | sed -n 1p)"
if [ "$row1" = "héllo·λhello world" ]; then ok "multibyte literal insert [$row1]"
else bad "multibyte literal insert (got [$row1])"; fi
check "status shows INSERT + col" "$(cap | sed -n 10p)" "probe.txt [+]  INSERT  1,8"

# Esc, then undo everything typed (7 code points -> 7 undos)
tmux send-keys -t "$SESS" Escape
sleep 0.3
tmux send-keys -t "$SESS" -l 'uuuuuuu'
sleep 1.0
check "undo run restores row 1" "$(cap | sed -n 1p)" "hello world"

# 7: resize
tmux resize-window -t "$SESS" -x 30 -y 6
sleep 1.2
check "resize: pane height"  "$(tmux display-message -p -t "$SESS" '#{pane_height}')" "6"
check "resize: rows drawn"   "$(cap | sed -n 5p)" "~"
check "resize: status row"   "$(cap | sed -n 6p)" "probe.txt  NORMAL  1,1"

# capture-pane and trailing blank rows: put the cursor on the last buffer line (empty line 4)
tmux send-keys -t "$SESS" -l 'jjj'
sleep 0.6
echo "  INFO  capture-pane line count at 30x6: $(cap | wc -l | tr -d ' ')"
echo "  INFO  capture-pane -N line count:      $(tmux capture-pane -p -N -t "$SESS" | wc -l | tr -d ' ')"

# long line truncation + cursor clamp: type a long line at width 30
tmux send-keys -t "$SESS" -l 'i'
sleep 0.3
tmux send-keys -t "$SESS" -l '0123456789012345678901234567890123456789'
sleep 1.5
echo "  INFO  row with long line: [$(cap | sed -n 1p)]"
echo "  INFO  cursor at long line: [$(cur)]  status: [$(cap | tail -n 1)]"

# quit refusal on a dirty buffer
tmux send-keys -t "$SESS" Escape
sleep 0.3
tmux send-keys -t "$SESS" -l 'q'
sleep 0.6
echo "  INFO  dirty-quit status row: [$(cap | tail -n 1)]"
alive="$(tmux display-message -p -t "$SESS" '#{pane_dead}')"
check "dirty q refused (pane alive)" "$alive" "0"

# :q! exit status
tmux send-keys -t "$SESS" -l ':q!'
tmux send-keys -t "$SESS" Enter
sleep 2
st="$(tmux display-message -p -t "$SESS" '#{pane_dead} #{pane_dead_status}')"
check ":q! exits 0" "$st" "1 0"

echo
echo "== $pass passed, $fail failed"
[ "$fail" = 0 ]
