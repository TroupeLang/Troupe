#!/bin/sh
# bt-tmux.sh — a one-command tmux debug session for battallion.
#
#   bt-tmux.sh start <file> [<rows>x<cols>]   compile bt, open <file> in a detached session
#   bt-tmux.sh type '<literal>'               send exact characters (quote them)
#   bt-tmux.sh keys <Key> [...]               named keys: Escape Enter Up Down BSpace PageUp ...
#   bt-tmux.sh snap                           the screen + cursor position + liveness
#   bt-tmux.sh resize <rows>x<cols>
#   bt-tmux.sh rec [<path>]                   toggle raw escape recording (default /tmp/btdbg.raw)
#   bt-tmux.sh attach                         watch live; detach with C-b d
#   bt-tmux.sh stop                           kill the session
#
# The file is created under the io-root if it does not exist; the io-root is
# ${BT_IOROOT:-/tmp/btdbg-io}. Session name: ${BT_SESSION:-btdbg}.
set -e

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../../../.." && pwd)"
S="${BT_SESSION:-btdbg}"
IOROOT="${BT_IOROOT:-/tmp/btdbg-io}"
JS=/tmp/btdbg-bt.js

case "$1" in
  start)
    FILE="${2:?usage: bt-tmux.sh start <file> [<rows>x<cols>]}"
    GEOM="${3:-24x80}"
    ROWS="${GEOM%x*}"; COLS="${GEOM#*x}"
    mkdir -p "$IOROOT"
    [ -e "$IOROOT/$FILE" ] || printf 'one\ntwo\nthree\n' > "$IOROOT/$FILE"
    "$ROOT/bin/troupec" "$ROOT/examples/battallion/bt.trp" -m --output="$JS"
    tmux kill-session -t "$S" 2>/dev/null || true
    tmux new-session -d -x "$COLS" -y "$ROWS" -s "$S" \
      "cd '$ROOT' && node rt/built/troupe.mjs -f='$JS' --localonly --io-root '$IOROOT' -- '$FILE'"
    tmux set-option -t "$S" remain-on-exit on
    echo "session $S: $FILE at ${ROWS}x${COLS}, io-root $IOROOT"
    ;;
  type)  shift; tmux send-keys -t "$S" -l "$@" ;;
  keys)  shift; tmux send-keys -t "$S" "$@" ;;
  snap)
    if [ "$(tmux display-message -p -t "$S" '#{pane_dead}')" = "1" ]; then
      echo "== exited $(tmux display-message -p -t "$S" '#{pane_dead_status}')"
    else
      echo "== alive, cursor $(tmux display-message -p -t "$S" '#{cursor_x},#{cursor_y}') (x,y from 0)"
    fi
    tmux capture-pane -t "$S" -p | cat -n
    ;;
  resize)
    GEOM="${2:?rows x cols, e.g. 10x40}"
    tmux resize-window -t "$S" -x "${GEOM#*x}" -y "${GEOM%x*}"
    ;;
  rec)   tmux pipe-pane -t "$S" -o "cat > ${2:-/tmp/btdbg.raw}"; echo "recording toggled" ;;
  attach) exec tmux attach -t "$S" ;;
  stop)  tmux kill-session -t "$S" ;;
  *) sed -n '2,16p' "$0"; exit 1 ;;
esac
