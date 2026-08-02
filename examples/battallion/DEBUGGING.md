# Debugging battallion (and other full-screen Troupe programs) by automation

How to drive, observe, and pin the behaviour of a terminal program that cannot be debugged by
reading its stdout: the tool layers, when to use which, and the traps. Commands are written for
this checkout's root; the test tooling lives in `tests/_unautomated/claude/` (git-ignored — local
tooling, not part of the corpus).

None of it needs a human at the keyboard. Every layer is a programmatic interface, so the whole
loop — reproduce, minimise, fix, pin — runs from a symptom described in words. A human enters
where perception is the instrument (does it *feel* right, does a real emulator render it right),
or to watch any tmux session live (`tmux attach`) while the automation drives it;
`tests/_unautomated/claude/battallion-drive/bt-tmux.sh` wraps a session into
start / type / keys / snap / attach / stop for that kind of poking.

## The layers

| Layer                | Use it for                                                        | Where |
|----------------------|-------------------------------------------------------------------|-------|
| One-shot run         | Startup behaviour: arguments, refusals, exit codes, stderr       | `./local.sh` |
| tmux session         | Interactive debugging, by hand or scripted — and watchable live  | `tmux` |
| File-driven session  | Scripted sessions with screen snapshots as files                 | `battallion-drive/driver.py` |
| Scripted harnesses   | Pinning a behaviour so it stays fixed                            | `battallion-{viewer,editor,plugins,cli}/harness.py` |
| Session simulator    | Finding what no scripted case thought to try                     | `battallion-sim/` |

## One-shot runs

Startup behaviour needs no terminal at all: the editor refuses bad invocations before touching
the terminal, on stderr, with exit 1.

```sh
./local.sh examples/battallion/bt.trp --io-root /tmp/w -- notes.txt   # normal invocation
./local.sh examples/battallion/bt.trp --io-root /tmp/w --             # "no file to open" + usage
echo $?                                                                # 1
```

`local.sh` compiles and runs; for a fixed build compile once and run node directly — but from the
checkout root, because the module resolver reads the `.deps.json` pin file relative to the working
directory:

```sh
bin/troupec examples/battallion/bt.trp -m --output=/tmp/bt.js
node rt/built/troupe.mjs -f=/tmp/bt.js --localonly --io-root /tmp/w -- notes.txt
```

## tmux: the interactive debugger

tmux is a real terminal emulator with a command-line remote control. One session can be driven by
a script and *watched live by a human at the same time* (`tmux attach`), which is the fastest way
to see what a bug report means.

```sh
# Start the editor in a detached 80x24 session.
tmux new-session -d -x 80 -y 24 -s bt \
  'node rt/built/troupe.mjs -f=/tmp/bt.js --localonly --io-root /tmp/w -- notes.txt'

tmux send-keys -t bt -l 'jjjix'      # -l = literal characters, exactly these bytes
tmux send-keys -t bt Escape          # named keys: Escape Enter Up Down BSpace Tab PageUp ...
tmux capture-pane -t bt -p           # the screen, as text (trailing blank rows dropped)
tmux display-message -p -t bt '#{cursor_x} #{cursor_y}'   # cursor, tracked even while hidden
tmux resize-window -t bt -x 100 -y 40                     # arrives as a TTY_RESIZE event
tmux attach -t bt                    # watch (and type) live; detach with C-b d
tmux kill-session -t bt
```

Recording the raw escape stream while a session runs:

```sh
tmux pipe-pane -t bt -o 'cat > /tmp/bt-raw.log'    # toggle off by running it again
cat -v /tmp/bt-raw.log                             # escapes made visible
```

Traps, measured on tmux 3.5a: a `send-keys -l` argument that is exactly `;` is eaten as tmux's
command separator (send `\;`); `capture-pane` drops trailing blank rows (pad if comparing);
`set-option remain-on-exit on` keeps a dead pane inspectable and exposes
`#{pane_dead_status}` (the exit code).

## The file-driven session (no tmux needed)

`tests/_unautomated/claude/battallion-drive/driver.py` holds the editor on a pty, renders its
output through a screen model covering the escapes `Screen.trp` documents, and is controlled
entirely through files — usable from any tool that can append to a file and read one back.

```sh
python3 tests/_unautomated/claude/battallion-drive/driver.py \
        "$PWD" /tmp/session /tmp/bt.js /tmp/w notes.txt 24 80 &   # rows cols

printf 'text:jjj\n'      >> /tmp/session/cmd.txt   # then read the next /tmp/session/snap-NNN.txt
printf 'key:esc\n'       >> /tmp/session/cmd.txt   # enter esc tab backspace up down left right
printf 'ctrl:c\n'        >> /tmp/session/cmd.txt   #   pgup pgdn home end
printf 'raw:aTsb\n'      >> /tmp/session/cmd.txt   # base64 bytes, for payloads with spaces
printf 'resize:10x40\n'  >> /tmp/session/cmd.txt
printf 'quit\n'          >> /tmp/session/cmd.txt   # SIGTERM
```

A snapshot is written automatically ~300 ms after the output goes quiet: the numbered grid, the
cursor, alt-screen, automatic-wrap and cursor-visibility state, and any escape the model did not
recognise.
`status.txt` reads `RUNNING` or `EXITED <code>`; `out.raw` is the raw byte log. The driver's
screen model is simpler than tmux's — prefer tmux when the escapes in play might go beyond what
`Screen.trp` emits.

## Pinning a behaviour: the harnesses

Four python pty harnesses cover startup, motions, editing/saving/undo, the plugin frame shape,
and the invocation/path/content edges. Run any of them as
`python3 tests/_unautomated/claude/battallion-<name>/harness.py "$PWD"`.

The discipline for a new case: reproduce the behaviour by hand first (tmux or the driver), then
assert exactly what was observed — never write the expectation from belief. On darwin the pty is
revoked when its owning child exits, so termios and screen state must be asserted while the child
is alive; a post-mortem read sees the reset default.

## Finding the unknown: the simulator

`tests/_unautomated/claude/battallion-sim/` generates seeded sessions of realistic activity and
checks every action against a shadow model of the editor's documented semantics — screen, cursor,
status fields, and the saved file byte-for-byte. When it disagrees, it saves a replay log and
minimises the failing prefix automatically.

```sh
cd tests/_unautomated/claude/battallion-sim
python3 simulate.py run --seed 3 --file large --actions 400
python3 simulate.py replay <runs>/<tag>/replay.jsonl --upto N     # reproduce, step-bounded
bash campaign.sh par                                              # the full nine-run campaign
python3 report.py                                                 # counts, latency percentiles
```

It also measures keystroke→stable-frame latency (first campaign: p50 22 ms, p95 59 ms). The model
cites the source line for every rule it implements, so when the editor and the model disagree,
the citation says which one is wrong.

The simulator runs a `.js` it does not compile — `<scratch>/bt-current.js`, the path
`tmuxdrv.py` and `campaign.sh` name. After an edit to an editor source, recompile it
(`bin/troupec examples/battallion/bt.trp -m --output=<scratch>/bt-current.js`) or the run reports
`DIVERGENCE at action 0 (startup): editor exited`; the reason is in the run's `stderr.log`, and
for a stale build it is the module-pin refusal `cannot link module <hash>: it is not among this
program's dependencies`.

## General traps

- **Run from the checkout root**: `.deps.json` resolution is cwd-relative.
- **Rebuild what changed**: editor sources need only `bin/troupec` (recompile the `.js`);
  `rt/src` changes need `/usr/bin/make rt`; goldens run the *installed* `bin/troupec`.
- **A wedged terminal** after killing a raw-mode session by hand: `stty sane`, or let the
  editor's own teardown run (`q`, `:q!`, or SIGTERM — trapped and restored).
- **Timeouts around interactive runs**: SIGTERM is honoured (the supervisor traps it and
  restores), so `timeout N` wrappers terminate sessions cleanly.
- **Bytes on the wire**: input arrives latin1 (one code unit per byte) via `Tty.TTY_DATA`;
  what the decoder makes of any byte sequence can be checked in isolation with
  `./local.sh examples/battallion/keydemo.trp --localonly` or a `Key.decode` probe.
