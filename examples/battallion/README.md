# battallion

A terminal text editor written in Troupe. Under construction; the stages and what each one
delivers are in `_dev_planning/text-editor/mvp-plan.md`. At this stage it is a viewer: it opens a
file, draws it, and navigates it. Editing, modes, a status line and saving are stage 5.

| File             | What it is                                                                          |
|------------------|-------------------------------------------------------------------------------------|
| `bt.trp`         | The editor: argument handling and the file read, then a session                      |
| `Session.trp`    | The process constellation — supervisor, kernel, renderer — and the terminal handling |
| `Editor.trp`     | The viewer's state and its keymap: cursor, viewport, motions, frames                 |
| `Screen.trp`     | The terminal escapes and the assembly of one frame                                   |
| `Key.trp`        | Decodes terminal input bytes into `key` values, with a carry across chunk boundaries |
| `keydemo.trp`    | Runs `Key.decode` over a recorded byte stream and prints the keys                    |
| `crashprobe.trp` | The editor with a keymap that kills the kernel, for the supervisor's restore check   |

## Running

```
./local.sh examples/battallion/bt.trp --io-root <dir> -- <file>
```

`--io-root` names the subtree file access is confined to; the argument after `--` is the file to
open, resolved inside it. A file that cannot be read is reported on stderr before the terminal is
touched.

| Key                                | What it does           |
|------------------------------------|------------------------|
| `h` `j` `k` `l`, arrow keys        | Move the cursor by one |
| `0`, Home                          | Start of the line      |
| `$`, End                           | End of the line        |
| PageUp / PageDown, CTRL-b / CTRL-f | Move by one screen     |
| `q`, CTRL-c                        | Quit                   |

The viewport scrolls to follow the cursor, a window resize redraws at the new size, and the
terminal is given back on every way out — including a kernel that stops answering.

`keydemo.trp` needs no terminal:

```
./local.sh examples/battallion/keydemo.trp --localonly
```

## Dependency pins

`bt.deps.json`, `crashprobe.deps.json` and `keydemo.deps.json` pin the program-relative modules by
content hash. Regenerate them after changing a module or rebuilding the compiler:

```
./bin/troupec --update-deps examples/battallion/bt.trp
```

`make benchmark-deps` does the same for every program under `examples/` that imports a
program-relative module.

## Verification

The viewer is interactive, so it is checked under a pseudo-terminal rather than by the golden
runner:

```
python3 tests/_unautomated/claude/battallion-viewer/harness.py "$PWD"
```

The harness opens a file taller than the terminal, injects motions and reads the cursor-position
escapes back, quits, kills the kernel with `crashprobe.trp`, resizes the pty, and sends SIGTERM,
asserting on each path that the terminal was restored. The label questions the design rests on are
measured by the probe programs in the same directory.
