# battallion

A terminal text editor written in Troupe. Under construction; the stages and what each one
delivers are in `_dev_planning/text-editor/mvp-plan.md`. At this stage it opens a file, draws it,
navigates it, edits it, undoes, saves and quits, and its keymap, status line, cursor shape and
file commands are plugins behind a published interface.

| File              | What it is                                                                          |
|-------------------|-------------------------------------------------------------------------------------|
| `bt.trp`          | The editor: argument handling and the file read, then a session                      |
| `Session.trp`     | The process constellation — supervisor, kernel, renderer — and the terminal handling |
| `Api.trp`         | The plugin interface: the editor state and the operations a plugin is handed         |
| `Editor.trp`      | The kernel: the frame's geometry, and the plugin set the mvp ships                   |
| `VimMotions.trp`  | Plugin: the keymap — modes, motions, and the editing keys                            |
| `StatusLine.trp`  | Plugin: the frame's bottom row                                                       |
| `CursorStyle.trp` | Plugin: the cursor's DECSCUSR shape, chosen by where the cursor is                   |
| `FileOps.trp`     | Plugin: what `:w`, `:q`, `:q!` and `:wq` mean                                        |
| `Screen.trp`      | The terminal escapes and the assembly of one frame                                   |
| `Key.trp`         | Decodes terminal input bytes into `key` values, with a carry across chunk boundaries |
| `keydemo.trp`     | Runs `Key.decode` over a recorded byte stream and prints the keys                    |
| `crashprobe.trp`  | The editor with a keymap that kills the kernel, for the supervisor's restore check   |

## Running

```
./local.sh examples/battallion/bt.trp --io-root <dir> -- <file>
```

`--io-root` names the subtree file access is confined to; the argument after `--` is the file to
open, resolved inside it. A file that cannot be read is reported on stderr before the terminal is
touched. The same subtree bounds where `:w` can write.

| Key                                | What it does                                     |
|------------------------------------|--------------------------------------------------|
| `h` `j` `k` `l`, arrow keys        | Move the cursor by one                           |
| `0`, Home                          | Start of the line                                |
| `$`, End                           | End of the line                                  |
| PageUp / PageDown, CTRL-b / CTRL-f | Move by one screen                               |
| `i`, Esc                           | Enter and leave insert mode                      |
| `o`                                | Open a line below and enter insert mode          |
| `x`, Delete                        | Delete the character under the cursor            |
| `u`                                | Undo one change                                  |
| `:`                                | Open the command line                            |
| `q`, CTRL-c                        | Quit, refusing while the buffer is modified      |
| `:w` `:q` `:q!` `:wq`              | Write, quit, quit without writing, write and quit |

The viewport scrolls to follow the cursor, a window resize redraws at the new size, and the
terminal is given back on every way out — including a kernel that stops answering. The cursor is a
bar while it sits on a character and an underscore when it sits past the end of a line.

`keydemo.trp` needs no terminal:

```
./local.sh examples/battallion/keydemo.trp --localonly
```

## Plugins

`Api.trp` is what a plugin programs against: the state record, and the `api` record of operations
over it. A plugin holds no authority, no pid and no descriptor — a module is compiled in library
mode, where `authority` is not bound and the ambient wrappers are not injected — so the state it
returns is its whole effect on the editor. `Editor.plugins` is the mvp's set, and `Session.run`
takes it as a parameter: `crashprobe.trp` replaces one field of it and keeps the rest.

Plugins are static imports at this stage. Delivered plugins — a separately compiled closure
`restore`d at runtime — use the same interface; `docs/MODULES.md` explains the hash-pinned linking
that makes that safe, and `Api.trp`'s header explains what it means for recompilation.

## Dependency pins

`bt.deps.json`, `crashprobe.deps.json` and `keydemo.deps.json` pin the program-relative modules by
content hash. Regenerate them after changing a module or rebuilding the compiler:

```
./bin/troupec --update-deps examples/battallion/bt.trp
```

`make benchmark-deps` does the same for every program under `examples/` that imports a
program-relative module.

## Verification

The editor is interactive, so it is checked under a pseudo-terminal rather than by the golden
runner:

```
python3 tests/_unautomated/claude/battallion-viewer/harness.py "$PWD"
python3 tests/_unautomated/claude/battallion-editor/harness.py "$PWD"
python3 tests/_unautomated/claude/battallion-plugins/harness.py "$PWD"
```

The first opens a file taller than the terminal, injects motions and reads the cursor-position
escapes back, quits, kills the kernel with `crashprobe.trp`, resizes the pty, and sends SIGTERM,
asserting on each path that the terminal was restored. The second types text, edits, undoes,
writes, and compares the saved file byte for byte. The third checks the cursor's shape against the
cursor's position and its reset on the way out. The label questions the design rests on are
measured by the probe programs in the same directories.
