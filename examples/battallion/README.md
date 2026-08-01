# battallion

A terminal text editor written in Troupe. Under construction; the stages and what each one
delivers are in `_dev_planning/text-editor/mvp-plan.md`. At this stage it opens a file, draws it,
navigates it, edits it, undoes, saves and quits, and its keymap, status line, cursor and file
commands are plugins behind a published interface.

| File              | What it is                                                                           |
|-------------------|--------------------------------------------------------------------------------------|
| `bt.trp`          | The editor: argument handling and the file read, then a session                       |
| `Session.trp`     | The process constellation — supervisor, kernel, renderer — and the terminal handling  |
| `Api.trp`         | The plugin interface: the editor state and the operations a plugin is handed          |
| `Editor.trp`      | The kernel: the frame's geometry, and the plugin set the mvp ships                    |
| `VimMotions.trp`  | Plugin: the keymap — modes, motions, and the editing keys                             |
| `StatusLine.trp`  | Plugin: the frame's bottom row                                                        |
| `CursorStyle.trp` | Plugin: the seam for the cursor's appearance; inert, the terminal's own cursor stands |
| `FileOps.trp`     | Plugin: what `:w`, `:q`, `:q!` and `:wq` mean                                         |
| `Screen.trp`      | The terminal escapes and the assembly of one frame                                    |
| `Key.trp`         | Decodes terminal input bytes into `key` values, with a carry across chunk boundaries  |
| `keydemo.trp`     | Runs `Key.decode` over a recorded byte stream and prints the keys                     |
| `crashprobe.trp`  | The editor with a keymap that kills the kernel, for the supervisor's restore check    |

## Running

```
./local.sh examples/battallion/bt.trp --io-root <dir> -- <file>
```

`--io-root` names the subtree file access is confined to; the argument after `--` is the file to
open, resolved inside it. The same subtree bounds where `:w` can write.

Everything the editor refuses at startup is one line on stderr and exit 1, written while the
terminal is still the shell's: no file named, more than one, an empty path, and a file that cannot
be read. The path rules are the runtime's (`rt/src/builtins/simplefileio.mts`) and the reason it
gives is reported as it comes:

| Path                              | What happens                                            |
|-----------------------------------|---------------------------------------------------------|
| `f.txt`, `sub/f.txt`              | Opens; a relative path may descend into the subtree      |
| `../elsewhere`, `sub/../../x`     | `path escapes the io-root sandbox`                       |
| An absolute path inside the root  | Opens, spelled as the root's own realpath                |
| An absolute path outside          | `path escapes the io-root sandbox`                       |
| A symlink pointing out of the root| `path escapes the io-root sandbox via a symlink`         |
| A directory                       | `path is a directory`                                    |
| Anything not there                | `file not found`                                         |
| A file whose bytes are not UTF-8  | `file is not valid UTF-8`                                |

The absolute-path row has a wrinkle worth knowing on macOS: the runtime resolves `--io-root`
through its symlinks once at startup and compares against the result, so `--io-root /tmp/x` with
the file named `/tmp/x/f.txt` is refused as an escape while `/private/tmp/x/f.txt` — the same file
— opens. Relative paths are unaffected.

Left off entirely, `--io-root` is a fresh empty directory the runtime makes for the invocation, so
every path is a file that is not there; the second line of a startup failure says so, there being
no way for a program to ask what the subtree is.

## What is in the file

The buffer is the file's bytes, and `:w` writes the buffer. Nothing is added or trimmed: a file
whose last line has no newline keeps it that way, and a file ending in a newline has an empty last
line, which the editor draws and the cursor can reach — where vi draws filler from that row on. A
file of no bytes is one empty line.

The file has to be text. `SimpleFileIO.readFile` decodes as UTF-8 and refuses a file whose bytes
are not, so a binary file is a startup refusal — `file is not valid UTF-8`, on stderr with the
terminal untouched — rather than a buffer the editor would write back with each undecodable byte
replaced. A NUL and the other control bytes are valid UTF-8 and survive a round trip unchanged.
Editing arbitrary bytes is out of scope: the runtime has a byte-level read and write
(`readFileBytes`, `writeFileBytes`), but the buffer, the screen and the key decoder are all text.

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
terminal is given back on every way out — including a kernel that stops answering.

The cursor is the terminal's own, and it is on the screen whenever the editor is idle: a frame
hides it while it redraws and shows it again as the last thing it writes. No shape is selected —
`CursorStyle.trp` is the seam that would and its header says why it is parked.

`[+]` in the status line means the buffer differs from the file, which is a comparison of
revisions rather than a flag: it is right after an undo that walks back past a `:w`, where a flag
carried in the undo history is not (`Api.trp`'s header).

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
python3 tests/_unautomated/claude/battallion-cli/harness.py "$PWD"
```

The first opens a file taller than the terminal, injects motions and reads the cursor-position
escapes back, quits, kills the kernel with `crashprobe.trp`, resizes the pty, and sends SIGTERM,
asserting on each path that the terminal was restored. The second types text, edits, undoes,
writes, and compares the saved file byte for byte — including an undo that walks back past a `:w`.
The third checks the per-frame cursor discipline and that no shape escape is written. The fourth
checks the edges: the argument list, the path rules above, the file-content policy, a terminal of
one row, and a `:wq` the filesystem refuses. The label questions the design rests on are measured
by the probe programs in the same directories.

`tests/_unautomated/claude/battallion-drive/driver.py` holds a session open on a pty and renders
its output through a screen model, for driving the editor by hand from a shell.
