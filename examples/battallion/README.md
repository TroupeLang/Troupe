# battallion

A terminal text editor written in Troupe. Under construction; the stages and what each one
delivers are in `_dev_planning/text-editor/mvp-plan.md`. It opens a file, draws it, and navigates
it with a vim command grammar — counts, word and character motions, brackets and paragraphs,
search and marks. It edits with operators over motions and text objects, insert and replace modes,
counts and the `.` repeat, a visual selection, undo and redo, and it saves — to its own file or to
another — and quits; `:help` pages through the keys over the buffer. Its keymap, status line,
cursor, file commands and help page are plugins behind a published interface.

| File              | What it is                                                                           |
|-------------------|--------------------------------------------------------------------------------------|
| `bt.trp`          | The editor: argument handling and the file read, then a session                       |
| `Session.trp`     | The process constellation — supervisor, kernel, renderer — and the terminal handling  |
| `Api.trp`         | The plugin interface: the editor state and the operations a plugin is handed          |
| `Editor.trp`      | The kernel: the frame's geometry, and the plugin set the editor ships                 |
| `VimMotions.trp`  | Plugin: the keymap — the command grammar, the modes, search, marks, and the repeat    |
| `Motions.trp`     | Where a motion lands: words, character finds, brackets, paragraphs, the search scan   |
| `TextObjects.trp` | The text an `i` or `a` object names: a word, a bracket pair, a quote, a paragraph     |
| `Operators.trp`   | What an operator does to a range: delete, change, yank, shift, case; put, join, `r`    |
| `StatusLine.trp`  | Plugin: the frame's bottom row                                                        |
| `CursorStyle.trp` | Plugin: the seam for the cursor's appearance; inert, the terminal's own cursor stands |
| `FileOps.trp`     | Plugin: what a command word means — `:w`, `:q`, a line number, `:help`                |
| `Help.trp`        | Plugin: lays the `:help` page out from the sections the other plugins export          |
| `Screen.trp`      | The assembly of one frame and its tab stops, over `../proscenium/Ansi`'s escapes      |
| `Key.trp`         | Adapts `../proscenium/Key`'s decoder to this key set, with a carry across chunks      |
| `keydemo.trp`     | Runs `Key.decode` over a recorded byte stream and prints the keys                     |
| `helpdemo.trp`    | Draws the `:help` page at several terminal sizes and prints it                        |
| `crashprobe.trp`  | The editor with a keymap that kills the kernel, for the supervisor's restore check    |

## Running

```
./local.sh examples/battallion/bt.trp --io-root <dir> -- <file>
```

`--io-root` names the subtree file access is confined to; the argument after `--` is the file to
open, resolved inside it. The same subtree bounds where `:w` can write, including the path
`:w PATH` names.

Everything the editor refuses at startup is one line on stderr and exit 1, written while the
terminal is still the shell's: no file named, more than one, an empty path, and a file that cannot
be read. The path rules are the runtime's (`rt/src/ffi/node/simplefiles.mts`) and the reason it
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

Motions:

| Key                                    | What it does                                              |
|----------------------------------------|-----------------------------------------------------------|
| `h` `j` `k` `l`, arrow keys            | Move the cursor by one                                    |
| `0` Home, `^`, `$` End, <code>&#124;</code> | Line start, first non-blank, end, column `N`         |
| `w` `b` `e` `ge`                       | Word start next and previous, word end forward and back   |
| `W` `B` `E` `gE`                       | The same, over blank-delimited words                      |
| `f` `t` `F` `T`, `;` `,`               | To or till a character; repeat, and reversed              |
| `%`, `{` `}`                           | Matching bracket; paragraph back and forward              |
| `gg` `G`, `H` `M` `L`                  | First, last or line `N`; screen top, middle, bottom       |
| `+` `-` `_`, Enter                     | Line down, up, `N` down, to the first non-blank           |
| CTRL-f / CTRL-b, PageDown / PageUp     | Move by one screen                                        |
| CTRL-d / CTRL-u, CTRL-e / CTRL-y       | Half a screen; scroll a line, cursor kept on screen       |

Operators, over any motion or text object, and doubled to take whole lines:

| Key                       | What it does                                                    |
|---------------------------|-----------------------------------------------------------------|
| `d` `c` `y`               | Delete, change, yank; `dd` `cc` `yy` the line                   |
| `<` `>`                   | Shift a line left, right by one tab; `<<` `>>` the line          |
| `g~` `gu` `gU`            | Toggle, lower, upper case; `g~~` `guu` `gUU` the line            |

Text objects, after an operator or in visual mode, with `i` for inside and `a` for around:

| Key                                        | What it does                                        |
|--------------------------------------------|-----------------------------------------------------|
| `iw` `aw`, `iW` `aW`                        | A word, with its spaces; blank-delimited            |
| `i(` `a(` `ib`, `i[` `i{` `iB` `i<`        | Inside or around a bracket pair                     |
| <code>i"</code> `i'` <code>i`</code>       | Inside or around a quoted stretch                   |
| `ip` `ap`                                  | A paragraph                                         |

Editing:

| Key                                | What it does                                        |
|------------------------------------|-----------------------------------------------------|
| `i` `a` `I` `A`                    | Insert before, after the cursor, at line start, end |
| `o` `O`                           | Open a line below, above, and enter insert mode     |
| `R`                                | Replace mode: overwrite until Esc                   |
| `x` `X`, Delete                    | Delete the character under, before the cursor       |
| `s` `S`, `D` `C`, `Y`             | Change a character, the line, to the end; yank line  |
| `r`                                | Replace one character, `N` with a count             |
| `~`                                | Toggle case and move on                             |
| `J` `gJ`                          | Join lines, without a space                         |
| `p` `P`                           | Put after, before the cursor                        |
| `u`, CTRL-r                        | Undo, redo                                          |
| `.`                                | Repeat the last change                              |

Visual mode, search, marks, and the session:

| Key                                | What it does                                        |
|------------------------------------|-----------------------------------------------------|
| `v` `V`                           | Start a charwise, linewise selection; `o` swaps ends |
| `/text` `?text` Enter              | Search forward, backward for `text`                 |
| `n` `N`, `*` `#`                   | Next, previous match; the word under the cursor      |
| `ma`, `'a` <code>`a</code>         | Set mark `a`; go to its line, its column            |
| `:`                                | Open the command line                               |
| `ZZ` `ZQ`                         | Write and quit; quit discarding changes             |
| `q`, CTRL-c                        | Quit, refusing while the buffer is modified         |

| Command             | What it does                                                       |
|---------------------|---------------------------------------------------------------------|
| `:w`                | Write the buffer to the file it was opened from                     |
| `:w PATH`           | Write the buffer to `PATH`; the buffer keeps its own file           |
| `:wq` `:x`          | Write and quit; `:wq PATH` writes to `PATH` and quits               |
| `:q`                | Quit, refusing while the buffer is modified                         |
| `:q!`               | Quit, discarding the changes                                        |
| `:N` `:$`           | Go to line `N`, to the last line                                    |
| `:help` `:h`        | List the keys and commands over the buffer; any key returns         |

The viewport scrolls to follow the cursor, a window resize redraws at the new size, and the
terminal is given back on every way out — including a kernel that stops answering.

Normal mode is a grammar rather than a fixed set of keys: an optional count, then a command that
is either a standalone key or an operator followed by a motion or a text object, itself with an
optional count. So `3j`, `d2w`, `2dd`, `ci(`, `>ip` and `d/foo` Enter are all read from the same
rules, and two counts multiply — `2d3w` deletes six words. A partial command waits in the state
and is shown at the end of the status line in parentheses; a key sequence the grammar cannot read
is dropped, and Esc abandons whatever is pending. A motion that cannot be made — `fx` on a line
with no `x`, `j` on the last line — aborts the command it is the target of, so `dfx` with no `x`
deletes nothing, as vi does.

The cursor sits on a character in normal mode, never past the last one: leaving insert mode steps
it left onto the last character typed, and every normal-mode command pulls it back onto the line
when a motion would leave it beyond the end. Insert mode may sit one past the end, since that is
where `a` and `A` append. A vertical motion aims for a remembered column, which a horizontal
motion sets and `$` sets to the line's end, so `$` then `j` walks down the ends of the lines.

Every command is one undo step, and so is an insert: a command that edits several times — `3x`,
`J`, a shift over a range — squashes its edits into one, and an insert entered with a count, `3ix`
or `2o`, repeats its typed text that many times and undoes as one. `.` repeats the last change,
replaying its keys, and a count on `.` replaces the count the change was recorded with. The
register is the unnamed one only: everything that deletes writes it, `y` writes it without
deleting, the text is marked charwise or linewise, and `p` and `P` put it accordingly, `N` times.

Replace mode, entered with `R`, overwrites the character under the cursor with each printable key
and appends past the end of a line; Backspace steps back and restores the character it overwrote,
and Esc returns to normal mode. `r` replaces a single character without entering the mode.

Visual mode selects first and operates second: `v` starts a charwise selection and `V` a linewise
one, the selection is drawn in reverse video and follows the cursor as any motion moves it, `o`
swaps which end the cursor is on, and an operator, a text object or a key like `d`, `y`, `U`, `J`,
`r` or `p` acts on what is selected and returns to normal mode. A second `v` or `V`, or Esc, ends
the selection without acting.

The search is a plain substring, case-sensitive, and it wraps: `/text` searches forward and
`?text` backward from the cursor, `n` and `N` repeat it in and against its direction, `*` and `#`
search for the word under the cursor as a whole word, a search that runs off the end continues
from the other and says so, and one that finds nothing says that instead and leaves the cursor
where it was. A pattern is remembered whether or not it was found; `/` and Enter with nothing typed
repeats it. A pattern is matched within one line.

A mark is a position: `ma` notes the cursor under the letter `a`, `'a` goes to the first non-blank
of its line and <code>`a</code> to its exact column, and `''` and <code>``</code> return to where
the cursor was before the last jump. An edit above a mark does not move it, and a jump to one
clamps it to the buffer as it now stands.

The linewise commands — `gg`, `G`, `dd`, `:N`, a jump to a mark's line — land on the line's first
non-blank, as vim does with `startofline` set, rather than at column zero. `^` is one keystroke
from any of them when column zero is wanted.

`:w PATH` writes a copy and does not adopt the path, which is what vi does: the buffer goes on
belonging to the file it was opened with, a later `:w` writes there, and the buffer stays modified
because its own file is still out of date. Where the copy may land is bounded by `--io-root`, like
every other path.

`:help` draws the keys and commands over the buffer. The list is assembled from sections the
plugins export — the keymap's keys and the command interpreter's words — so a key added to a plugin
is listed by the plugin that added it. It is laid out in as many columns as the terminal is wide
enough for, and a list too long for one screen is paged: Space and PageDown turn forward, `b` and
PageUp back, and any other key returns to the buffer. The bottom row says which page is up.

The cursor is the terminal's own, and it is on the screen whenever the editor is idle: a frame
hides it while it redraws and shows it again as the last thing it writes. No shape is selected —
`CursorStyle.trp` is the seam that would and its header says why it is parked.

A row is cut to the columns it is drawn in rather than to its characters, with tab stops every
eight, and the cursor is drawn at the column its buffer position falls at over those same stops —
so a tab-indented line shows as much as fits and the cursor sits on the character after the
indent. Characters wider than one column, such as CJK, are not measured: a line of them is drawn
past the width it was cut to. The session clears the terminal's automatic wrap for its whole
length, so such a line is clipped at the right edge instead of continuing onto the next row and
scrolling the frame away; the cursor may be drawn to the left of where such a character appears.

`[+]` in the status line means the buffer differs from the file, which is a comparison of
revisions rather than a flag: it is right after an undo that walks back past a `:w`, where a flag
carried in the undo history is not (`Api.trp`'s header).

`keydemo.trp` and `helpdemo.trp` need no terminal:

```
./local.sh examples/battallion/keydemo.trp --localonly
./local.sh examples/battallion/helpdemo.trp --localonly
```

## Plugins

`Api.trp` is what a plugin programs against: the state record, and the `api` record of operations
over it. A plugin holds no authority, no pid and no descriptor — a module is compiled in library
mode, where `authority` is not bound and the ambient wrappers are not injected — so the state it
returns is its whole effect on the editor. `Editor.plugins` is the set the editor ships, and
`Session.run` takes it as a parameter: `crashprobe.trp` replaces one field of it and keeps the
rest.

A plugin also says what its keys are for. `VimMotions.help` and `FileOps.help` are lists of
sections — a title and (keys, what they do) pairs — and `Editor.plugins` concatenates them into the
`:help` page, so a plugin set built from other plugins gets their help rather than this one's.

Plugins are static imports at this stage. Delivered plugins — a separately compiled closure
`restore`d at runtime — use the same interface; `docs/MODULES.md` explains the hash-pinned linking
that makes that safe, and `Api.trp`'s header explains what it means for recompilation.

## Dependency pins

`bt.deps.json`, `crashprobe.deps.json`, `keydemo.deps.json` and `helpdemo.deps.json` pin the
program-relative modules by content hash. Regenerate them after changing a module or rebuilding
the compiler:

```
./bin/troupec --update-deps examples/battallion/bt.trp
```

`make benchmark-deps` does the same for every program under `examples/` that imports a
program-relative module.

## Verification

The editor is interactive, so it is checked under a pseudo-terminal rather than by the golden
runner:

```
python3 examples/battallion/testing/viewer/harness.py "$PWD"
python3 examples/battallion/testing/editor/harness.py "$PWD"
python3 examples/battallion/testing/plugins/harness.py "$PWD"
python3 examples/battallion/testing/cli/harness.py "$PWD"
```

The first opens a file taller than the terminal, injects motions and reads the cursor-position
escapes back, walks the remembered column over lines of differing lengths, cuts tab-indented rows
at the tab stops on a terminal too narrow for them, quits, kills the kernel with `crashprobe.trp`,
resizes the pty, and sends SIGTERM, asserting on each path that the terminal was restored — the
automatic wrap the session clears among the rest. The second types text, edits, searches, deletes and puts lines, goes to a line by
number, undoes, writes — to its own file and to another — and compares the saved file byte for
byte, including an undo that walks back past a `:w`. It also drives the vim grammar the keymap
adds: counts and operators over motions, the text objects, the `.` repeat and redo, replace mode,
join and the case operators, and a visual selection an operator acts on — each ending in a write
and a byte-exact comparison. The third checks the per-frame cursor
discipline, that no shape escape is written, and the frame `:help` draws over the buffer. The
fourth checks the edges: the argument list, the path rules above applied to `:w PATH` as well as to
the file opened, the file-content policy, a terminal of one row, and a `:wq` the filesystem
refuses. The label questions the design rests on are measured by the probe programs in the same
directories.

`examples/battallion/testing/drive/driver.py` holds a session open on a pty and renders
its output through a screen model, for driving the editor by hand from a shell.
