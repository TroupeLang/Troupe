# Troupe Standard Library

Unlike the ones implemented in the runtime, `rt/`, this standard library is implemented in Troupe.
This moves them out of the trusted computing base (TCB), i.e. the parts of Troupe that need to be
reviewed rigorously rather than depend on the monitor.

## Modules

- `Bench`         : Timing harness for the benchmark programs under `examples/`. `time` returns a
                    thunk's value with its elapsed milliseconds; `run` performs one warmup and then
                    `reps` timed runs per size, reporting each run on a line of its own.
- `BigInt`        : Arbitrary-precision integers, written with the `123n` literal syntax. They are
                    a distinct type from numbers and the ordinary operators do not apply, so all
                    bigint arithmetic and comparison goes through this module; the decimal printer
                    is `show`.
- `Duration`      : Time spans, represented in milliseconds.
- `Hash`          : Hash functions for values of all types.
- `HashMap`       : Map from keys to values via their hash.
- `HashSet`       : Set of elements via their hash.
- `Html`          : Combinator-based HTML generation with escaping.
- `IfcUtil`       : Lattice-level constants and helper functions for downgrading
                    (declassification and endorsement).
- `Json`          : Parser and printer for JSON (RFC 8259). Documents are ordinary Troupe values:
                    `null` is unit, an array is a list, an object is a record. Object member order
                    and duplicate keys therefore do not survive a round trip, and `null` cannot be
                    told apart from unit. `parse` returns an `Outcome` whose `ERR` carries a
                    `reason` and a character `offset`.
- `List`          : Operations for lists, i.e. `[]` and `x::xs`.
- `ListPair`      : Operations for list of pairs, i.e. `(x,y)::xs`.
- `Map`           : Map from keys to values via a comparator function.
- `Markdown`      : Markdown to safe HTML, via an AST rendered through `Html`. Supports ATX
                    headings with `id` anchors, paragraphs, fenced code, blockquotes, nested and
                    task lists, thematic breaks, GFM tables, emphasis, strikethrough, inline code,
                    links and images. No raw HTML passthrough.
- `Number`        : Operations for numbers, i.e. integer and floats.
- `Option`        : An optional value: `SOME v` / `NONE`.
- `Outcome`       : A value or a diagnostic: `OK v` / `ERR e`. Use it when a failure carries
                    something the caller cannot work out for itself, such as a source offset;
                    where the failure is only the absence of a value, use `Option`. Importing
                    it brings `OK` and `ERR` into scope, and that is not transitive: a client
                    that receives an outcome from another library must import `Outcome` itself
                    in order to pattern-match on it. The same holds for `Option`.
- `Pretty`        : Hughes-style pretty-printing combinators: documents laid out to a target
                    width, with groups that occupy one line when they fit and stack when they do
                    not. Exports the operators `<.>` (beside), `<+>` (beside with a space), and
                    `$$` (above); see [docs/OPERATORS.md](../docs/OPERATORS.md).
- `Rope`          : Persistent text buffer held as a binary tree of string leaves. Nodes cache
                    length, newline count and depth, so `length`, `lineCount` and `depth` are
                    O(1), and `charAt`, `insert`, `delete`, `concat`, `splitAt`, `offsetOfLine`,
                    `lineAt`, `offsetToPosition` and `positionToOffset` are O(depth), where the
                    tree is rebuilt whenever its depth would pass `depthLimit`. Leaves carry the
                    offsets of their own newlines, so `fromString` is the only operation that
                    scans the text; it finds newlines with `strIndexOf`, about 50ms for a
                    megabyte. `text` is the buffer as a string -- not `toString`, which a
                    library cannot export without shadowing the builtin `print` is built from.
                    Offsets, lengths and columns are UTF-16 code units, lines are separated by
                    `"\n"` and counted from zero, and every operation clamps an out-of-range
                    argument rather than failing.
- `Set`           : Set of elements via a comparator function.
- `Sexp`          : Reader and printer for Lisp-style s-expressions: `ATOM` / `STR` / `LST`,
                    `parse` returning an `Outcome`, and both a single-line and a `Pretty`-based
                    rendering.
- `SimpleFileIO`  : Whole-file and directory access reported as an `Outcome`. Every operation
                    takes ROOT `authority` and resolves its path inside the `--io-root` subtree;
                    a path escaping that subtree fails rather than reaching the filesystem.
                    Import it selectively -- `import { readFile, writeFile } SimpleFileIO`
                    shadows the builtins of those names, leaving call sites unchanged. A name
                    left out of that list still resolves, to the raw builtin, whose tagged
                    record does not match `OK`.

  | Operation                        | `OK` payload                                              |
  |----------------------------------|-----------------------------------------------------------|
  | `readFile (auth, path)`          | the contents                                              |
  | `writeFile (auth, path, s)`      | `()`                                                      |
  | `appendFile (auth, path, s)`     | `()`, creating the file if absent                         |
  | `fileExists (auth, path)`        | `true` / `false`; never `ERR`                             |
  | `readDir (auth, path)`           | `{name, kind}` list, kind `"file"` / `"dir"` / `"other"`  |
  | `makeDir (auth, path)`           | `()`, recursive and idempotent                            |
  | `fileStat (auth, path)`          | `{kind, size, mtime}`, mtime in milliseconds              |
  | `removeFile (auth, path)`        | `()`                                                      |

  An `ERR` carries `{reason, path}`, where `path` is the caller-supplied path rather than the
  resolved one. Entry order from `readDir` is unspecified. A symlink reports `kind = "other"`
  rather than the kind of its target.
- `StencilVector` : Memory-efficient implementation of small (sparse) arrays.
- `String`        : Operations for strings
- `Svg`           : SVG generation on the node model of `Html`: an `svg` root that sets the xmlns,
                    `g`, the shape elements, `text` and `points`. Attribute names written in
                    camelCase are emitted hyphenated where SVG spells them that way (`fontSize`
                    becomes `font-size`); SVG's own camelCase attributes, such as `viewBox`, pass
                    through unchanged.
- `Template`      : Jinja-style text templating against a context record: `{{ expr }}`
                    interpolation, escaped through `Html.escapeHtml` unless piped to `safe`,
                    `{% if %}` and `{% for %}` blocks, filters, and `{# comments #}`. `compile` and
                    `render` return an `Outcome`; a path absent from the context is an `ERR` naming
                    it, not the empty string.
- `ThreadUtil`    : Additional functions for thread management.
- `Time`          : Date and time manipulation.
- `Tty`           : Terminal queries, raw mode, and keystroke delivery as mailbox events. The
                    runtime ships bytes, not decoded keys: `subscribe (fd, pid)` delivers one
                    `ttyEvent` per input chunk (`TTY_DATA` of the latin1-decoded bytes,
                    `TTY_RESIZE` of the new size, `TTY_EOF`), with presence and payload at the
                    stdio channel level. A plain `receive` never sees them: `listen (fd, auth)`
                    opens the ranged-receive region up to the channel level and `nextEvent`
                    receives at that interval; `nextEventWith` appends the caller's own handlers
                    so one receive waits on terminal events and its own protocol together.
                    `nextEventAtLevel` receives and declassifies an event's payload, the
                    `IfcUtil.freadlnAtLevel` idiom. Query and effect results are `Outcome`
                    values; the raw builtins (`ttySize`, `ttyRawMode`, ...) stay ambient and
                    return tagged records that never match `OK`, so call the wrappers.
- `timeout`       : Timers that send a message or exit the program after a duration.
- `Unit`          : Unit testing.
- `VariantsDemo`  : Datatype groups (`color`, `'a box`, `shape`) and functions over them, exported
                    for the syntactic-variant import tests under `tests/`. A test fixture rather
                    than a library for user code.
- `Vector`        : Immutable vectors held as a 16-way branching tree of lists. `sub` and `update`
                    walk one node per level and scan at most 16 entries in each; `update` copies
                    one node per level and shares the rest of the structure.

## How to add a new file

To compile a module as part of the standard library, add a `$(COMPILER) ./Name.trp -l` line to the
`build` target of `lib/Makefile` (the root `make lib` target delegates to it). The lines are grouped
in dependency order, so a new file goes after everything it imports. Build with `/usr/bin/make lib`
from the repository root.

## Design Principles

- File names are written in `CamelCase`. This makes them conform to the Standard ML Basis Library.
- We will try to match function names in the Standard ML library.
- A module's body is a list of `("name", value)` pairs, which importers reach under the file's own
  name. This (1) makes it closer to the SML module system and (2) allows for name resolution, e.g.
  `HashMap.findOpt` and `ListPair.findOpt` can be used in the same file. A module that declares
  datatypes exports them alongside that list, and constructors arrive unqualified — see
  [docs/MODULES.md](../docs/MODULES.md).
- Each function that is exported has to be documented (`(** <text> *)`). In the long run, we will
  auto-generate documentation for the Standard Library. Not every existing module meets this yet:
  `Html` documents its exports with plain `(* *)` comments, and a few modules share one doc comment
  across a group of related exports.
- A library that exports operators (see [docs/OPERATORS.md](../docs/OPERATORS.md)) declares their
  fixities in its header and exports them under their symbolic names
  (`("<+>", ( <+> ))`); the fixity travels in the `.exports` interface. When an operator has a
  natural word name, export that alphabetic alias alongside it — `Pretty` exports
  `("beside", ( <.> ))` and `("above", ( $$ ))` — so qualified-import users have a prefix spelling.
- Avoid changing signatures and names of existing functions. It breaks backwards compatibility
  with existing code, i.e., in the user guide, assignments, etc. New library functions are okay to
  introduce, including those that happen to duplicate functionality. We should then go through the
  discussion of deprecation of existing libraries.

### Other notes

- The `ThreadUtil` module was initially named `Thread`. But, this suggests incorrectly, that
  threading is implemented here rather than being a language primitive.
