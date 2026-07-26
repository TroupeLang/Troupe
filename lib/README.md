# Troupe Standard Library

Unlike the ones implemented in the runtime, `rt/`, this standard library is implemented in Troupe.
This moves them out of the trusted computing base (TCB), i.e. the parts of Troupe that need to be
reviewed rigorously rather than depend on the monitor.

## Modules

- `Duration`      : Time spans, represented in milliseconds.
- `Hash`          : Hash functions for values of all types.
- `HashMap`       : Map from keys to values via their hash.
- `HashSet`       : Set of elements via their hash.
- `Html`          : Combinator-based HTML generation with escaping.
- `IfcUtil`       : Lattice-level constants and helper functions for downgrading
                    (declassification and endorsement).
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
- `Set`           : Set of elements via a comparator function.
- `SimpleFileIO`  : Whole-file and directory access reported as an `Outcome`. Every operation
                    takes ROOT `authority` and resolves its path inside the `--io-root` subtree;
                    a path escaping that subtree fails rather than reaching the filesystem.

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
- `ThreadUtil`    : Additional functions for thread management.
- `Time`          : Date and time manipulation.
- `timeout`       : Timers that send a message or exit the program after a duration.
- `Unit`          : Unit testing.

## How to add a new file

To compile a module as part of the standard library, add it to the list of files in the `lib`
target of the *makefile*.

## Design Principles

- File names are written in `CamelCase`. This makes them conform to the Standard ML Basis Library.
- We will try to match function names in the Standard ML library.
- Each module exports a single *record* with the same name as the file. This (1) makes it closer to
  the SML module system and (2) allows for name resolution, e.g. `HashMap.findOpt` and
  `ListPair.findOpt` can be used in the same file.
- Each function that is exported has to be documented (`(** <text> *)`). In the long run, we will
  auto-generate documentation for the Standard Library.
- A library that exports operators (see [docs/OPERATORS.md](../docs/OPERATORS.md)) declares their
  fixities in its header and exports them under their symbolic names
  (`("<+>", ( <+> ))`); the fixity travels in the `.exports` interface. When an operator has a
  natural word name, export that alphabetic alias alongside it (`("beside", ( <+> ))`), so
  qualified-import users have a prefix spelling.
- Avoid changing signatures and names of existing functions. It breaks backwards compatibility
  with existing code, i.e., in the user guide, assignments, etc. New library functions are okay to
  introduce, including those that happen to duplicate functionality. We should then go through the
  discussion of deprecation of existing libraries.

### Other notes

- The `ThreadUtil` module was initially named `Thread`. But, this suggests incorrectly, that
  threading is implemented here rather than being a language primitive.
