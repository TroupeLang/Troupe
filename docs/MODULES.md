# Program modules

> **Scope:** the `import "./Path"` form, what a module source file looks like, the artifacts a
> module compile produces, how a module path is resolved, content-addressed module identity and the
> dependencies file, datatypes across a module boundary, and what happens when a closure that
> references a module is sent to another actor or node. For the standard library import form
> (`import List`) and the compilation pipeline see [ARCHITECTURE.md](ARCHITECTURE.md); for
> `datatype` declarations and constructor tags see [VARIANTS.md](VARIANTS.md).

A **module** is a program-local `.trp` file that another `.trp` file in the same program imports by
a relative path. A **library** is what a bare-identifier import names (`import List`), resolved
under `$TROUPE/lib/out`. The two share the same `.exports` interface format and the same runtime
loader, and differ in how they are found, how they are identified, and when they are built:

|                             | Library                          | Module                                    |
|-----------------------------|----------------------------------|-------------------------------------------|
| Import form                 | `import List`                    | `import "./util/Fmt"`                     |
| Location                    | `$TROUPE/lib/out/<Name>.exports` | Relative to the importing file            |
| Identity                    | Its name                         | The content hash of its compiled IR       |
| Built                       | Ahead of time (`make lib`)       | On every compile of the importing program |
| Datatype skew check at load | Yes                              | No                                        |

The last row is explained under [Datatypes across modules](#datatypes-across-modules).

## Import syntax

A quoted string in the library position of an `import` declaration is a module import. It composes
with the `qualified`, selection, and `as` modifiers exactly as a library import does
(`compiler/src/Parser.y`, the `ImportDecl` production).

| Form                                       | Effect                                                                     |
|--------------------------------------------|----------------------------------------------------------------------------|
| `import "./text/Greeting"`                 | Binds `Greeting`; its exported values are also visible unqualified         |
| `import "./text/Greeting" as G`            | Binds `G` instead of `Greeting`                                            |
| `import qualified "./text/Greeting" as G`  | Exported values reachable only as `G.name`                                 |
| `import { render } "./text/Greeting"`      | Only `render` is in scope, unqualified and as `Greeting.render`            |
| `import "../shared/Report"`                | A path that walks up out of the importing file's directory                 |

Rules on the literal path (`ProcessImports.checkModulePath`, `compiler/src/ProcessImports.hs:63`):

- it must start with `./` or `../`;
- it must not end with `/`;
- it must not contain empty segments.

The path names the file **without** the `.trp` extension; `.trp` is appended during resolution.
Writing the extension yields a lookup for `Greeting.trp.trp`.

Without `as`, a module import binds the **last path segment** of the literal
(`Parser.y`, `moduleBindName`). Two imports that bind the same name are rejected when at least one
of them is a module import.

Import declarations come first in a file, before any `datatype` declaration and before the main
expression.

## Writing a module

A module source file has the same shape as a standard library source file: imports, optional
`datatype` groups, and a main expression that must be a **list of `(string, name)` pairs** — the
export list.

```sml
(* text/Greeting.trp *)
import String

datatype tone = PLAIN | LOUD

let fun render (PLAIN, name) = "hello, " ^ name
      | render (LOUD, name)  = "HELLO, " ^ String.map (fn c => c) name ^ "!"
in [ ("render", render) ]
end
```

A module may import standard libraries and other modules. Any body that is not a list of
`(string, name)` pairs is rejected:

```
parse error: libraries need to use restricted syntax for their main body
```

### What is not in scope inside a module

Modules are compiled in library mode. Two consequences follow from that mode, both surfacing as
compile errors:

- `authority` is not bound. In a normal (program) compile, `CaseElimination.trans`
  (`compiler/src/CaseElimination.hs:29`) binds `authority` to the program's root authority; in
  library mode it does not, so `authority` inside a module fails with
  `troupec: bad base function: authority`. A module that needs authority takes it as a function
  argument and the main-mode consumer applies it — the pattern used by
  `examples/benchmarks/labeled-savina/LabeledSavina.trp`.
- The ambient wrappers injected by `AddAmbientMethods` — `print`, `printString`,
  `printWithLabels`, `fwriteln`, `fwritelnWithLabels`, `inputLine` — are not injected in library
  mode (they are defined in terms of `authority`). `print` inside a module fails with
  `troupec: bad base function: print`.

Base functions that do not go through `authority` (`spawn`, `send`, `self`, `receive`, arithmetic,
string operations, …) are available.

## Artifacts

Compiling a program compiles its whole module import graph first, dependencies before consumers
(`compiler/app/Main.hs:539-547`). There is no cache: every module in the graph is recompiled on
every compile of the program.

Each module's output is written next to its source, in an `out/` directory. `<dir>` below is the
directory holding the module source; `<main>` is the main program's source path.

| File                       | Content                                                             |
|----------------------------|---------------------------------------------------------------------|
| `<dir>/out/<Name>.js`      | The compiled module                                                 |
| `<dir>/out/<Name>.exports` | The module's interface                                              |
| `<main>.deps.json`         | The importing program's [dependencies file](#the-dependencies-file) |

A module's `.exports` is the line-oriented interface described in
[ARCHITECTURE.md](ARCHITECTURE.md#file-extensions), plus one line that only module artifacts
carry — its own content hash:

```
module-hash 39iec28751hjo458ptvhl08p4i5vnbk2n4nfb5ai571i6cspklu0
render
datatype shll35alvv9na0ofcilb0c97npl3ka69cvck8ppn541kugfdrk90 (group (dt tone 0 (ctor LOUD) (ctor PLAIN)))
```

A standard-library compile (`troupec -l`) emits no `module-hash` line; the `ModuleArtifact` marker
that adds it is internal to the compiler driver and is not a command-line option
(`compiler/app/Main.hs:79`, `:307`).

The compiler writes the `.js`, the `.exports`, and the dependencies file atomically — a temporary
file beside the target, then a rename — so a concurrent build or a running program never reads a
partially written artifact (`compiler/src/Util/FileUtil.hs`, `atomicWriteFileD`).

## Resolution

A module import literal is resolved against **the directory of the file that contains the import**,
not against the program root (`ProcessImports.resolveModule`, `compiler/src/ProcessImports.hs:102`).
A module in a subdirectory therefore reaches its own neighbours with `./Name`, and its parent
directory's files with `../Name`.

Resolution produces two things:

- the **source path**, used to compile the module and to locate its artifact. It is lexically
  canonicalized: `seg/..` and `.` are collapsed without touching the filesystem
  (`ProcessImports.collapseDotDot`, `:77`). Different spellings of one file reduce to one module,
  one compilation and one artifact. `import "./lib/Counter"` and `import "./sub/../lib/Counter"` in
  the same program produce a single entry.
- the **key**, the path of the module relative to the program root (the main file's directory). The
  key is what the dependencies file records and what the runtime joins onto the program root to
  find the artifact. A key may begin with `..` when the module lives outside the program root.

### Path independence

Because a module's identity is its content hash and not its location, an import path is purely a
resolution concern. A module may live anywhere reachable by a relative path, including above the
importing file and above the program root, and the same file reached along different paths from
different programs is the same module.

`examples/savina/SavinaReport.trp` is the worked case in the tree: it is imported as
`"./SavinaReport"` from `examples/savina/runall.trp`, as `"../SavinaReport"` from
`examples/savina/analysis/mkreport.trp`, and as `"../../savina/SavinaReport"` from the three
sibling suites under `examples/benchmarks/`. All five dependencies files pin the same hash.

The import graph must be acyclic. A cycle is a compile error that prints the cycle.

## Content-addressed identity

A module's identity is the SHA-256 hash of its **position-erased, codegened IR**, rendered as
lowercase unpadded base32hex (`compiler/src/ModuleHash.hs`). The hashed input is the format
identifier `troupe:mod:1` followed by the canonical s-expression printing of the optimized IR
(`IRSexp.printProg . IRSexp.erasePosProg`), the same canonical form `--verify-ir-sexp` round-trips.

The hash is Merkle by the IR's own structure: a dependency reference appears in the IR as a
`Lib "module:<dep-hash>" _` instruction, so a module's hash covers its dependencies' hashes with no
extra threading.

Two modules are the same module exactly when their hashes agree. Observable consequences, each
verified against the compiler:

| Change to a module source                               | Hash      |
|---------------------------------------------------------|-----------|
| Comments, whitespace, formatting                        | Unchanged |
| Renaming a local variable or function parameter         | Unchanged |
| Copying the file to another path or another directory   | Unchanged |
| Changing a string literal or any other code             | Changes   |
| Adding a constructor to a `datatype` group it declares  | Changes   |
| A dependency's own hash changing                        | Changes   |

Positions are erased before hashing, and α-renaming has already run by the time the IR exists, so
the file name, line numbers and local names are not inputs. Any file whose body is
`let fun stamp s = "[" ^ s ^ "]" in [ ("stamp", stamp) ] end`, wherever it sits and whatever its
comments say, hashes to `lbrn5kic1lu9hcug3mprn435a6qkjq33idevk91j68l4s884evrg` — the hash of
`tests/rt/pos/modules/modsrc/ModuleGuardHelper.trp`.

Codegen addresses a module by `module:<hash>`, so the name appearing in the emitted JavaScript and
in a serialized closure is the hash, never a path (`compiler/src/Core.hs:337`). From the
[worked example](#worked-example) below:

```js
this.__moduleRoot = "moddemo"
this.__moduleDepsFile = "moddemo/hello.deps.json"
this.addLib  ("module:39iec28751hjo458ptvhl08p4i5vnbk2n4nfb5ai571i6cspklu0" , "render")
```

`__moduleRoot` and `__moduleDepsFile` are recorded verbatim from the path the compiler was given.
Compiling with a relative path produces a program that must be run from the same working directory;
compiling with an absolute path produces one that runs from anywhere.

### `--datatype-hashes`

`troupec --datatype-hashes <file>` prints one line per `datatype` group **declared in that file** —
its group hash, two spaces, its canonical form — and stops before code generation:

```
$ troupec --datatype-hashes tests/rt/pos/modules/modsrc/ModuleShapes.trp
oj5ljjtenmlcv7bbn3m77h48dikj8qg74gic8bec77sifi2v07o0  (group (dt colour 0 (ctor BLUE) (ctor GREEN) (ctor RED)))
hkvb6plai26n57bmrhtsonit96qve9guc38umtgapoorvmvl4a4g  (group (dt shape 0 (ctor CIRCLE (prim int)) (ctor RECT (prod (prim int) (prim int)))))
```

These are **datatype-group** hashes, a different identity from the module hash: they identify a
declaration so that two parties can compare fingerprints and see whether their declarations agree
(see [VARIANTS.md](VARIANTS.md#identity-normalization-and-hashing)). Imported groups are not
listed; a file that declares no groups prints nothing.

## The dependencies file

Every program that imports modules has a dependencies file, `<main>.deps.json`, next to the main
file. It records one entry per module the program transitively imports:

```json
{
  "deps": [
    {"path": "text/Greeting", "hash": "39iec28751hjo458ptvhl08p4i5vnbk2n4nfb5ai571i6cspklu0", "name": "text/Greeting"}
  ]
}
```

- `path` — the module's key, relative to the program root. The resolution key, not the identity.
- `hash` — the pinned content hash.
- `name` — the module's user-visible name in diagnostics. Currently always equal to `path`.

Entries are sorted by path and written with a fixed key order, so a re-established file diffs
minimally (`compiler/src/DepsFile.hs:73`).

The file is read by both halves of the toolchain:

- **The compiler enforces it.** For each module import it reads the dependency's actual hash from
  that dependency's `.exports` and compares it with the pin. A missing pin or a mismatch is a
  compile error (`ProcessImports.hs:175-188`). The compiler never writes the file during a normal
  compile.
- **The runtime seeds its resolver from it.** The compiled program points at the file through
  `__moduleDepsFile`; the runtime builds a `hash -> (path, name)` map from it and resolves a
  `module:<hash>` reference to `<root>/<dirname(path)>/out/<basename(path)>.js`
  (`rt/src/moduleResolver.mts`). The file must therefore be present at run time, not only at
  compile time.

### `troupec --update-deps`

`troupec --update-deps <main.trp>` establishes or refreshes the file. It runs the same
dependencies-first resolution and compilation as a normal compile, but records actual hashes
instead of checking pins, then reports what moved:

```
$ bin/troupec --update-deps moddemo/hello.trp
  + text/Greeting  39iec28751hjo458ptvhl08p4i5vnbk2n4nfb5ai571i6cspklu0 (new)
wrote 1 module pin(s) to moddemo/hello.deps.json
```

| Line                       | Meaning                                      |
|----------------------------|----------------------------------------------|
| `+ <path>  <hash> (new)`   | A module not previously pinned               |
| `~ <path>  <old> -> <new>` | A pinned module whose content changed        |
| `- <path> (removed)`       | A pin with no corresponding import any more  |

`--update-deps` also writes the main program's JavaScript to `<dir>/out/<main>.js`, so it does not
depend on the working directory. It rejects `-l`.

Run it whenever a module's content changes, a module is added or removed, or a module moves to a
different path (a move changes the key, so the old pin is dropped and a new one added).

### Modules and standalone library compiles

Pins belong to a program, so a standalone `troupec -l` compile of a file that imports a module has
no pins to enforce and fails:

```
no pin for module "./Counter" (Counter) imported from Wrapper.trp; run 'troupec --update-deps' on the main program to establish it
```

Modules importing other modules is the supported case; those compiles happen inside the program's
build, under the program's pins.

## Datatypes across modules

A module exports every `datatype` group declared in its header, as `datatype <group-hash>
<canonical-form>` lines in its `.exports`. The importer parses the canonical form, recomputes the
hash, and rejects the interface if the stored hash disagrees (`compiler/src/SynVarFolding.hs:215`).

Constructor resolution then works across the module boundary the same way it works across a library
boundary. With `import "./modsrc/ModuleShapes"`, all four spellings resolve, in expression and in
pattern position (`tests/rt/pos/modules/module-datatype-import.trp`):

```sml
CIRCLE 3                       (* bare *)
shape.CIRCLE 2                 (* datatype-qualified *)
ModuleShapes.RECT (2, 5)       (* module-qualified *)
ModuleShapes.shape.CIRCLE 1    (* module- and datatype-qualified *)
```

Both sides desugar to the same tag strings, so a value built by the module's own compiled
constructor is matched by the importer's independently compiled pattern. A local `datatype` may
reference an imported one in an `of` clause; that reference is recorded by the imported group's
hash.

Three properties worth knowing:

- **Selection does not restrict datatypes.** `import { render } "./text/Greeting"` brings only
  `render` into the value namespace, but the module's constructors are imported wholesale — they
  are compile-time only.
- **`qualified` does restrict them.** Under `import qualified "./text/Greeting" as G`, the
  constructors are reachable as `G.PLAIN` or `G.tone.PLAIN`, not bare.
- **Datatype interfaces are not re-exported transitively.** If module `M` imports module `N` and
  exports a function returning one of `N`'s constructors, a program that imports only `M` does not
  get `N`'s constructors. Because an unresolved capitalized name in a pattern is an ordinary
  variable pattern, this is silent: the pattern matches everything. Import `N` directly to pattern
  match on its constructors.

The load-time datatype version-skew check described in
[VARIANTS.md](VARIANTS.md#across-libraries-and-modules) applies to libraries only. An importer
records no consumed-hash entry for a module import (`SynVarFolding.hs:206`, `:226`), because a change
to a module's declarations changes the module's own IR hash and is caught at compile time by the
pin check instead.

## Sending a closure that references a module

A closure that calls a module export can be serialized: sent to an actor on another node, or
written with `save` and read back with `restore`. There is no send-side restriction. (Remote
`spawn` serializes and deserializes through the same code path, `rt/src/runtimeMonitored.mts:166`.)

**The module code does not travel.** The closure's serialized function IR carries its module
dependency as the string `module:<hash>` and the name it uses from that module. Decoding the
persisted form of a closure `fn s => Stamp.stamp s` shows exactly that:

```
wrap40 ... gensym105 ;module:lbrn5kic1lu9hcug3mprn435a6qkjq33idevk91j68l4s884evrg stamp ...
```

**The receiver relinks against its own dependencies.** On deserialization the runtime resolves each
`module:<hash>` through the receiver's own dependencies file (`rt/src/loadLibsAsync.mts:42`,
`rt/src/moduleResolver.mts:45`). There is no remote fetch and no fallback: the receiver either has a
module with that exact hash among its own pinned dependencies and links it, or it has none and the
whole value is rejected. Since identity is content, "has that hash" does not mean "has it at the
same path" — a receiving program with its own copy of the module at a different path links it.

Across two nodes: node A sends `fn s => Stamp.stamp s` to node B; node B, holding its own copy of
`Stamp.trp`, applies the received closure and prints `[net]`.

**When the receiver does not have the module**, linking fails with

```
cannot link module module:<hash>: it is not among this program's dependencies
```

Where that failure surfaces depends on the entry point, and the two entry points behave
differently:

| Entry point                          | Behaviour                                                              |
|--------------------------------------|------------------------------------------------------------------------|
| `restore`                            | A thread-level Troupe error: `Error restoring value: cannot deserialize inbound value: cannot link module module:<hash>: it is not among this program's dependencies` |
| A message received from another node | The `DeserializationError` is classified as expected inbound input and the message is dropped (`rt/src/runtimeMonitored.mts:239`, `rt/src/deserialize.mts:200`) |
| A remote `spawn`                     | Same classification; the spawn is rejected (`rt/src/runtimeMonitored.mts:182`) |

On the network path the drop is reported through `debug`/`qdebug` and not to the program, so a node
that ignores a message may be missing a module the sender has.

Two properties limit what a received module-bearing closure can do:

- it runs only module code the receiver already has, matched by hash, so a sender cannot substitute
  code for a module name;
- module code has no ambient authority (see
  [What is not in scope inside a module](#what-is-not-in-scope-inside-a-module)), so linking a
  module does not hand the closure any of the receiver's authority.

The interaction of module loading with the pc and with blocking labels is not characterized here.

## Worked example

```
moddemo/
  hello.trp
  text/Greeting.trp
```

`text/Greeting.trp` is the module shown in [Writing a module](#writing-a-module). The program:

```sml
(* hello.trp *)
import "./text/Greeting"

let val a = render (PLAIN, "world")
    val b = Greeting.render (Greeting.tone.LOUD, "world")
in print a; print b end
```

Establish the pins, then run. Both commands are issued from the directory that contains `moddemo/`;
`troupec` is `$TROUPE/bin/troupec` and `local.sh` is `$TROUPE/local.sh`.

```
$ troupec --update-deps moddemo/hello.trp
  + text/Greeting  39iec28751hjo458ptvhl08p4i5vnbk2n4nfb5ai571i6cspklu0 (new)
wrote 1 module pin(s) to moddemo/hello.deps.json

$ local.sh moddemo/hello.trp
"hello, world"
"HELLO, world!"
>>> Main thread finished with value: ()@{}%{}
```

Afterwards the tree holds the two artifacts and the dependencies file:

```
moddemo/hello.deps.json
moddemo/hello.trp
moddemo/out/hello.js
moddemo/text/Greeting.trp
moddemo/text/out/Greeting.exports
moddemo/text/out/Greeting.js
```

Editing `Greeting.trp` and running again reports the stale pin:

```
module "./text/Greeting" (text/Greeting) imported from hello.trp has content hash
84jumpcssj64ssblmtq8g3ak0bk0f3cplptrnfmtrjqjp1vc9h10 but the dependencies file pins
39iec28751hjo458ptvhl08p4i5vnbk2n4nfb5ai571i6cspklu0; re-run 'troupec --update-deps' if this
change is intended
```

(The message is one line; it is wrapped here.)

Larger examples in the tree:

- `examples/savina/` — `Savina.trp` aggregates thirty per-benchmark modules under `benchmarks/`;
  each thin driver (`pingpong.trp`, …) imports `"./Savina"`.
- `examples/benchmarks/{clbg,datastructures,labeled-savina}/` — suites that import their own
  descriptor module and share `../../savina/SavinaReport`.
- `tests/rt/pos/modules/` and `tests/cmp/modules/` — the golden tests for each behaviour and each
  error below.

## Failure modes

Every message below is the compiler's or runtime's actual output.

| Situation                                              | Message                                                             |
|--------------------------------------------------------|---------------------------------------------------------------------|
| Path does not start with `./` or `../`                 | `invalid module import "ModuleEscape" in module-import-bad-path.trp: the path must start with "./" or "../"` |
| No such source file                                    | `cannot find module "./ModuleNoSuchFile" imported from module-import-missing.trp (no ModuleNoSuchFile.trp)` |
| Import cycle                                           | `module import cycle: modsrc/ModuleCycleA.trp -> modsrc/ModuleCycleB.trp -> modsrc/ModuleCycleA.trp` |
| Two imports bind the same name                         | `two imports bind the name 'List'; use 'as' to disambiguate`        |
| Selective import of a name the module does not export  | `Module "./modsrc/ModuleCollideHelper" does not export: nosuchexport` |
| Module body is not an export list                      | `parse error: libraries need to use restricted syntax for their main body` |
| No dependencies file, or no entry for this module      | `no pin for module "./Counter" (Counter) imported from Wrapper.trp; run 'troupec --update-deps' on the main program to establish it` |
| Pinned hash disagrees with the module's content        | `module "./lib/Counter" (lib/Counter) imported from main.trp has content hash <actual> but the dependencies file pins <pinned>; re-run 'troupec --update-deps' if this change is intended` |
| Dependency's `.exports` has no `module-hash` line      | `module "./Greeting" imported from W.trp has no content hash in out/Greeting.exports (recompile it)` |
| Dependency has no compiled interface                   | `module "./Fresh" imported from Wrapper3.trp is not compiled (no out/Fresh.exports)` |
| Malformed dependencies file                            | `malformed dependencies file hello.deps.json: Unexpected end-of-input, expecting JSON value` |
| Dependencies file missing at run time                  | Node `ENOENT` on the recorded `__moduleDepsFile` path               |
| Received closure names a module the receiver lacks     | `cannot link module module:<hash>: it is not among this program's dependencies` — on stdout from `restore`, at debug level on the network path |

The structural checks run before the pin check, so a program with both a bad selective import and a
stale pin reports the selective import (`ProcessImports.hs:162-170`).

Three of these errors — *no pin*, *no content hash*, *not compiled* — reach a well-formed program
only through a compile that is not driven by a main program, that is, a `troupec -l` compile of a
file that imports a module. A normal program compile recompiles the whole module graph first, so
every dependency has a fresh interface with its current hash by the time it is read.

## Where this is implemented

| Concern                                                      | File                                              |
|--------------------------------------------------------------|---------------------------------------------------|
| Import grammar, default bound name                           | `compiler/src/Parser.y`                           |
| Path checking, resolution, graph discovery, pin enforcement  | `compiler/src/ProcessImports.hs`                  |
| Dependencies file format, read and write                     | `compiler/src/DepsFile.hs`                        |
| Module hash                                                  | `compiler/src/ModuleHash.hs`                      |
| `.exports` interface format                                  | `compiler/src/Exports.hs`                         |
| `module:<hash>` codegen name                                 | `compiler/src/Core.hs`                            |
| Driver: module-graph compile, `--update-deps`                | `compiler/app/Main.hs`                            |
| Datatype interfaces of imports                               | `compiler/src/SynVarFolding.hs`                   |
| `__moduleRoot` and `__moduleDepsFile` emission               | `compiler/src/Stack2JS.hs`                        |
| Hash-to-artifact resolution, display names                   | `rt/src/moduleResolver.mts`                       |
| Module and library loading, linking                          | `rt/src/loadLibsAsync.mts`                        |
| Closure serialization and relinking                          | `rt/src/serialize.mts`, `rt/src/deserialize.mts`  |
