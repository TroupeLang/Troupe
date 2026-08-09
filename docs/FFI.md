# Native modules (`require native`)

> **Scope:** the `require native` declaration, native-module manifests, how a native name is
> resolved at compile time, how the dependency is recorded in the serialized IR, how a runtime
> decides availability, and how to write a native module for the node host. For library imports
> and the compilation pipeline see [ARCHITECTURE.md](ARCHITECTURE.md); for program modules see
> [MODULES.md](MODULES.md).

A **native module** is a named group of host-implemented primitives. Its interface is declared by
a manifest file; its implementation lives in the runtime of each host that provides it. A program
states its dependency with a `require native` declaration, and the dependency is recorded through
every representation down to the serialized IR, so a runtime that lacks the module rejects the
code cleanly instead of failing at the first call.

Native code does not travel. What a native module has instead of one identity-carrying artifact
is one implementation per host, and a host that has no implementation does not have the module:

|                | Library                            | Module                            | Native module                             |
|----------------|------------------------------------|-----------------------------------|-------------------------------------------|
| Declared       | `import List`                      | `import "./util/Fmt"`             | `require native SimpleFiles`              |
| Implemented in | Troupe (`lib/*.trp`)               | Troupe (a program-local `.trp`)   | The host runtime (TypeScript on node)     |
| Interface      | `$TROUPE/lib/out/<Name>.exports`   | `<dir>/out/<Name>.exports`        | `$TROUPE/ffi/<Name>.exports` manifest     |
| Identity       | Its name                           | Content hash of its compiled IR   | Its name                                  |
| Availability   | Any runtime with the compiled lib  | Receivers holding the same hash   | Per host: what the host registered        |

## Declaration forms

`require native` takes the same `qualified`, selection, and `as` modifiers as an `import`
declaration (`compiler/src/Parser.y`, the `ImportDecl` production), and require declarations sit
in the same header section as imports, in any order relative to them. `require` and `native` are
reserved words.

| Form                                    | Effect                                                                 |
|-----------------------------------------|------------------------------------------------------------------------|
| `require native FFIDemo`                | Binds the manifest names unqualified; also reachable as `FFIDemo.name` |
| `require native FFIDemo as F`           | Renames the qualified path to `F`; the unqualified names stay bound    |
| `require native qualified FFIDemo`      | Names reachable only as `FFIDemo.name`                                 |
| `require native { ffiDemoAdd } FFIDemo` | Only `ffiDemoAdd` is in scope, unqualified and as `FFIDemo.ffiDemoAdd` |

Two declarations that bind the same name are rejected, exactly as two imports are. A library may
carry a `require native` of its own: `lib/SimpleFileIO.trp` requires `SimpleFiles`, and a program
that imports `SimpleFileIO` needs no require of its own.

## Manifests

The compile-time interface of a native module `Name` is the manifest `$TROUPE/ffi/Name.exports`,
in the `.exports` line format (`compiler/src/Exports.hs`): one value name per line, with
`fixity` lines allowed. `ffi/SimpleFiles.exports`:

```
readFile
readFileBytes
writeFile
writeFileBytes
appendFile
fileExists
readDir
makeDir
fileStat
removeFile
```

Three restrictions, each rejected when the manifest is read:

- no `module-hash` lines — a native module has no content-addressed identity;
- no `datatype` lines — a native module exports values only;
- no name that is a core base function (`compiler/src/BaseFunctions.hs`) — the ambient names must
  not depend on which manifests are installed.

The manifest declares the interface; each host's registry provides the implementation. Nothing
forces the two to agree by construction, so the test corpus exercises every manifest name
(`tests/rt/pos/ffi/`, `tests/lib/SimpleFileIO.trp`): a name registered but not declared is
unreachable, and a name declared but not registered fails those tests.

## Resolution and the missing-require error

A `require native Name` is resolved against the manifest at compile time: an absent manifest is a
compile error, and a selection naming something the manifest does not declare is a compile error
(`compiler/src/ProcessImports.hs`, `processNativeRequire`).

Manifest names are not ambient. The renamer reads every installed manifest once per compile; an
unresolved name that some manifest declares does not fall through to the base functions but fails
with a targeted error naming the declaration to add (`compiler/src/Core.hs`, `lookforgen`):

```
troupec: 'ffiDemoGreet' is provided by native module 'FFIDemo': add 'require native FFIDemo'
```

Should two installed manifests declare the same name, the alphabetically first module wins in
this message; the declared requires of a program are unaffected.

## Wire representation

A native reference compiles to the library namespace with the `native:` prefix: a call is a
`Lib "native:Name" var` IR node, codegen emits an `addLib("native:Name", …)` link record, and the
serialized document wrapper carries the referenced modules in an optional `natives` element,
present exactly when the list is non-empty:

```
(troupe-ir-sexp 2
  (program …)
  (natives "FFIDemo" "SimpleFiles"))
```

The header is derived from the body by the printer and verified against the body by every
decoder; a disagreement in either direction is rejected. Native-free documents keep the
three-element wrapper byte for byte, so module hashes of native-free code are unchanged. The
grammar and the verification rule are specified in
[compiler/docs/spec-troupe-ir-sexp.md](../compiler/docs/spec-troupe-ir-sexp.md) (*The `natives`
header*).

## Runtime availability

Each host holds a registry of the native modules it provides (`rt/src/ffi/registry.mts`). The
node host registers its modules in `registerNodeNatives()` (`rt/src/ffi/node/index.mts`), called
from the entry point before the first library link. A `native:Name` link record resolves through
the registry (`rt/src/loadLibsAsync.mts`): no file, no instantiation, no dependencies of its own —
the registered export table is served as the values of an already-loaded library.

A name the host did not register raises

```
native module 'Name' is not available on this runtime
```

classified as expected inbound input (`rt/src/deserialize.mts`), so inbound code requiring an
absent native module follows the missing-module dispositions of
[MODULES.md](MODULES.md#sending-a-closure-that-references-a-module): a `restore` fails with a
thread-level error, a received message is dropped at debug level, a remote spawn is rejected.

Serialization is sender-side unrestricted: a closure over a native function serializes without
any availability check, and the receiver resolves `native:` names only through its own registry.
A sender therefore cannot cause execution of anything but the receiver's own implementations.

## Writing a native module (node host)

Three pieces, kept consistent by the tests described under [Manifests](#manifests):

1. **The manifest** — `ffi/<Name>.exports`, the declared value names.
2. **The implementation** — a file under `rt/src/ffi/node/` exporting a table with one `mkBase`
   entry per manifest name. The table has the same shape as a loaded library's values table, and
   the entries follow the same conventions as builtins: `mkBase` from
   `rt/src/builtins/UserRuntimeZero.mts` wraps the function, `rt/src/Asserts.mts` checks
   arguments, and the runtime is reached through `getRuntimeObject()` (`rt/src/SysState.mts`) —
   there is no mixin `this`. The implementation is responsible for its own label discipline and,
   where it touches host resources, its own authority checks.
3. **The registration** — one `nativeModules.register('<Name>', <table>)` line in
   `registerNodeNatives()`. Registering a name twice throws
   `native module '<Name>' is already registered` at startup.

Module imports must be side-effect-free: each host assembles its own registration set, so
importing an implementation file must not decide what is registered or touch host resources.
Work that needs the process environment runs at first use — `simplefiles.mts` resolves its
`--io-root` sandbox on the first file operation, not at import.

Worked examples in the tree:

| File                              | Demonstrates                                                        |
|-----------------------------------|---------------------------------------------------------------------|
| `rt/src/ffi/node/ffidemo.mts`     | Minimal platform-neutral module; result labels join argument labels |
| `rt/src/ffi/node/simplefiles.mts` | Authority checks, sandboxing, suspending the thread for async I/O   |

## Failure modes

Every message below is the compiler's or runtime's actual output.

| Situation                                            | Message                                                                 |
|------------------------------------------------------|-------------------------------------------------------------------------|
| No manifest for the required name                    | `unknown native module 'NoSuchNative': no $TROUPE/ffi/NoSuchNative.exports` |
| Manifest name used without its require               | `troupec: 'ffiDemoGreet' is provided by native module 'FFIDemo': add 'require native FFIDemo'` |
| Selection names something the manifest lacks         | `native module 'FFIDemo' does not export: noSuchFn`                     |
| Two declarations bind the same name                  | `two imports bind the name 'FFIDemo'; use 'as' to disambiguate`         |
| Manifest carries a `module-hash` line                | `invalid native module manifest $TROUPE/ffi/BadDemo.exports: module-hash lines are not allowed` |
| Manifest carries a `datatype` line                   | `invalid native module manifest $TROUPE/ffi/BadDemo.exports: datatype lines are not allowed` |
| Manifest declares a base function                    | `invalid native module manifest $TROUPE/ffi/BadDemo.exports: 'print' is a core base function` |
| `restore` of a value requiring an absent module      | `Error restoring value: cannot deserialize inbound value: native module 'AbsentModule' is not available on this runtime` |
| Received message requiring an absent module          | Dropped; reported at debug level, not to the program                    |
| Remote spawn requiring an absent module              | Rejected, same classification                                           |
| Host registers one module name twice                 | `native module '<Name>' is already registered` (a host defect, at startup) |

## Where this is implemented

| Concern                                             | File                                           |
|-----------------------------------------------------|------------------------------------------------|
| Grammar, reserved words                             | `compiler/src/Parser.y`                        |
| Manifest reading, selection check, providers map    | `compiler/src/ProcessImports.hs`               |
| Missing-require error, `native:<Name>` codegen name | `compiler/src/Core.hs`                         |
| Base-function whitelist                             | `compiler/src/BaseFunctions.hs`                |
| `natives` header derivation and printing            | `compiler/src/IR.hs`, `compiler/src/IRSexp.hs` |
| Wire-format specification                           | `compiler/docs/spec-troupe-ir-sexp.md`         |
| Registry, availability error                        | `rt/src/ffi/registry.mts`                      |
| Node-host registrations                             | `rt/src/ffi/node/index.mts`                    |
| Link-time resolution                                | `rt/src/loadLibsAsync.mts`                     |
| Inbound-error classification                        | `rt/src/deserialize.mts`                       |
| Demonstration module                                | `rt/src/ffi/node/ffidemo.mts`                  |
| Whole-file I/O module (`SimpleFiles`)               | `rt/src/ffi/node/simplefiles.mts`              |
