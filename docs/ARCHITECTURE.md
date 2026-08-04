# Architecture

> **Scope:** how Troupe is structured internally — the compilation pipeline, the runtime, the
> information-flow-control model, and file extensions. For making changes (e.g. adding a built-in)
> see [CONTRIBUTING.md](CONTRIBUTING.md); for the networking layer see [NETWORKING.md](NETWORKING.md).
> The module system and syntactic variants each have their own document —
> [MODULES.md](MODULES.md) and [VARIANTS.md](VARIANTS.md).

## Compilation pipeline

The compiler driver is `compiler/app/Main.hs`. `main` resolves the file's module graph and then
calls `process` once per module (dependencies before consumers) and once for the file itself;
`process` runs the stages below in order. The list is a simplification — consult `Main.hs` for the
authoritative sequence.

0. **Module graph** (`main`, `ProcessImports.discoverModules`) — for a non-library compile,
   discover the program-relative modules the file imports transitively and load the pins recorded
   in `<main>.deps.json` (`DepsFile.hs`). Each module is compiled first, as a content-hashed
   library artifact. See [MODULES.md](MODULES.md).
1. **Parsing** (`Parser.y`, `Lexer.x`) — parse `.trp` files into the parse-phase AST
   (`Surface.hs`): expressions carry flat operator chains with no precedence commitment, and the
   file header may carry fixity declarations. See [OPERATORS.md](OPERATORS.md).
2. **Front end:**
   - Import processing (`ProcessImports.hs`) — resolve library and module imports, read their
     `.exports` interfaces, and either enforce or establish the dependency pins.
   - Operator re-association (`OpReassoc.hs`) — translate `Surface` into the `Direct` AST,
     rebuilding each operator chain from the fixity environment (built-ins seeded; user
     operators from the header declarations and the imported `fixity` interface lines).
   - Export extraction (`Exports.hs`) when compiling a library or module (`-l`).
   - Syntactic-variant folding (`SynVarFolding.hs`, hashing in `SynVarHash.hs`) — process
     `datatype` groups, rewriting constructor occurrences and constructor patterns into tagged
     tuples and tuple patterns. See [VARIANTS.md](VARIANTS.md). A constructor may shadow an
     ambient built-in.
3. **Core transformations:**
   - Pattern-match / case elimination (`CaseElimination.hs`). `DirectWOPats.hs` is the
     pattern-free AST representation these produce, not a driver pass.
   - Function/let lowering and alpha renaming (`Core.hs`).
   - CPS transformation (`RetDFCPS.hs`). `RetCPS.hs` supplies the CPS IR datatype consumed by
     later passes; it is not itself a driver pass.
   - CPS optimization (`CPSOpt.hs`).
   - Closure conversion (`ClosureConv.hs`) producing IR.
   - IR optimization (`IROpt.hs`).
4. **Code generation:**
   - IR → Raw (`IR2Raw.hs`).
   - Raw optimization (`RawOpt.hs`), unless `--no-rawopt` is passed.
   - Raw → Stack (`Raw2Stack.hs`).
   - Stack → JavaScript (`Stack2JS.hs`), with optional source-map embedding (`-m`).
   - For a library or module compile, the `.exports` interface is written alongside the `.js`.

`IRSexp.hs` prints and parses the optimized IR as s-expression text. It is not part of the default
path: `--emit-ir-sexp` stops after IR optimization and writes the text, `--verify-ir-sexp` prints
and re-parses it and checks the position-erased ASTs match, and `--ingest-ir-sexp` reads such a file
and runs stage 4 on it.

With `-v`, each stage writes a dump into `out/` under the working directory: `out.syntax`,
`out.opreassoc`, `out.nopats`, `out.lowered`, `out.alpha`, `out.cps`, `out.cpsopt`, `out.ir`,
`out.iropt`, `out.rawout`, `out.rawopt`, `out.stack`. `out.rawopt` is not written under
`--no-rawopt`. Dumps are
written for the top-level file only, not for the modules compiled ahead of it.

## Runtime architecture

The runtime implements:

- **Actor system** — process spawning, message passing, mailbox management
- **Information flow control** — security levels, label tracking, declassification
- **P2P networking** — libp2p integration for distributed actors
- **Built-in functions** — `rt/src/builtins/`
- **Level system** — label implementations in `rt/src/levels/`

Key components:

| File                   | Role                                                          |
|------------------------|---------------------------------------------------------------|
| `troupe.mts`           | Main entry point                                              |
| `runtimeMonitored.mts` | Gluing point for most of the runtime                          |
| `Scheduler.mts`        | Scheduler                                                     |
| `MailboxProcessor.mts` | Message handling                                              |
| `TrustManager.mts`     | Trust and security management                                 |
| `TroupeCliArgs.mts`    | Runtime command-line options                                  |
| `loadLibsAsync.mts`    | Loads the compiled libraries and modules a program imports    |
| `moduleResolver.mts`   | Maps a module's content hash to its artifact (see MODULES.md) |
| `serialize.mts`        | Value serialization for the wire                              |
| `deserialize.mts`      | Value deserialization from the wire                           |
| `p2p/p2p.mts`          | P2P networking layer                                          |
| `builtins/`            | Language built-ins                                            |

`loadLibs.mts` is marked deprecated in its own header and is not imported anywhere under `rt/src/`.

## External resource access

Every runtime operation that reaches outside the program — persistence, the network registry, and
file I/O — requires authority rather than ordinary label flow. Two exceptions: `send`, governed by
wire label/trust checks, and stdio (below), checked by label flow at the operations. The rest
require **full (ROOT) authority**: `persist`, `cliargs`, `exit`, `register`, and the
`SimpleFileIO` primitives call `assertIsRootAuthority`.

### Standard streams (stdio)

`rt/src/builtins/stdio.mts` and `rt/src/builtins/tty.mts`. stdio is a pair of channels at a level
`L`, set by `--stdiolev` (default ROOT) in either V1 (`'{alice}'`) or V2
(`'<alice;#root-integrity>'`) syntax.

`stdin`, `stdout` and `stderr` are **values**, not something acquired: a descriptor names a stream
and grants nothing, so there is nothing for it to show and nothing to check. Enforcement sits on
the operations:

- **Observations** drawn through a channel — a line from `freadln`, a pushed terminal event,
  `ttyIsTTY`, `ttySize` — are labelled at `L`.
- **Effects** on a channel — `fwrite`, consuming a line, a raw-mode change, arming or disarming
  event delivery — are checked `flowsTo(lub(pc, operand levels), L)` after raising the pc to the
  blocking label. The check runs in both label dimensions: what comes from the channel is
  `L`-data, and what goes to the channel must flow to `L`.

**The ambient names.** `print`, `printString`, `printWithLabels`, `fwriteln`, `fwritelnWithLabels`
and `inputLine` are runtime builtins, available to a program, a module and a library on the same
terms. `print` and its two neighbours write a line to stdout; `fwriteln` and `fwritelnWithLabels`
take an explicit descriptor, which is the form to reach for when the destination is a parameter.

**Receive-then-lower-the-blocking-label needs a channel whose integrity something can act for.**
`lib/Tty.trp`'s `nextEventAtLevel` closes with `blockdecl auth`, and the blocking label it lowers
was raised by the channel. ROOT is fine and a named principal is fine; a channel at
`#null-integrity` leaves nothing to declassify from, and the receive path fails there even though
the write path does not.

Terminal support beyond the streams: `ttyIsTTY`/`ttySize`/`ttyLevel` (queries), `ttyRawMode`
(line-discipline switch; entering raw mode closes the lazily-created readline interface, which a
later `freadln` re-creates), and `ttySubscribe`/`ttyUnsubscribe` (event delivery). Events are
runtime-built mailbox messages with string tags — `("TTYDATA", bytes)` one per chunk,
latin1-decoded; `("TTYRESIZE", cols, rows)`; the bare string `"TTYEOF"` — delivered on the
network's mailbox ingress path with presence and payload at `L`. There is exactly one
subscriber, module-private, replaced on re-subscribe; if it has died, the next event tears the
subscription down and leaves raw mode. `lib/Tty.trp` names the events as a datatype and
packages the receive ceremony (a ranged-receive region up to `L`). `ttyRestore`, run from
`cleanupAsync` for every program, detaches the listeners, resets raw mode if the runtime set it,
and pauses stdin; it restores termios, not screen state, and no signal handlers are installed —
an external SIGTERM leaves a raw terminal raw.

### File I/O (`SimpleFileIO`)

Whole-file read/write lives in `rt/src/builtins/simplefileio.mts`. It is a **placeholder** — a
deliberately small surface (`readFile`, `writeFile`, `readFileBytes`, `writeFileBytes`,
`appendFile`, `fileExists`) that exists to support document-processing programs and is expected to
be superseded by a labelled-path model.

- **Authority.** Every operation requires ROOT authority (mirrors `persist`). Untrusted code cannot
  reach the filesystem at all, so per-write confidentiality checks and per-path levels are deferred
  rather than half-answered.
- **Labeling.** Read content is labeled at ROOT ("we trust our own files"), exactly as `persist`
  labels restored data. Each primitive returns a tagged record
  (`{tag="Ok", value=…}` / `{tag="Err", error={reason, path}}`), so a missing file, a rejected path
  or a file `readFile` cannot decode never crashes the thread. The standard library reports failure
  with `Option` and `Outcome`; the runtime still builds tagged records here, in the bigint
  conversion built-ins, and in the two decoding codec built-ins (below), and those three are the
  only places the encoding survives.
- **Text or bytes.** `readFile`, `writeFile` and `appendFile` are UTF-8, and `readFile` decodes
  with a fatal decoder: a file whose bytes are not UTF-8 is `{reason="file is not valid UTF-8"}`
  rather than a string with U+FFFD where those bytes were, a substitution the program cannot see
  and a later write would put on disk. `readFileBytes` and `writeFileBytes` carry the bytes
  themselves, one code unit per byte, the byte-string convention of the codec built-ins (below)
  and of the tty reader; `writeFileBytes` refuses a code unit above 255 as a thread error, as
  `base64Encode` does. Appending is UTF-8 only.
- **Sandbox.** `--io-root <dir>` (`rt/src/TroupeCliArgs.mts`) bounds path reachability, orthogonal
  to authority: `..`, absolute-outside, and symlink escapes are rejected before any filesystem
  access, so even a bug in ROOT code cannot write outside the subtree. When unset, a per-invocation
  scratch directory is used, keeping observable output hermetic without a runtime flag. Error
  payloads carry the caller-supplied (io-root-relative) path, never the resolved absolute path.
- **Deferred to the revision:** non-ROOT/parameterized I/O levels, per-path label manifests,
  write-confidentiality checks, bounded-integrity read content, quarantine integration, and
  streaming/handle-based access.

### Compression and base64 (`BytesAndZips`)

`rt/src/builtins/codec.mts` provides `gzip`/`gunzip` and `base64Encode`/`base64Decode`, the
primitives a Troupe program needs in order to read and write the serialized IR blob that carries
mobile code (`compiler/src/IRBlob.hs`). The blob framing is not among them: it is assembled by
Troupe code (`trp-compiler/Blob.trp`), so the format version has one home, in the compiler.

- **Bytes are strings.** Troupe has no byte arrays, so compressed and decoded data is carried in
  strings whose characters are code units in 0..255. `gzip` produces one, `base64Encode` consumes
  one, and a string carrying anything above 255 is refused rather than truncated. `gzip` likewise
  refuses a string with no UTF-8 encoding — one holding an unpaired surrogate — which Node would
  otherwise turn into U+FFFD, altering the value in a round trip that reported success.
- **Failure is a value where the input is foreign.** `gunzip` and `base64Decode` return tagged
  records, wrapped as `Outcome` by `lib/BytesAndZips`: a blob arriving from another node may be
  truncated, corrupt or a decompression bomb, and the language has no exception handling, so a
  thread error would be unrecoverable. A malformed *argument* stays a thread error.
- **Bombs.** `gunzip` caps output at the 64 MiB the compiler enforces, so a small blob that expands
  without bound fails as a value rather than exhausting memory.
- **Labels.** All four carry their argument's label unchanged, tag included — whether a blob decoded
  is a fact about that blob.

## Arbitrary-precision integers (bigint)

Bigints are a base value type backed by JavaScript BigInt, distinct from numbers.

- **Syntax.** A bigint literal is a decimal digit run with an `n` suffix (`123n`). The parser
  desugars the literal to the `bigFromLiteral` built-in, so no compiler phase past parsing knows
  about bigints. Literals are expressions only; they are not accepted in patterns.
- **Operations.** All bigint arithmetic goes through named built-ins (`rt/src/builtins/bigint.mts`),
  surfaced by [lib/BigInt.trp](../lib/BigInt.trp) (`add`, `sub`, `mul`, `quot`, `rem`, `neg`,
  `cmp`, comparison predicates, conversions). The ordinary arithmetic operators are number-only and
  reject bigints at their type asserts; there is no implicit mixing.
- **Semantics.** `getType` reports `"bigint"`. Equality is kind-first: two bigints compare by
  value; a bigint never equals a number or a string. Bigints print in literal form (`5n`);
  `BigInt.show` yields the plain decimal digits. The built-ins `bigFromString` and `bigToInt` return
  tagged records built in the runtime; `BigInt.fromString` and `BigInt.toInt` convert those to an
  `Option`, so `NONE` reports failure (`toInt` fails beyond exact double range).
- **Representation.** A bigint is boxed (`rt/src/TroupeBigInt.mts`) so it can carry the runtime
  type tag; the label rides the enclosing labeled value like every base type, and every built-in
  joins the current pc into its result label, matching the labeling of number literals. On the
  wire (`serialize.mts`/`deserialize.mts`) a bigint travels as a decimal string.
- `getNanoTime` returns a bigint (nanoseconds); it was unusable before this type existed.

## Information flow control

Troupe implements dynamic information flow control:

- **Security levels** — values carry labels drawn from a lattice. Concrete lattice implementations
  live in `rt/src/levels/`: DC labels (`DCLabels/`), a singleton lattice (`singleton.mts`), and
  tag sets (`tagsets.mts`).
- **PC (program-counter) label** — tracks implicit flows through control flow.
- **Blocking label** — a per-thread label maintained alongside the PC (see the pini stack below).
- **Sandboxing** — isolated execution under label constraints (tests under
  `tests/rt/pos/ifc/sandbox/`).

### Downgrading: declassification and endorsement

Downgrading moves a value's label in a way ordinary computation cannot; every downgrading operator
takes an authority argument. Two dimensions are supported (`rt/src/DowngradeEnums.mts`,
`DowngradeDimension`):

- **Confidentiality** downgrading is **declassification**.
- **Integrity** downgrading is **endorsement**.
- A cross-dimensional **downgrade** changes both at once (`BOTH`).

The value-downgrading operators are built in `rt/src/builtins/declassify.mts`:

| Operator          | Dimension       | Granularity                     |
|-------------------|-----------------|---------------------------------|
| `declassify`      | confidentiality | value and type                  |
| `endorse`         | integrity       | value and type                  |
| `downgrade`       | both            | value and type                  |
| `declassifyType`  | confidentiality | type label only (`TYPE_ONLY`)   |
| `endorseType`     | integrity       | type label only (`TYPE_ONLY`)   |
| `downgradeType`   | both            | type label only (`TYPE_ONLY`)   |

The `*Type` variants downgrade only the label associated with a value's type rather than its
contents; they support downgrading in control-flow positions such as conditionals. The result
labels produced by these operators follow the current sound model (recent commits `09f754c`,
`103b182`). The downgrade machinery itself is in `rt/src/downgrading.mts`, with dimension/kind
enumerations in `rt/src/DowngradeEnums.mts` and error/message formatting in
`rt/src/DowngradeFormatter.mts`.

Two related authority-based operators live under `rt/src/builtins/`: `attenuate` (`attenuate.mts`)
and `raiseTrust` (`raiseTrust.mts`).

### Non-malleable IFC

Downgrading carries two further conditions beyond the authority check, enforced by default and
disabled by `--no-nmifc`. Both are in `okToDowngradeGeneric`
(`rt/src/levels/DCLabels/dclabel.mts`), which cites the security-model document for them:

- **Robust declassification** — `(S_auth ∨ I_from ∨ I_pc) ∧ S_to ⟹ S_from`. A release must not be
  influenced by whoever could have written the data.
- **Transparent endorsement** — `I_from ⟹ I_to ∨ (S_from ∧ S_pc)`. A thread must not endorse what
  it cannot see.

A label `<S, I>` is **corrupt** when its integrity does not imply its confidentiality
(`DCLabel.isCorrupt`) — more secret than it is trusted. Robust declassification refuses to
downgrade from a corrupt label whatever authority is shown, so a corrupt label is one nothing can
move. `{}`, `{#ROOT}` and any self-dual `{p}` are not corrupt;
`<#root-confidentiality;#null-integrity>` is.

This reaches stdio through the channel level. Writing to a channel is not a downgrade, so a corrupt
channel level admits every write — but a thread that *reads* from such a channel takes the channel's
level into its blocking label and can never put it back down, in either dimension.
`enableRangedReceive` reports the same condition as its certification bit: an interval whose top is
corrupt yields `ok = false`, and that region can be received in but never closed.

Twenty-eight tests run at `<#root-confidentiality;#null-integrity>` because they print labelled
values and only need the write to be admitted. Each says so in its `.trp.options`:

```
# Note: this test requires the corrupt stdio level.
```

A self-dual level naming each test's own principals would be non-corrupt and would leave those
tests able to downgrade; the note is the thread to pull if that is wanted.

### The pini / blocking-label stack

The blocking label bounds where a thread may downgrade. It is managed as a stack through operators
in `rt/src/builtins/pini.mts`:

- `pinipush` (authority) raises the blocking level; `pinipop` (capability) restores it;
  `pinipushto` raises it to a specified level, requiring that level to flow to the current blocking
  level.
- Blocking-level downgrade operators tie the pini stack to downgrading: `blockdecl` / `blockdeclto`
  (confidentiality), `blockendorse` / `blockendorseto` (integrity), and `blockdown` / `blockdownto`
  (both). The `*to` forms take an explicit target level.

### Inspecting labels

`debugpc` (`rt/src/builtins/debugutils.mts`) prints the current PC and blocking labels for a running
thread; `debugMbox` prints mailbox contents with their labels. Use these to observe actual label
state rather than assuming lattice relationships.

### Note on DC Labels

All DC label tags are normalized to lowercase. For example, `` `<Alice ; Bob>` `` is equivalent to `` `<alice ; bob>` ``. This normalization is applied both at compile time (by the lexer) and at runtime.

The user guide currently uses the V1 label syntax `` `{alice}` ``, which the present Troupe runtime interprets as a DC Label corresponding to `` `<alice ; alice>` ``.

## File extensions

Source and compiler artifacts:

| Extension    | Meaning                                                          |
|--------------|------------------------------------------------------------------|
| `.trp`       | Troupe source files                                              |
| `.js`        | Compiled output                                                  |
| `.exports`   | Interface of a compiled library or module, written next to `.js` |
| `.deps.json` | A program's module dependencies file                             |
| `.tpnb`      | Notebook file (JSON), read and written by `notebook/`            |

A library or module compile writes its `.js` and `.exports` to `<dir>/out/<name>`; a program compile
writes to `-o`'s argument, or to `out/out.stack.js` under the working directory when `-o` is absent.
With `-m` the source map is embedded in the `.js`; no separate `.map` file is produced.

An `.exports` file is a line-oriented text interface (`compiler/src/Exports.hs`): an optional
leading `module-hash <hash>` line carrying a module artifact's own content-addressed identity (a
standard-library compile emits none), then one exported value name per line, then one
`datatype <group-hash> <canonical-form>` line per exported datatype group in declaration order, then
one `fixity <l|r|n> <level> <name>` line per exported operator — mandatory for a symbolic export, see
[OPERATORS.md](OPERATORS.md).

A program that imports program-relative modules has a `<main>.deps.json` next to it, pinning each
resolved module by path, content hash, and display name. See [MODULES.md](MODULES.md) and
[VARIANTS.md](VARIANTS.md).

Test-corpus artifacts (see [CONTRIBUTING.md](CONTRIBUTING.md#test-suite-layout)):

| Extension         | Meaning                                                     |
|-------------------|-------------------------------------------------------------|
| `.golden`         | Expected test output, compared against a colored run        |
| `.nocolor.golden` | Expected test output under `bin/golden --no-color`          |
| `.trp.input`      | Standard input fed to the test program                      |
| `.trp.options`    | Extra `local.sh` arguments; `#` lines are comments          |
