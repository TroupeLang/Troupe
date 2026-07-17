# Architecture

> **Scope:** how Troupe is structured internally — the compilation pipeline, the runtime, the
> information-flow-control model, and file extensions. For making changes (e.g. adding a built-in)
> see [CONTRIBUTING.md](CONTRIBUTING.md); for the networking layer see [NETWORKING.md](NETWORKING.md).

## Compilation pipeline

The compiler driver is `compiler/app/Main.hs`; its `process` function runs the stages below in
order. The list is a simplification — consult `Main.hs` for the authoritative sequence.

1. **Parsing** (`Parser.y`, `Lexer.x`) — parse `.trp` files into an AST.
2. **Front end:**
   - Ambient-method injection (`AddAmbientMethods.hs`, Normal compile mode only).
   - Import processing (`ProcessImports.hs`).
   - Export extraction (`Exports.hs`) when compiling a library (`-l`).
3. **Core transformations:**
   - Atom folding (`AtomFolding.hs`).
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

## Runtime architecture

The runtime implements:

- **Actor system** — process spawning, message passing, mailbox management
- **Information flow control** — security levels, label tracking, declassification
- **P2P networking** — libp2p integration for distributed actors
- **Built-in functions** — `rt/src/builtins/`
- **Level system** — label implementations in `rt/src/levels/`

Key components:

| File                   | Role                                 |
|------------------------|--------------------------------------|
| `troupe.mts`           | Main entry point                     |
| `runtimeMonitored.mts` | Gluing point for most of the runtime |
| `Scheduler.mts`        | Scheduler                            |
| `MailboxProcessor.mts` | Message handling                     |
| `TrustManager.mts`     | Trust and security management        |
| `p2p/p2p.mts`          | P2P networking layer                 |
| `builtins/`            | Language built-ins                   |

## External resource access

Every runtime operation that reaches outside the program — standard streams, persistence, the
network registry, and file I/O — is gated on authority rather than on ordinary label flow. With the
exception of `send` (governed by wire label/trust checks), these operations require **full (ROOT)
authority**: `stdio` defaults its level to ROOT, and `persist`, `cliargs`, `exit`, and `register`
call `assertIsRootAuthority`.

### File I/O (`SimpleFileIO`)

Whole-file read/write lives in `rt/src/builtins/simplefileio.mts`. It is a **placeholder** — a
deliberately small surface (`readFile`, `writeFile`, `appendFile`, `fileExists`) that exists to
support document-processing programs and is expected to be superseded by a labelled-path model.

- **Authority.** Every operation requires ROOT authority (mirrors `persist`). Untrusted code cannot
  reach the filesystem at all, so per-write confidentiality checks and per-path levels are deferred
  rather than half-answered.
- **Labeling.** Read content is labeled at ROOT ("we trust our own files"), exactly as `persist`
  labels restored data. Each primitive returns a `Result` (`{tag="Ok",…}` / `{tag="Err",{reason,
  path}}`), so a missing file or rejected path never crashes the thread.
- **Sandbox.** `--io-root <dir>` (`rt/src/TroupeCliArgs.mts`) bounds path reachability, orthogonal
  to authority: `..`, absolute-outside, and symlink escapes are rejected before any filesystem
  access, so even a bug in ROOT code cannot write outside the subtree. When unset, a per-invocation
  scratch directory is used, keeping observable output hermetic without a runtime flag. Error
  payloads carry the caller-supplied (io-root-relative) path, never the resolved absolute path.
- **Deferred to the revision:** non-ROOT/parameterized I/O levels, per-path label manifests,
  write-confidentiality checks, bounded-integrity read content, quarantine integration, and
  streaming/handle-based access.

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
  `BigInt.show` yields the plain decimal digits. `BigInt.fromString` and `BigInt.toInt` return
  `Result` records (`toInt` fails beyond exact double range).
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

| Extension  | Meaning                     |
|------------|-----------------------------|
| `.trp`     | Troupe source files         |
| `.golden`  | Expected test outputs       |
| `.exports` | Library export definitions  |
