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

| Extension                            | Meaning                    |
|--------------------------------------|----------------------------|
| `.trp`                               | Troupe source files        |
| `.picox`, `.pico`, `.femto`, `.atto` | Test file variants         |
| `.golden`                            | Expected test outputs      |
| `.exports`                           | Library export definitions |
