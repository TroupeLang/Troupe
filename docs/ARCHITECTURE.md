# Architecture

> **Scope:** how Troupe is structured internally — the compilation pipeline, the runtime, the
> information-flow-control model, and file extensions. For making changes (e.g. adding a built-in)
> see [CONTRIBUTING.md](CONTRIBUTING.md); for the networking layer see [NETWORKING.md](NETWORKING.md).

## Compilation pipeline

The compiler transforms source through several stages:

1. **Parsing** (`Parser.y`, `Lexer.x`) — parse `.trp` files into an AST.
2. **Core transformations:**
   - Pattern elimination (`DirectWOPats.hs`)
   - Function/let lowering
   - Alpha renaming
   - CPS transformation (`RetCPS.hs`, `RetDFCPS.hs`)
   - CPS optimization (`CPSOpt.hs`)
   - Closure conversion (`ClosureConv.hs`)
3. **Code generation:**
   - IR → Raw (`IR2Raw.hs`)
   - Raw → Stack (`Raw2Stack.hs`)
   - Stack → JavaScript (`Stack2JS.hs`)

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

Troupe implements dynamic information flow control with:

- **Security levels** — High/Low, DC labels, custom lattices
- **PC (program counter) label** — tracks implicit flows
- **Declassification** — controlled information release
- **Sandboxing** — isolated execution with label constraints

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
