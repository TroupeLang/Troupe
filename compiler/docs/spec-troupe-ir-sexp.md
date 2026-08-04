# `troupe-ir-sexp` — a textual s-expression format for the Troupe IR

A **textual s-expression syntax for Troupe's intermediate representation (IR)**, together with a
**printer**, a **parser**, and the **round-trip law** they must satisfy.

This document is **self-contained and portable**: it defines the format with no dependency on any
other document or on any external repository, and is meant to be dropped straight into the Troupe
repo. The audience is an agent working in the **Troupe repository** (the Haskell compiler under
`compiler/`).

- **Status:** implemented in the Troupe repo — `compiler/src/Sexp.hs` (datum layer and the `Sexp`
  class), the `Sexp` instances in the modules defining each type, and `compiler/src/IRSexp.hs` (the
  document layer). Exercised by `compiler/test/ir-sexp-test` and by `troupec --emit-ir-sexp` /
  `--ingest-ir-sexp` / `--verify-ir-sexp`. Last re-verified against the Troupe sources 2026-07-28:
  every type transcribed under *Source of truth* matches `compiler/src/IR.hs` / `Core.hs` /
  `Basics.hs` / `DCLabels.hs` verbatim.
- **Format:** `troupe-ir-sexp`. A readable, round-trippable textual encoding of the full Troupe IR
  AST (`compiler/src/IR.hs`), structured DC-label literals included.
- **Versioning:** version `2`. Every document is wrapped in `(troupe-ir-sexp VERSION …)`; format
  identification and the versioning policy are specified inline (§ *Format identification and
  versioning*).

## Purpose and scope

The Troupe IR (`compiler/src/IR.hs`) is today serialized only as `cereal` binary (`Serialize`) and
pretty-printed one-way (`ppProg` → `out/out.ir`, no parser). This spec adds a **readable,
round-trippable textual format**:

- a **printer** `IRProgram → Text` (s-expression form), and
- a **parser** `Text → Either Error IRProgram`,

such that parsing a printed program reproduces it exactly. The motivating consumer is an external
tool that needs to **emit Troupe IR programs** (constructed elsewhere) and have the Troupe compiler
**ingest and run them** — i.e. enter the backend at the IR stage instead of the parser. But the
format stands on its own as a general Troupe facility (debugging, golden tests, hand-written IR).

**In scope:** the full IR AST (every constructor of every type below); the structured DC-label
literal; the s-expression lexical layer; the round-trip law; the conventions a *runnable* whole
program must satisfy.

**Out of scope (non-goals):** how the format is wired into `troupec` (a flag, a mode, a standalone
tool — implementer's choice); the runtime semantics of labels/flows; and any consumer-side
translation that *produces* the IR.

## Source of truth — the AST being concretized

The format is a 1:1 concretization of these Haskell types. Field order below is the order used in
the s-expression. (Locations are optional — see *Source positions are optional*.)

From `compiler/src/IR.hs`:

```haskell
data IRProgram   = IRProgram [LFunDef]
data FunDef      = FunDef HFN LVarName Consts IRBBTree          -- name, arg, consts, body
newtype HFN      = HFN Ident                                    -- Ident = String
type    Consts   = [(VarName, C.Lit)]
data IRBBTree    = BB [LIRInst] LIRTerminator

data IRInst      = Assign VarName IRExpr
                 | MkFunClosures [(VarName, LVarAccess)] [(VarName, HFN)]

data IRExpr      = Bin Basics.BinOp LVarAccess LVarAccess
                 | Un  Basics.UnaryOp LVarAccess
                 | Tuple [LVarAccess] Basics.SynVariantTag      -- tag: is this a variant value?
                 | Record LFields                               -- [(FieldName, LVarAccess)]
                 | WithRecord LVarAccess LFields
                 | ProjField LVarAccess Basics.FieldName
                 | ProjIdx LVarAccess Word
                 | List [LVarAccess]
                 | ListCons LVarAccess LVarAccess
                 | Const C.Lit
                 | Base Basics.VarName                          -- builtin, e.g. "$$authorityarg"
                 | Lib Basics.LibName Basics.VarName

data IRTerminator= TailCall LVarAccess LVarAccess
                 | Ret LVarAccess
                 | If LVarAccess IRBBTree IRBBTree
                 | AssertElseError LVarAccess IRBBTree LVarAccess
                 | LibExport LVarAccess
                 | Error LVarAccess
                 | StackExpand VarName IRBBTree IRBBTree

data VarAccess   = VarLocal VarName | VarEnv VarName | VarFunSelfRef
```

From `compiler/src/Core.hs`:

```haskell
data Numeric = NumInt Integer | NumFloat Double
data Lit     = LNumeric Numeric | LString String | LLabel String
             | LDCLabel DCLabelExp | LUnit | LBool Bool
```

From `compiler/src/Basics.hs`:

```haskell
type VarName = String;  type FieldName = String;  type SynVariantTag = Bool
newtype LibName = LibName String
data BinOp   = Plus | Minus | Mult | Div | Mod | Eq | Neq | Le | Lt | Ge | Gt
             | And | Or | RaisedTo | Concat | IntDiv
             | BinAnd | BinOr | BinXor | BinShiftLeft | BinShiftRight | BinZeroShiftRight
             | HasField | LatticeJoin
data UnaryOp = IsList | IsTuple | IsRecord | Head | Tail
             | ListLength | TupleLength | RecordSize | LevelOf | UnMinus | Not
```

From `compiler/src/DCLabels.hs` (the structured label literal):

```haskell
type Tag         = String
data LabelOp     = Conj | Disj
data LabelExp    = TagExp Tag | OpExp LabelOp LabelExp LabelExp
data LabelConst  = LabelTrue | LabelFalse                        -- shown #true / #false
data LabelComponent = ExprComponent LabelExp | ConstComponent LabelConst
newtype DCLabelExp  = DCLabelExp (LabelComponent, LabelComponent) -- (confidentiality, integrity)
```

**Why this grammar is stable under language growth.** Troupe's entire primitive surface — the
downgrade family (`declassify`, `endorse`, `downgrade`, `declassifyType`, `endorseType`,
`downgradeType`), the blocking-label family (`blockdeclto`, `blockendorseto`, `blockdownto`, …),
the mailbox primitives (`peek`, `consume`, `guard`, `raisembox`, `lowermbox`), the actor layer
(`spawn`, `self`, `send`, `receive`), `attenuate`, `levelOf`, and every other builtin — enters the
IR as `Base VarName` applied via ordinary calls, i.e. as a *string*, not as an AST constructor. New
primitives therefore never change this grammar; they only add names the runtime binds. The format
is a fixed point of the IR's *structure*, which has been stable for years, so the spec can be held
as an independent contract while the primitive set evolves on both sides.

**Source positions are optional (version 2).** `LVarAccess = Located VarAccess`,
`LFunDef = Located FunDef`, `LVarName = Located VarName`, `LIRInst`/`LIRTerminator = Located …`
all carry source positions, from

```haskell
data PosInf = SrcPosInf String Int Int | RTGen String | NoPos   -- file, line, column
```

A located value with a position is written inside an `@` wrapper; a value at `NoPos` is written
as its payload alone:

```
(@ ("examples/foo.trp" 12 3) (local "x"))    ; SrcPosInf
(@ (rt "description") (local "x"))           ; RTGen
(local "x")                                  ; NoPos
```

Any node may therefore appear with or without a wrapper, and a producer that has no positions to
offer emits none. Two round-trip laws follow (see *Round-trip law*): with positions the AST comes
back exactly, and a position-erased document comes back position-erased.

Version 1 of this format could not represent positions at all. A version-1 document is a
version-2 document that happens to have no `@` wrappers, but the version atom is part of the text
and that text is a module's content-addressed identity (`compiler/src/ModuleHash.hs`), so the
version bump repinned every module in the repository.

## Lexical layer

The surface is standard s-expressions, whitespace-insensitive.

- **Lists:** `( … )`, elements separated by whitespace. Nesting unrestricted.
- **Comments:** `;` to end of line. Ignored.
- **Symbols (keywords / operators / constructors):** match `[A-Za-z_$!*+/<>=?-][A-Za-z0-9_$.!*+/<>=?-]*`.
  Used for node heads (`program`, `fun`, `bb`, `assign`, …), operators (the `BinOp`/`UnaryOp`
  constructor names), and the label-constant tokens `#true` / `#false`.
- **Strings:** double-quoted, with escapes `\" \\ \n \t \r \uXXXX`. **All names** — `VarName`, the
  `HFN` ident, `LibName`, `FieldName`, label `Tag` — are written as **quoted strings**,
  because Troupe identifiers legitimately contain `$`, `.`, etc. (e.g. `"$$authorityarg"`,
  `"$env.x"`). (A reader MAY also accept a bare symbol where a name is expected, but the printer
  always quotes.)
- **Integers:** optional `-`, then digits (`LNumeric (NumInt _)`). `NumInt` is `Integer` —
  unbounded; the reader must not truncate to a machine word.
- **Floats:** `LNumeric (NumFloat _)`, an IEEE-754 double. Two implementations will not spell the
  same value the same way — Haskell renders `1.0e-3` where JavaScript renders `0.001`, and
  `1.0` where JavaScript renders `1` — and that is allowed: what is fixed is the syntax every
  reader must **accept**, not the syntax a writer must **emit**.

  ```
  FLOAT ::= ['-'] digit+ ['.' digit+] [('e'|'E') ['+'|'-'] digit+]
          | ['-'] "Infinity"
          | "NaN"
  ```

  A writer MUST emit a shortest round-trip representation, so that reading its output recovers the
  same double. It MUST NOT emit a leading `+`, a leading `.` (`.5`), or a trailing `.` (`1.`):
  Haskell's reader rejects the first two outright and consumes only the `1` of the third.

  **Non-finite values are representable and do occur.** An overflowing source literal such as
  `1.0e400` lexes through `read` into an infinity, so the IR can hold one, and `Infinity` /
  `-Infinity` are the spellings both Haskell's `reads` and JavaScript's `Number` accept and
  produce. `NaN` is accepted for completeness; no source construct produces it.

  **A caveat for the laws below:** structural equality is not reflexive on `NaN`, so
  `parse (print p) = p` cannot hold for a document carrying one. A conforming implementation
  checks that case by asking whether the decoded value is a NaN, not whether it equals the
  original (`compiler/test/ir-sexp-test` does this).

  **Negative zero** is a distinct value that survives Haskell's `show`/`read` as `-0.0`, but
  JavaScript's `String(-0)` is `"0"` — an implementation printing through it must special-case
  the sign, or it silently loses the distinction.

## Grammar — node by node

Notation: UPPERCASE non-terminals; `…*` zero-or-more; `STRING` a quoted string; `INT`/`FLOAT`
numbers. Each rule names the AST constructor it denotes.

**Program, functions, blocks.**

```
PROGRAM   ::= (program FUN*)                             -- IRProgram [FunDef …]
FUN       ::= (fun STRING (arg STRING) CONSTS BBTREE)    -- FunDef (HFN name) arg consts body
CONSTS    ::= (consts (STRING LIT)*)                     -- [(VarName, Lit)]
BBTREE    ::= (bb (INST*) TERM)                          -- BB [IRInst] IRTerminator
```

**Instructions (`IRInst`).**

```
INST ::= (assign STRING EXPR)                            -- Assign VarName IRExpr
       | (mkclos (CAP*) (CLO*))                          -- MkFunClosures
CAP  ::= (STRING VARACCESS)                              --   captured-env binding (VarName ← VarAccess)
CLO  ::= (STRING STRING)                                 --   (VarName, HFN) closure created
```

**Expressions (`IRExpr`).**

```
EXPR ::= (bin BINOP VARACCESS VARACCESS)                 -- Bin
       | (un  UNOP  VARACCESS)                           -- Un
       | (tuple VARACCESS*)                              -- Tuple … False (an ordinary tuple)
       | (tuple-variant VARACCESS*)                      -- Tuple … True  (a syntactic-variant value)
       | (record (STRING VARACCESS)*)                    -- Record
       | (with-record VARACCESS (STRING VARACCESS)*)     -- WithRecord
       | (proj-field VARACCESS STRING)                   -- ProjField … FieldName
       | (proj-idx VARACCESS INT)                        -- ProjIdx … Word  (0 ≤ INT ≤ 2^31−1, per IR.hs)
       | (list VARACCESS*)                               -- List
       | (cons VARACCESS VARACCESS)                      -- ListCons
       | (const LIT)                                     -- Const Lit
       | (base STRING)                                   -- Base VarName     (e.g. "$$authorityarg")
       | (lib STRING STRING)                             -- Lib LibName VarName
```

**Terminators (`IRTerminator`).**

```
TERM ::= (tail-call VARACCESS VARACCESS)                 -- TailCall f arg
       | (ret VARACCESS)                                 -- Ret
       | (if VARACCESS BBTREE BBTREE)                    -- If cond then else
       | (assert-else-error VARACCESS BBTREE VARACCESS)  -- AssertElseError
       | (lib-export VARACCESS)                          -- LibExport
       | (error VARACCESS)                               -- Error
       | (stack-expand STRING BBTREE BBTREE)             -- StackExpand dst body cont
```

**Variable access (`VarAccess`).**

```
VARACCESS ::= (local STRING)                             -- VarLocal VarName
            | (env STRING)                               -- VarEnv VarName
            | self                                       -- VarFunSelfRef
```

## Literals and operators

**Literals (`Lit`).**

```
LIT ::= (int INT)                                        -- LNumeric (NumInt _)
      | (float FLOAT)                                    -- LNumeric (NumFloat _)
      | (string STRING)                                  -- LString
      | (bool true) | (bool false)                       -- LBool
      | unit                                             -- LUnit
      | (label-string STRING)                            -- LLabel String  (legacy; raw surface text)
      | DCLABEL                                          -- LDCLabel DCLabelExp  (preferred; below)
```

**Operator tokens.** Operators are written as their **Haskell constructor name, verbatim**, as a
symbol — chosen for an unambiguous, trivially round-trippable 1:1 mapping (the implementer MAY add
readable aliases the parser also accepts, but the printer emits the constructor name).

```
BINOP ::= Plus | Minus | Mult | Div | Mod | Eq | Neq | Le | Lt | Ge | Gt
        | And | Or | RaisedTo | Concat | IntDiv
        | BinAnd | BinOr | BinXor | BinShiftLeft | BinShiftRight | BinZeroShiftRight
        | HasField | LatticeJoin
UNOP  ::= IsList | IsTuple | IsRecord | Head | Tail
        | ListLength | TupleLength | RecordSize | LevelOf | UnMinus | Not
```

Note the label-relevant operators are ordinary `BinOp`s/`UnaryOp`s, not special forms: `RaisedTo`
(`x raisedTo lev`), `LatticeJoin` (`⊔`), `LevelOf`.

## Labels and authorities — the structured encoding

**DC-label literal (`LDCLabel DCLabelExp`).** A DC label is a **pair** `(confidentiality, integrity)`,
each a *component* that is either a constant or a tag-formula:

```
DCLABEL   ::= (dclabel COMPONENT COMPONENT)              -- DCLabelExp (conf, integ)
COMPONENT ::= #true                                      -- ConstComponent LabelTrue
            | #false                                     -- ConstComponent LabelFalse
            | LABELEXP                                   -- ExprComponent LabelExp
LABELEXP  ::= (tag STRING)                               -- TagExp Tag
            | (and LABELEXP LABELEXP)                    -- OpExp Conj
            | (or  LABELEXP LABELEXP)                    -- OpExp Disj
```

Semantics for orientation (not part of the syntax; defined by the runtime / `DCLabels.hs`):
`#true` ≡ the empty conjunction (`CNF []`, the *bottom* / flows-everywhere component); `#false` ≡ the
empty disjunction (`CNF [DisjTags []]`, the *top* / unsatisfiable component). A tag is a principal.
`and`/`or` are conjunction/disjunction of principal formulas (the DC-label CNF, Stefan et al.).

**Tag case.** `LabelExp` preserves the tag string **as written** (case included); lowercasing happens
only when converting to CNF (`labelExpToCNF`). The printer therefore emits the stored tag verbatim
and the parser preserves it — round-trip is **syntactic**, independent of the runtime's
case-insensitive *semantic* equality (`dcLabelEq`).

`and`/`or` are binary (mirroring `OpExp`); n-ary conjunctions/disjunctions are right-nested
(`(and a (and b c))`). A reader MAY accept n-ary `and`/`or` as sugar, but the printer emits binary.

**Authorities** are **not** literals. They are produced two ways, both represented with ordinary IR
nodes already in this grammar:

1. **The ambient authority** — the program/function authority parameter, accessed as the builtin
   `(base "$$authorityarg")`. (The entry function `"main"` receives `$$authorityarg` as its argument;
   see *Conventions for a runnable whole program*.)
2. **Attenuation** — an authority weakened to a level, produced by *calling the attenuation builtin*
   `(base "attenuate")` on a 2-tuple `(authority, level)` (an ordinary call — a `stack-expand`/
   `tail-call` over a `tuple` — not a new node). Per `rt/src/builtins/attenuate.mts`: if the
   authority does not act for the requested level, the result is the authority clamped to `⊥`, not
   an error.

The format thus needs no authority-specific syntax; an emitter expresses authorities through these
existing constructs.

## The interchange laws (acceptance properties)

The point of the format is that two implementations can exchange IR. Conformance is stated on the
**decoded value**, never on bytes or on text: gzip streams from different compressors differ while
remaining mutually readable, and the s-expression layer is whitespace-insignificant, so neither
compression output nor layout is part of the format.

Let `print`/`parse` range over documents and `encodeBlob`/`deserialize` over blobs (see *Blobs*).

> **L1 (round trip).** For every well-formed `p`, `parse (print p) = Right p'` where `p'` is
> structurally equal to `p`, positions included. Printing with positions erased instead gives back
> the position-erased program.
>
> **L2 (text ingestion).** An implementation parses a document another implementation printed and
> obtains a structurally equal value.
>
> **L3 (blob ingestion).** An implementation decompresses and decodes a blob another
> implementation produced and obtains a structurally equal value. The blob bytes may differ.
>
> **L4 (framing).** `deserialize (encodeBlob u) = Right u` for every serialization unit `u`.
>
> **L5 (rejection).** A blob that is not one an encoder produced is refused, as a value the caller
> can act on rather than as a crash or a hang.

Structural equality is the derived `Eq` on `FunDef`/`IRExpr`/`Lit`/… — for `DCLabelExp` the
*syntactic* `Eq`, preserving the `LabelExp` tree, not the semantic `dcLabelEq` — with the `NaN`
caveat noted under *Floats*.

**Making L2 and L3 observable.** Do not compare one implementation's values with another's through
a canonical text; there is no canonical text. Compare inside one implementation: parse the other
side's artifact with your own reader and compare against your own value using your own equality.
The check is then symmetric, and neither side needs to reproduce the other's layout or compression.

**What the laws do not require.** Identical text, identical byte length, identical gzip output,
identical float spelling, identical line breaking. An implementation may pretty-print, or emit
everything on one line, and remain conformant.

Checked in this repository by `compiler/test/ir-sexp-test` (L1 over generated and hand-built IR),
`compiler/test/ir-sexp-corpus` (L1 over every program in the test corpus, module graphs included),
`compiler/test/ir-sexp-conformance` (L1–L4 over the reference documents and blobs, including one
compressed by Node rather than Haskell and one produced by the second implementation), and
`scripts/ir-blob-interchange.mjs` (the other direction of the compression check).

The second implementation is `trp-compiler/IR.trp` and `trp-compiler/Blob.trp`, written in Troupe.
`scripts/ir-sexp-troupe-conformance.sh` runs the same laws inside it, over the same corpus, using
Troupe's structural equality; `make test/local` runs both sides. `make test/ir-sexp-corpus-troupe`
widens the text half from the six reference programs to every program in `tests/rt/pos`: each is
compiled, printed, decoded and re-printed by the Troupe implementation, and parsed back here, with
the two IR values compared.

**A reader must also refuse.** Accepting a malformed blob is a conformance failure in the same way
as rejecting a valid one, since blobs arrive from other nodes. Both implementations are held to the
same list of malformations — truncations, a foreign identifier, an unreadable version, a flipped
byte, a payload that is not a gzip stream, one that is not UTF-8, a decompression bomb, and a valid
stream with trailing bytes after it. A blob is a header and exactly one gzip stream: input after
that stream is refused rather than ignored, so that neither side admits what the other rejects.

## Format identification and versioning

Every artifact is **self-identifying and versioned**, so a reader can verify it is the Troupe-IR
s-expression format (and a version it supports) *before* parsing, and so the format can evolve
without silent misreads. The format is a contract between two independently-evolving implementations;
a self-identifying, versioned wrapper lets either side detect a mismatch immediately and loudly
instead of mis-parsing.

**The top-level format wrapper.** Every document is wrapped in a top-level form naming the format and
its version. This wrapper is the outermost production, replacing `(program …)` at the top level;
`(program …)` is nested inside it, and nothing else in the grammar changes:

```
DOCUMENT ::= (troupe-ir-sexp VERSION PROGRAM)
VERSION  ::= a positive integer literal           ; current = 2
PROGRAM  ::= (program FUN*)
```

Example (the `a + b` program, wrapped):

```
(troupe-ir-sexp 2
  (program
    (fun "main" (arg "$$authorityarg")
      (consts)
      (bb ((assign "a" (const (int 1)))
           (assign "b" (const (int 2)))
           (assign "c" (bin Plus (local "a") (local "b"))))
          (ret (local "c"))))))
```

**Printer / parser requirements.**

- **Printer** emits the wrapper with the current version (`2`).
- **Parser** MUST check the head symbol is exactly `troupe-ir-sexp` and that `VERSION` is one it
  supports; otherwise it **rejects with a clear error** — it must not attempt to parse an unknown
  head or version. This is the loud-on-drift guard, and it also distinguishes this format from
  sibling s-expression formats (e.g. a Lean-machine format whose head is `lean-ir-sexp`).
- **The round-trip law** ranges over the **wrapped** document: `parse (print p) = Right p` with the
  wrapper present and the version preserved. The property test should generate wrapped documents.

**Versioning policy.**

- This spec defines **version 2**.
- Bump the integer on any **backward-incompatible** grammar change (a renamed, removed, or retyped
  node). Purely additive changes need not bump — a `v1` parser may then loudly reject the unknown
  head, which is acceptable.
- Maintain a changelog mapping each version number to its grammar revision.

**Changelog.**

- `1` — initial grammar.
- `2` — optional source positions (the `@` wrapper); `(program …)` no longer carries an `(atoms …)`
  list, atoms having been retired from the language.

## Blobs — the mobile-code framing

A blob is how a serialization unit travels: embedded per function in emitted JavaScript
(`this.<fn>.serialized`, base64) and carried between nodes when a closure moves.

```
bytes 0-3 : "TRPI"                     format identifier
byte  4   : version, currently 2
bytes 5.. : gzip stream (RFC 1952) of the UTF-8 encoded document
```

The document inside is an ordinary `(troupe-ir-sexp 2 …)` whose body is either `(fun …)` — one
function, the mobile-code case — or `(program …)`. Positions are carried: the receiving compiler
rebuilds a source map for the relinked code from them.

A reader MUST reject a blob whose identifier is absent or whose version it does not implement, and
MUST bound decompression output (this implementation caps it at 64 MB) — blobs arrive from remote
nodes, so a small input inflating without limit is a denial of service.

Any conforming gzip implementation may produce the stream: Haskell's `zlib`, Node's `zlib` and a
browser's `CompressionStream('gzip')` all interoperate, which L3 exists to check rather than
assume. Compression level and therefore byte length are implementation choices.

## Conventions for a *runnable* whole program

The format faithfully represents any `IRProgram`; **running** one additionally requires:

- **Entry:** the entry function is the `FunDef` named `"main"` (`HFN "main"`), and its argument is
  `"$$authorityarg"` (the ambient authority). (Cf. `ClosureConv.hs`: the top-level pair is
  `("$$authorityarg", "main")`.)
- **Ambient names:** `print` and its neighbours are base functions, resolved by the runtime like
  any other builtin, so an `IRProgram` referring to them carries nothing extra and needs nothing
  injected.
- **Backend path:** from an `IRProgram`, the existing backend runs `IR2Raw.ir2raw → RawOpt.rawopt →
  Raw2Stack.raw2Stack → Stack2JS.stack2JSWithMappings` (the whole-program codegen used by the normal
  compile in `app/Main.hs`), producing a runnable `.mjs`. (The `--json-ir`/`--text-ir` stdin modes
  are *fragment* compilers — `stack2JSON` errors on a whole `ProgramStackUnit` — so they are **not**
  the whole-program path.)

## Worked examples

**`a + b`.**

```
(program
  (fun "main" (arg "$$authorityarg")
    (consts)
    (bb ((assign "a" (const (int 1)))
         (assign "b" (const (int 2)))
         (assign "c" (bin Plus (local "a") (local "b"))))
        (ret (local "c")))))
```

**A value-producing `if` (via `StackExpand`).**

```
(fun "main" (arg "$$authorityarg")
  (consts)
  (bb ()
    (stack-expand "r"
      (bb () (if (local "$$authorityarg")        ; (schematic cond)
                 (bb () (ret (const (int 10))))
                 (bb () (ret (const (int 20))))))
      (bb ((assign "s" (bin Plus (local "r") (const (int 5)))))
          (ret (local "s"))))))
```

**A structured label and `raisedTo`.**

```
; x raisedTo `<alice ∨ bob ; #root-integrity-as-#false-top>`
(assign "s"
  (bin RaisedTo
    (local "x")
    (const (dclabel (or (tag "alice") (tag "bob")) #false))))
```

**Closures (`mkclos`).**

```
; create closures f (=fun#"f") and g (=fun#"g") over a shared env capturing local x:
(assign "_"
  ; (mkclos is an INST, shown standalone for illustration)
  (const unit))
(mkclos (("x" (local "x")))            ; captured-env bindings
        (("f" "f") ("g" "g")))         ; (VarName, HFN) closures created
```

## Deliverables and acceptance

The Troupe-side implementation provides:

1. **Printer** `IRProgram → Text` in the canonical form (R2). Recommended: have it also back the
   existing `-v` IR dump so `out/out.ir` and this format share one syntax (optional but reduces
   drift).
2. **Parser** `Text → Either Error IRProgram`, tolerant per the lexical/R2 rules, with clear errors.
3. **Round-trip property test** (R1) over generated `IRProgram`s — the primary acceptance criterion.
4. Coverage of **every** constructor in *Source of truth* (full IR), including labels.

**Open items (for the implementer to resolve in-repo).**

- Ambient-methods/prelude strategy for runnable standalone programs.
- Whether to expose the printer/parser as a `troupec` flag/mode or a standalone executable — not
  constrained here.
- `LLabel String` (raw surface label text) is retained for fidelity (it exists in `Lit`), but
  emitters SHOULD prefer the structured `dclabel` form; a reader must accept both.
