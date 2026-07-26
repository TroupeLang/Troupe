# Syntactic variants

Example programs for the syntactic-variants feature: ML-style `datatype`
declarations, constructors used as values and functions, and constructor
patterns.

A `datatype` declaration introduces named constructors, each optionally
carrying a payload described by an `of` clause:

```sml
datatype 'a option = none | some of 'a
```

Constructors are values (nullary, e.g. `none`) or functions (with a payload,
e.g. `some 5`). They match in patterns anywhere a pattern is allowed: case
heads, function clauses, tuple and list elements, and receive handlers. A bare
constructor application used as a whole function argument must be parenthesised:
`fun f (some x) = ...`. A value prints as its constructor name (`none`) or as
`(c payload)` (`(some 5)`).

Mutually recursive datatypes are grouped with `and`:

```sml
datatype expr = lit of int | block of stmt * expr
     and stmt = assign of string * expr | seq of stmt * stmt
```

An `and` group must be genuinely mutually recursive — each member must reach
every other through payload references — otherwise it is a static error.
Scoping is sequential: a datatype may refer only to members of its own `and`
group and to datatypes declared earlier, so dependencies are declared before
dependents. When a constructor name is declared by more than one datatype in
scope, a use is disambiguated with the datatype-qualified form `t.c`
(`color.red`).

## Examples

| File               | What it is                                                                     |
|--------------------|--------------------------------------------------------------------------------|
| `calculator.trp`   | Evaluator and infix pretty-printer over an arithmetic-expression datatype; division by zero reported through a result datatype. |
| `interpreter.trp`  | Interpreter for a small imperative language built on mutually recursive `expr`/`stmt` datatypes; environment held in a `Map`.   |
| `shapes.trp`       | Area and perimeter over a shape datatype with float tuple payloads; map and fold over a list of shapes.                         |
| `actors.trp`       | Bank-account process whose message protocol is a datatype; server loop matches constructor patterns in `receive`.              |
| `json-lite.trp`    | Recursive JSON-like value datatype with an array payload (`value list`) and a renderer to a string.                            |
| `lambda-calculus.trp` | Call-by-value interpreter for the untyped lambda calculus (extended with if-then-else and integer arithmetic) over an AST datatype, with a mutually recursive value/environment `and` group. Builds the Z combinator as an AST value to define a doubly recursive fib inside the object language — and a paired-Z mutual fixed-point combinator over Church pairs to declare a mutually recursive even/odd group — then times interpreted fib against native Troupe fib and reports the slowdown ratio. Split across program-relative modules in `lambda/` (see below), and renders terms two ways: a precedence-aware layout through `Pretty` that breaks to fit a width, and a structural s-expression through `Sexp` that reads back into the same term. |
| `definitional-interpreters.trp` | The recursion strategies of Reynolds' *Definitional Interpreters* (1972) on the same object language, extended with a native `fixt` construct: Z combinator only, first-order recursive-closure unrolling, meta-circular higher-order, CPS, and a defunctionalized CEK-style machine (a three-way mutually recursive `and` fun group over a continuation datatype). Times fib 18 under every strategy against native Troupe fib.                     |
| `interpreter-benchmarks.trp` | The interpreter-overhead benchmarks of the literature (fac, fib, tak, ack, power, and Romer-style per-construct micros; sources cited in the header) ported to the object language and run under the naive Z-combinator interpreter against native Troupe twins.                                                                                                                        |
| `bench.trp`          | The one-place benchmark runner for the whole interpreter-overhead study: all suites (macro benchmarks, recursion strategies, environment representations, per-node micros), selectable via `key=value` CLI arguments, rendering markdown or machine-readable JSON through the `Json` library; every row self-validates against the native result. See the header for usage.             |

## The `lambda/` modules

`lambda-calculus.trp` is the one multi-module program here. Its parts are
program-relative modules, imported by path rather than by name:

| Module                | What it holds                                                          |
|-----------------------|------------------------------------------------------------------------|
| `lambda/Term.trp`     | The `term`, `value` and `env` datatypes, and a node count              |
| `lambda/Eval.trp`     | `lookup`, `numOf`, `eval`, and `run` for a closed term                 |
| `lambda/Print.trp`    | Precedence-aware `Pretty` layout, and `Sexp` rendering both ways       |
| `lambda/Programs.trp` | fib and even/odd as object-language terms, via fixed-point combinators |

Datatype constructors travel with the module that declares them, and not
transitively: a file that builds or matches terms imports `"./lambda/Term"`
itself, even when it also imports `Eval` or `Print`, which import it too.

`lambda-calculus.deps.json` pins the content hash of each module. A hash that
does not match the module on disk is a compile error; after editing a module,
re-pin with

```bash
./bin/troupec --update-deps examples/variants/lambda-calculus.trp
```

`definitional-interpreters.trp` and `interpreter-benchmarks.trp` declare their
own `term` datatypes, with constructors the shared one does not have (a native
`fixt`, primitives as a `prim` datatype), so they do not use these modules.

## Running

Every program but `lambda-calculus.trp` is a single self-contained file; that one
compiles its modules as a side effect of compiling the driver. Run one with:

```bash
./local.sh examples/variants/NAME.trp --localonly
```

`--localonly` skips p2p initialisation. `actors.trp` uses `spawn`/`send`/
`receive` and requires it.
