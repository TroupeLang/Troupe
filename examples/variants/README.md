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
| `lambda-calculus.trp` | Call-by-value interpreter for the untyped lambda calculus (extended with if-then-else and integer arithmetic) over an AST datatype, with a mutually recursive value/environment `and` group. Builds the Z combinator as an AST value to define a doubly recursive fib inside the object language, then times interpreted fib against native Troupe fib and reports the slowdown ratio. |

## Running

Each program is self-contained. Run one with:

```bash
./local.sh examples/variants/NAME.trp --localonly
```

`--localonly` skips p2p initialisation. `actors.trp` uses `spawn`/`send`/
`receive` and requires it.
