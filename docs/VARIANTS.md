# Syntactic variants

> **Scope:** the `datatype` declaration form, constructor expressions and patterns, what a variant
> value is at runtime, and how a declaration's content hash gives its constructors a global
> identity. For the compilation pipeline see [ARCHITECTURE.md](ARCHITECTURE.md); for imports across
> libraries and modules see [MODULES.md](MODULES.md).

Syntactic variants are sugar. A `datatype` declaration is compiled away in the frontend
(`compiler/src/SynVarFolding.hs`): a constructor becomes a tuple whose first element is a tag
string, and a constructor pattern becomes a tuple pattern whose first element is that string
literal. After that pass the program carries no declarations and no constructor patterns; the only
thing later phases carry is a boolean on each tuple-construction node, which becomes the one
runtime deviation described [below](#the-one-runtime-deviation). Everything else about how variants
behave at runtime follows from tuple, string, and literal semantics.

## Declaring a datatype

Declarations appear in the file header: after the `import` lines, before the program's main
expression. `datatype` inside a `let` is a parse error, and an `import` after a `datatype` is a
parse error (`compiler/src/Parser.y`, the `Prog` production).

```sml
datatype colour = red | green | blue
datatype shape  = circle of int | rect of int * int
```

Each constructor is a name, optionally followed by `of` and a type expression describing its
payload. A datatype may take type parameters, written with the tick syntax:

```sml
datatype 'a option        = none | some of 'a
datatype ('a, 'b) pair    = p of 'a * 'b
```

Payload type expressions are:

| Form                                               | Meaning                                                                            |
|----------------------------------------------------|------------------------------------------------------------------------------------|
| `'a`                                               | A type variable; it must appear in the datatype's parameter list                   |
| `int`, `float`, `bigint`, `bool`, `string`, `unit` | The fixed primitive type names                                                     |
| `t`                                                | A datatype in scope (same `and` group, or an earlier group, or imported)           |
| `t1 * ... * tn`                                    | An n-ary product, flat and non-associative; `(a * b) * c` differs from `a * b * c` |
| `{l1 : t1, ..., ln : tn}`                          | A record with distinct labels; `{}` is the empty record                            |
| `ty t`, `(ty1, ty2) t`                             | Postfix application of a parameterized datatype or of the built-in `list`          |
| `X.t`                                              | A datatype from the import qualified as `X`                                        |

The primitive set is `compiler/src/SynVarFolding.hs` `primNames`; `list` is the only built-in type
constructor. Payload types are not checked at runtime — Troupe is dynamically typed. They are
inputs to the declaration's identity (see
[Identity](#identity-normalization-and-hashing)) and nothing else. `rect 5`, where `rect` is
declared `of int * int`, builds a value; the mismatch surfaces only when a `rect (w, h)` pattern
fails to match it.

A record payload is destructured by a record pattern under the constructor pattern, and the
payload slot holds an ordinary record value:

```sml
datatype shape = circle of {r : int}
               | rect   of {w : int, h : int}

fun area (circle {r = r})      = 3 * r * r
  | area (rect {w = w, h = h}) = w * h
```

Record and product payloads differ in how identity treats component order. A product is
positional, so `of int * string` and `of string * int` are different declarations. A record is
addressed by label, so field order is normalized away: `of {a : int, b : string}` and
`of {b : string, a : int}` hash to the same tag and their constructors are interchangeable across
libraries. The labels themselves are part of the identity — renaming a field yields a different
tag. Repeating a label in one record type is a static error.

### Mutual recursion

Members of one declaration group are joined by `and`, and only members of the same group may refer
to each other or forward:

```sml
datatype expr = lit of int | pair of expr * expr | block of stmt
     and stmt = skip | assign of string * expr | seq of stmt * stmt
```

Scoping is sequential: a group sees previously declared groups and its own members. A reference to
a datatype declared in a later group is an unbound-name error. An `and` group of two or more
members must be genuinely mutually recursive — every member reachable from every other through
in-group payload references — otherwise it is a static error.

A datatype name may shadow an earlier datatype, a primitive, or `list`; the nearest declaration
wins. Two members of one `and` group may not share a name, and one datatype may not declare the
same constructor name twice.

## Using constructors

A nullary constructor is a value; a constructor with a payload is a one-argument function, usable
first class:

```sml
import List
datatype shape = circle of int | square of int

let val makers = [circle, square]        (* constructors held in a list        *)
    val xs     = map circle [1, 2, 3]    (* a constructor mapped over a list   *)
in print (case makers of (m :: _) => m 3);   (* (circle 3)                     *)
   print xs                                  (* [(circle 1), (circle 2), (circle 3)] *)
end
```

Constructor names may be declared by more than one datatype. A bare use resolves when exactly one
constructor of that name is in scope; otherwise it is a use-site error resolved by qualification:

| Form    | Resolves against                                                   |
|---------|--------------------------------------------------------------------|
| `c`     | The single constructor `c` in scope                                |
| `t.c`   | Constructor `c` of datatype `t`                                    |
| `X.c`   | Constructor `c` among the datatypes of the import qualified as `X` |
| `X.t.c` | Constructor `c` of datatype `t` of the import qualified as `X`     |

Ambiguity is measured by tag, not by name: candidates that resolve to the same tag — an identical
re-declaration, or two import paths to the same declaration — collapse and resolve silently.

Datatype names and value names are separate namespaces: `fun eval env = ...` is legal with a
datatype `env` in scope. The two meet only at a dotted head, where `env.frame` could be a record
projection off a value `env` or constructor access on the datatype `env`. When the head names both
a value in scope and a datatype declaring that constructor, the occurrence is a static error.

Two rules follow SML. A constructor name in a pattern is a constructor pattern — it matches, it
does not bind — and a constructor name cannot be the name of a recursive definition
(`fun red x = ...` with `red` a constructor is an error). A constructor declaration shadows an
ambient built-in of the same name: after `datatype colour = print of int`, bare `print` is the
constructor, and output goes through `printString`.

### Patterns

Constructor patterns are available wherever patterns are: case arms, function clauses, `receive`
handlers, and inside tuple and list patterns. A constructor application used as a whole argument
must be parenthesized, since a constructor pattern's own argument is an atomic pattern:

```sml
fun area (circle r)    = 3 * r * r
  | area (rect (w, h)) = w * h

receive [ hn (ping p) => ...
        , hn stop     => ... ]
```

A bare name in a pattern is a nullary-constructor pattern only when it names a constructor in
scope. A misspelled nullary constructor is therefore an ordinary binder and matches everything:
`case x of red => 1 | grean => 2` compiles, and the second arm always matches.

## What a variant is at runtime

Folding rewrites a nullary constructor to a 1-tuple holding the tag, and an applied constructor to
a unary lambda that builds a 2-tuple of the tag and its argument. For

```sml
datatype binop = ADD | SUB
datatype expr  = LIT of int | BIN of binop * expr * expr

let val e = BIN (ADD, LIT 8, LIT 4) in print e end
```

the post-folding term (`out/out.nopats` under `troupec -v`) is

```
(fn $arg1 => let val $synvar0 = $arg1
             in ("52lkqeu14qslt1b0fak7l0v3bheji43a44qoci30j035eq7v37b0#expr#BIN", $synvar0)
             end)
  (("lbf8abh66bb9uir0juna1l3pvvidvdsnhj3151va3ddhh470tia0#binop#ADD"),
   (fn $arg1 => ... "…#expr#LIT" ...) 8,
   (fn $arg1 => ... "…#expr#LIT" ...) 4)
```

and the emitted JavaScript builds the tuples directly:

```js
const _raw_11 = rt.mkTuple([gensym105], true);            // ADD, a 1-tuple
const _raw_32 = rt.mkTuple([gensym106, gensym117, gensym113], false);  // the 3-tuple payload
const _raw_39 = rt.mkTuple([gensym118, gensym109], true);  // BIN (tag, payload)
```

`BIN (ADD, LIT 8, LIT 4)` is thus a 2-tuple whose second slot is the plain 3-tuple of arguments;
the arity written in the `of` clause is the arity of that payload tuple, not of the outer value.

Because a variant *is* a tuple, the following are consequences of tuple, string, and literal
semantics rather than variant-specific rules:

| Question         | Follows from                                                             | Consequence                                                                                                              |
|------------------|--------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------|
| Equality         | Structural tuple equality; tags compare as strings                       | A variant is equal to the plain tuple with the same contents                                                             |
| Matching         | Constructor patterns are tuple patterns; the tag test is string equality | Tuple patterns destructure variants and expose the tag; a plain tuple carrying the tag satisfies the constructor pattern |
| Introspection    | Variants are tuples                                                      | `isTuple` is true; the length is 1 or 2                                                                                  |
| Level            | A tuple's level is the join of its elements' levels                      | Wrapping a payload adds the tag literal's level, nothing else                                                            |
| Level of the tag | A tag is a string literal, materialized under the current pc             | A constructor chosen under a secret-dependent branch carries that pc                                                     |
| Serialization    | Tuple wire format                                                        | Tags travel as full strings                                                                                              |

`tests/rt/pos/synvar/transparency.trp` pins this: a constructor value and a hand-written tuple
carrying the same tag are equal in both directions, satisfy each other's patterns, agree under
`isTuple`, have the same level when built under the same pc, and survive a `save`/`restore` round
trip identically. Printing is the one difference, and is the subject of the next section.

## The one runtime deviation

Variants deviate from pure sugar in exactly one place: a boolean field `_isSynVariant` on the
runtime tuple.

| Where                             | What happens                                                                                                      |
|-----------------------------------|-------------------------------------------------------------------------------------------------------------------|
| `compiler/src/SynVarFolding.hs`   | Emits the flag as `true` for constructor tuples only                                                              |
| `rt/src/RawTuple.mts`             | Declares the field; `mkTuple` defaults it to `false`                                                              |
| `rt/src/serialize.mts`            | Writes it into the tuple's wire form: `{vals, isSynVariant}`                                                      |
| `rt/src/deserialize.mts`          | Reads it back verbatim, rejecting a flagged tuple whose shape is not (tag) or (tag, payload) as corrupt data      |
| `rt/src/RawTuple.mts` `stringRep` | The only consumer: renders a flagged 1-tuple as the tag's last `#`-segment, a flagged 2-tuple as `(name payload)` |

The flag does not participate in matching or in equality, and no other runtime operation reads it.
Its single consumer is string rendering, which is reachable in-language: `toString` returns the
rendering to the program (and `print x` is `printString (toString x)`), so two values that compare
equal can produce different strings:

```sml
datatype colour = red | green of int

let val v = green 3
    val t = ("2cfc0t99sisjbr2bhq2i7bng9iq5hcr30ohcsda9e1un4uv6oqb0#colour#green", 3)
in print (toString v);          (* "(green 3)"                        *)
   print (toString t);          (* "("2cfc0t9…#colour#green", 3)"     *)
   print (toString v = toString t);   (* false *)
   print (v = t)                      (* true  *)
end
```

Because the flag stays out of matching, a constructor pattern tests only the tag string and the
tuple's shape: a program that writes the tuple by hand builds a value the pattern accepts. A
constructor therefore identifies a shape, not an issuer; code that needs an unforgeable value
places a nonce (`mkuuid`) in the payload.

Because the flag is part of the wire form and of a `save` file's contents, it is supplied by
whoever produced those bytes. A tuple that arrives with the flag set renders as a constructor even
when its tag names no datatype the receiving program declares.

## Identity: normalization and hashing

A constructor's runtime tag is

```
<group-hash> "#" <datatype-name> "#" <constructor-name>
```

where `<group-hash>` identifies the whole declaration group by its content. Two nodes, programs, or
libraries agree on a constructor exactly when they agree on this string, and the hash is computed
from the declaration alone — no module name, path, or alias is an input, so the same declaration
written in two places yields the same tags.

The hash is computed in `compiler/src/SynVarHash.hs`:

1. The group is put in normal form: members sorted by datatype name, constructors sorted by
   constructor name, type variables replaced by their position in the datatype's parameter list,
   references to members of the same group recorded by name, and references to earlier groups
   recorded by *that group's hash* together with the member name.
2. The normal form is rendered as a canonical ASCII s-expression, single spaces throughout.
3. `SHA-256("troupe:synvar:1" ++ canonical-form)` is rendered as lowercase unpadded base32hex
   (52 characters).

The canonical grammar:

| Node                             | Meaning                                                             |
|----------------------------------|---------------------------------------------------------------------|
| `(group dt ...)`                 | The group; members sorted by name                                   |
| `(dt name nparams ctor ...)`     | One datatype; constructors sorted by name                           |
| `(ctor name)` / `(ctor name ty)` | A nullary / applied constructor                                     |
| `(var i)`                        | Type variable, by position in the parameter list                    |
| `(prim p)`                       | A primitive type name                                               |
| `(in name)`                      | A member of this same group                                         |
| `(ext hash name)`                | A member of a previously hashed group                               |
| `(prod ty ty ...)`               | An n-ary product                                                    |
| `(app ty ... target)`            | Application; the target is `(builtin list)`, `(in …)`, or `(ext …)` |

Because a dependency enters as its group's hash, each digest commits to the entire declaration
closure beneath it:

| Change to the source                               | Tags unchanged |
|----------------------------------------------------|----------------|
| Layout, comments, parenthesization                 | yes            |
| Reordering declarations in the file (where legal)  | yes            |
| Reordering constructors within a declaration       | yes            |
| Renaming a type variable                           | yes            |
| Any change in an unreferenced datatype             | yes            |
| Renaming a datatype                                | no             |
| Renaming, adding, or removing a constructor        | no             |
| Changing a payload type                            | no             |
| Any change in a (transitively) referenced datatype | no             |

### `--datatype-hashes`

`troupec --datatype-hashes FILE` prints one line per datatype group declared in the file — the
group hash, two spaces, the canonical form — and stops before code generation. Only the file's own
declarations are reported; imported groups are not. For the `binop`/`expr` example above:

```
$ bin/troupec --datatype-hashes probe.trp
lbf8abh66bb9uir0juna1l3pvvidvdsnhj3151va3ddhh470tia0  (group (dt binop 0 (ctor ADD) (ctor SUB)))
52lkqeu14qslt1b0fak7l0v3bheji43a44qoci30j035eq7v37b0  (group (dt expr 0 (ctor BIN (prod (ext lbf8abh66bb9uir0juna1l3pvvidvdsnhj3151va3ddhh470tia0 binop) (in expr) (in expr))) (ctor LIT (prim int))))
```

Adding `MUL` to `binop` changes `binop`'s hash, and therefore `expr`'s, and therefore the tags of
`LIT` and `BIN` as well. Two parties comparing these lines can see both whether their declarations
agree and, from the canonical form, where they differ.

## Errors

Static errors, with the compiler's text:

| Situation                                         | Message                                                                                                                                                 |
|---------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------|
| Applied pattern names no constructor              | `unbound constructor: mauve`                                                                                                                            |
| Bare constructor name declared by two datatypes   | `constructor SHARED is ambiguous: declared in datatypes b, a; qualify it, e.g. b.SHARED`                                                                |
| Qualified name the datatype does not declare      | `datatype VariantsDemo.color has no constructor MAUVE`                                                                                                  |
| Argument given to a nullary constructor           | `constructor RED takes no argument`                                                                                                                     |
| Argument omitted from a non-nullary constructor   | `constructor SOME expects an argument`                                                                                                                  |
| Duplicate constructor in one datatype             | `duplicate constructor RED in datatype color (first declared at FILE:2:18)`                                                                             |
| Duplicate datatype name in one `and` group        | `duplicate datatype t in declaration group (first declared at FILE:5:10)`                                                                               |
| `and` group that is not mutually recursive        | `the 'and' group {a, b, c} is not mutually recursive: a, b are not mutually recursive with the rest; declare them in a separate group before this one.` |
| Reference to a datatype declared in a later group | `unbound type name: b`                                                                                                                                  |
| Type variable not in the parameter list           | `type variable 'b is not in the parameter list of datatype box`                                                                                         |
| Wrong number of type arguments                    | `datatype box expects 1 type argument, got 2`                                                                                                           |
| Parameterized datatype referenced unapplied       | `datatype box expects 1 type argument`                                                                                                                  |
| Constructor name used as a function name          | `RED is a constructor of datatype color and cannot be used as a function name`                                                                          |
| Dotted head is both a value and a datatype        | `t.C is ambiguous: t is both a value here and a datatype with constructor C; rename the value or qualify the constructor`                               |

A `case` that matches no arm is the ordinary pattern-match failure — constructor patterns are tuple
patterns, so nothing about the diagnostic is variant-specific:

```
  4 | in print (case x of red => 1 | green => 2)
    |           ^

>> pattern match failure in case expression
>> at FILE:4:11
```

## Printing

A nullary variant prints as its constructor name; an applied variant prints as `(name payload)`,
with the payload printed by the ordinary rules. Only the tag's segment after the last `#` is shown,
so two datatypes sharing a constructor name print identically:

```sml
datatype celsius    = DEG of int
datatype fahrenheit = DEG of int
(* celsius.DEG 100 prints as (DEG 100); fahrenheit.DEG 212 prints as (DEG 212) *)
```

Nested values print recursively — `bin (add, lit 8, lit 4)` prints as
`(bin (add, (lit 8), (lit 4)))`, the inner parentheses being the payload 3-tuple. A plain tuple
that carries a tag string but not the flag prints as the tuple it is, with the whole tag visible.

Rendering consults only the flag and the tag's shape; there is no registry of locally declared
group hashes, so a flagged tuple whose hash names no datatype in the running program still renders
constructor-style.

## Across libraries and modules

A library or module exports every datatype group declared in its header. The `.exports` interface
gains one line per group, `datatype <hash> <canonical-form>`; the importer parses the canonical
form, recomputes the hash, and rejects the interface if the stored hash disagrees. Constructor uses
and patterns in the importer desugar to the exporter's exact tag strings, so nothing is linked at
runtime — matching is string equality on identical literals. See [MODULES.md](MODULES.md).

One runtime check exists. A program records the group hashes it consumed per library
(`__consumedDatatypeHashes`), a library records the hashes it exports (`__datatypeHashes`), and at
load time every consumed hash must still be among the library's exported ones. Loading an importer
whose consumed group the library no longer exports fails with:

```
Error: datatype version skew: library 'VariantsDemo' no longer exports a datatype group that was
consumed at compile time (group hash 0000000…). The importer was compiled against a different
version of 'VariantsDemo'; recompile it against the current library.
```

The check is membership rather than equality, so extending a library — new declaration groups, new
or changed functions, or a new datatype that references an existing one — leaves the previously
exported hashes in the list and the check still passes.

## Worked example

```sml
datatype binop = add | sub
datatype expr  = lit of int | bin of binop * expr * expr

datatype 'a msg = evaluate of expr * 'a | answer of int | stop

let fun apply (add, a, b) = a + b
      | apply (sub, a, b) = a - b

    fun eval (lit n)          = n
      | eval (bin (op, l, r)) = apply (op, eval l, eval r)

    fun server () =
        receive [ hn (evaluate (e, client)) => (send (client, answer (eval e)); server ())
                , hn stop                   => () ]

    val e = bin (add, lit 8, bin (sub, lit 4, lit 1))
    val s = spawn server
in print e;
   print (eval e);
   send (s, evaluate (e, self ()));
   receive [ hn (answer n) => print (answer n) ];
   send (s, stop)
end
```

Output:

```
(bin (add, (lit 8), (bin (sub, (lit 4), (lit 1)))))
11
(answer 11)
```

Its three groups and their hashes:

```
dvp5c8b21undmulg16ejl4bir2i7blt0kgc7i1mcf9ls6r30ni00  (group (dt binop 0 (ctor add) (ctor sub)))
9cmokf58q4mfqjn45qs100265pj75cnocjm81s39sl2fnjgtsas0  (group (dt expr 0 (ctor bin (prod (ext dvp5c8b21undmulg16ejl4bir2i7blt0kgc7i1mcf9ls6r30ni00 binop) (in expr) (in expr))) (ctor lit (prim int))))
fs46sm7tt2qhq7jpfts88s8b7oh8m8pi6pvomk3v8c4jsfrt39ug  (group (dt msg 1 (ctor answer (prim int)) (ctor evaluate (prod (ext 9cmokf58q4mfqjn45qs100265pj75cnocjm81s39sl2fnjgtsas0 expr) (var 0))) (ctor stop)))
```

There is no primitive type name for a process id, so `msg` takes a type parameter standing for the
reply address carried in the payload. The same shape is used throughout
`examples/savina/benchmarks/`, where each benchmark's message protocol is one `datatype 'a msg`
group matched by constructor patterns in `receive` — for example `Pingpong.trp` (excerpt):

```sml
datatype 'a msg = Ping of 'a | Pong | Stop

let fun ponger () =
        receive [ hn (Ping p) => (send (p, Pong); ponger ())
                , hn Stop     => () ]
in ... end
```

## Where to look

| Path                             | What it holds                                                         |
|----------------------------------|-----------------------------------------------------------------------|
| `examples/variants/`             | Standalone programs: evaluators, an interpreter, shapes, actors, JSON |
| `examples/savina/benchmarks/`    | Message protocols as variants across the Savina benchmark suite       |
| `tests/rt/pos/synvar/`           | Behaviour tests, including `transparency.trp`                         |
| `tests/cmp/synvar/`              | Static-error tests, one per diagnostic                                |
| `compiler/src/SynVarFolding.hs`  | Resolution, static checks, and the rewrite to tuples                  |
| `compiler/src/SynVarHash.hs`     | Normal form, canonical encoding, hashing, tag construction            |
| `compiler/test/synvarhash-test/` | Exact hash and canonical-form vectors                                 |
| `rt/src/RawTuple.mts`            | The flag and the rendering that consumes it                           |
