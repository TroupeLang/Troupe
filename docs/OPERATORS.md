# User-defined operators

Troupe supports user-defined infix operators with declared fixity. An operator
is an ordinary curried function with a symbolic name; a fixity declaration
gives it a precedence level and associativity, which imports carry
automatically.

```
infixl 6 <+>
infixl 5 $$

let fun ( <+> ) a b = ...
    fun ( $$ ) a b = ...
in [ ("<+>", ( <+> )), ("$$", ( $$ )) ] end
```

```
import Pretty
let val doc = header <+> title $$ body
in ... end
```

## Operator names

An operator name is a maximal run of the characters

```
! $ % & * + - . / : < = > ? @ ^ | ~
```

beginning with one of `$ % & * + - / < = > @ ^ |`. The excluded starters:
`! ~ ?` are reserved for future prefix operators; `:` and `.` protect `::`,
field projection, and `..`. All four are legal continuation characters, so
`<.>`, `<:>`, and `|>.` are operator names.

Runs that spell a reserved token keep their meaning: `= => <> <= >= < > + - *
/ ^ :: . .. @ | & ; << >> ~>>` are the built-in spellings, and a longer run is
a user operator (`<>` is not-equal; `<>>` is available). Only `$` and `%` are
available as single-character operators. Built-in operators cannot be
redefined.

One collision with comment syntax: `(*` starts a comment, so an operator that
begins or ends with `*` is written with spaces inside its parentheses,
`( * )`-style. The compiler's comment errors carry a reminder.

## Fixity declarations

```
infixl 6 <+> <.>     left-associative
infixr 5 ++          right-associative
infix  4 <=>         non-associative
```

`infix` means non-associative — Haskell's reading. (In Standard ML, bare
`infix` declares a left-associative operator; Troupe diverges here because a
non-associative form is needed and this naming for the triple is the widely
known one.)

Declarations form their own section of the file header, after imports and
before `datatype` declarations. Levels are integers 0–9. Declaring the same
operator twice in one file is an error. A declaration is required for infix
use and for export; the parenthesized prefix form `( <+> ) a b` needs none.

## Precedence levels

User levels interleave with the built-in operators; a user operator declared
at a pinned level has exactly the built-in's precedence:

| Level | Built-ins there                 |
|-------|---------------------------------|
| 0–3   | —                               |
| 4     | `= <> < > <= >=` (non-assoc)    |
| (4,5) | `andb orb xorb`, `<< >> ~>>` — built-in only |
| 5     | —                               |
| 6     | `+ -` (left)                    |
| 7     | `* / div mod` (left)            |
| 8     | `::` (right)                    |
| (8,9) | `raisedTo`; `isTuple isList isRecord not` — built-in only |
| 9     | `^` (left)                      |

`andalso` and `orelse` bind below level 0. The bands marked built-in only are
not addressable by declarations. Two facts about the built-in table worth
knowing (both long-standing Troupe behavior): `::` binds tighter than `+`
(`1 + 2 :: []` is `1 + (2 :: [])`), and `^` binds tighter than `not`
(`not b ^ s` is `not (b ^ s)`).

Haskell's common fixities port verbatim: comparisons at 4, additive 6,
multiplicative 7, the HughesPJ pretty-printing levels 6 and 5, `>>=` at 1.

Chains that a fixity cannot order are errors:

```
1 = 2 = 3        error: '=' is non-associative; use parentheses to chain it
a <+ b +> c      error, for infixl 6 <+ and infixr 6 +> : same precedence,
                 incompatible associativity
x |> f           error if |> has no fixity in scope
```

## Operators as values

The parenthesized form makes an operator usable wherever a name is:

```
val inc     = ( + ) 1                   built-in section: fn a => fn b => a + b,
                                        partially applied
val prefix  = ( <+> ) header            partial application of a user operator
fun ( <=> ) a b = compare a b           definition
val (( <.> ), other) = pair             binding in a pattern
val beside  = Pretty.( <.> )            qualified prefix access
import { ( <.> ), render } Pretty       selective import
```

Operators are curried, matching function application. Troupe's higher-order
library functions take pairs (`List.foldl f` calls `f (x, acc)`), so passing
an operator to a fold uses an adapter lambda:
`List.foldl (fn (d, acc) => acc <.> d) empty docs`.

## Operators and modules

An exported operator's fixity is written into the library's `.exports`
interface (`fixity l 6 <+>`) and arrives with an import:

- an unqualified import makes the operator usable infix;
- a selective import restricts which operators (and fixities) arrive;
- a `qualified` import provides prefix access only (`M.( <+> )`); infix use
  reports the missing fixity;
- re-export works: the exporting file's fixity environment includes its
  imports.

When one operator name arrives from several sources, a local declaration is
strongest (it accompanies a local redefinition), and among unqualified imports
the later one wins — the same resolution order as for the value itself.
Exporting a symbolic name without a fixity in scope is a compile error.

## Semantics and cost

`a <+> b` is exactly `( <+> ) a b`: a curried application of an ordinary
function, with the label and blocking behavior every function call has.
Built-in operators are unaffected and keep their primitive compilation. A
user-operator application costs what a two-argument curried call costs
(measured ≈190 ns over the built-in on the reference machine, module
boundaries adding nothing); hot inner loops of a library are typically named
recursive functions, with operators as the API surface.

## Stage dumps

With `-v`, `out.syntax` shows the parse-phase program: fixity header and flat
operator chains, before any grouping. `out.opreassoc` shows the program after
re-association, with operator applications in parenthesized prefix form
(`( $$ ) (( <+> ) 1 2) (( <+> ) 3 4)`); grouping is explicit there.
