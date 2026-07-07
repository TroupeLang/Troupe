# Troupe Standard Library

Unlike the ones implemented in the runtime, `rt/`, this standard library is implemented in Troupe.
This moves them out of the trusted computing base (TCB), i.e. the parts of Troupe that need to be
reviewed rigorously rather than depend on the monitor.

## Modules

- `Duration`      : Time spans, represented in milliseconds.
- `Hash`          : Hash functions for values of all types.
- `HashMap`       : Map from keys to values via their hash.
- `HashSet`       : Set of elements via their hash.
- `Html`          : Combinator-based HTML generation with escaping.
- `IfcUtil`       : Lattice-level constants and helper functions for downgrading
                    (declassification and endorsement).
- `List`          : Operations for lists, i.e. `[]` and `x::xs`.
- `ListPair`      : Operations for list of pairs, i.e. `(x,y)::xs`.
- `Map`           : Map from keys to values via a comparator function.
- `Number`        : Operations for numbers, i.e. integer and floats.
- `Result`        : Success-or-failure values, encoded as tagged records.
- `Set`           : Set of elements via a comparator function.
- `StencilVector` : Memory-efficient implementation of small (sparse) arrays.
- `String`        : Operations for strings
- `ThreadUtil`    : Additional functions for thread management.
- `Time`          : Date and time manipulation.
- `timeout`       : Timers that send a message or exit the program after a duration.
- `Unit`          : Unit testing.

## How to add a new file

To compile a module as part of the standard library, add it to the list of files in the `lib`
target of the *makefile*.

## Design Principles

- File names are written in `CamelCase`. This makes them conform to the Standard ML Basis Library.
- We will try to match function names in the Standard ML library.
- Each module exports a single *record* with the same name as the file. This (1) makes it closer to
  the SML module system and (2) allows for name resolution, e.g. `HashMap.findOpt` and
  `ListPair.findOpt` can be used in the same file.
- Each function that is exported has to be documented (`(** <text> *)`). In the long run, we will
  auto-generate documentation for the Standard Library.
- Avoid changing signatures and names of existing functions. It breaks backwards compatibility
  with existing code, i.e., in the user guide, assignments, etc. New library functions are okay to
  introduce, including those that happen to duplicate functionality. We should then go through the
  discussion of deprecation of existing libraries.

### Other notes

- The `ThreadUtil` module was initially named `Thread`. But, this suggests incorrectly, that
  threading is implemented here rather than being a language primitive.
