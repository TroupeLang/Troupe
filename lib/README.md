# Troupe Standard Library

Unlike the ones implemented in the runtime, `rt/`, this standard library is implemented in Troupe.
This moves them out of the trusted computing base (TCB), i.e. the parts of Troupe that need to be
reviewed rigorously rather than depend on the monitor.

## Modules

- `DeclassifyUtil` : Helper functions for declassification.
- `Hash`           : Hash functions for values of all types.
- `HashMap`        : Map from keys to values via their hash.
- `HashSet`        : Set of elements via their hash.
- `List`           : Operations for lists, i.e. `[]` and `x::xs`.
- `ListPair`       : Operations for list of pairs, i.e. `(x,y)::xs`.
- `Number`         : Operations for numbers, i.e. integer and floats.
- `StencilVector`  : Memory-efficient implementation of small (sparse) arrays.
- `String`         : Operations for strings
- `ThreadUtil`     : Additional functions for thread management.
- `Unit`           : Unit testing.

## How to add a new file

To compile a module as part of the standard library, add it to the list of files in the `lib`
target of the *makefile*.

## Design Principles

- File names are written in `CamelCase`. This makes them conform to the Standard ML Basis Library.
- It is more important to match the function names and signatures in the Standard ML library than to
  improve on them. For example, `String.sub` would make more sense with the type `[Char] -> Int ->
  Char` but to match the SML library, we will stick with `[Char] * Int -> Char`.
- Each module exports a single *record* with the same name as the file. This (1) makes it closer to
  the SML module system and (2) allows for name resolution, e.g. `HashMap.findOpt` and
  `ListPair.findOpt` can be used in the same file.
- Each function that is exported has to be documented (`(** <text> *)`). In the long run, we will
  auto-generate documentation for the Standard Library.

### Other notes

- The `ThreadUtil` module was initially named `Thread`. But, this suggests incorrectly, that
  threading is implemented here rather than being a language primitive.

## TODO

The [modules](#modules) mentioned above already follow the [design principles](#design-principles).
The remaining files either need to be updated or to be removed.
