# Troupe Standard Library

Unlike the ones implemented in the runtime, `rt/`, this standard library is implemented in Troupe.
This moves them out of the trusted computing base (TCB), i.e. the parts of Troupe that need to be
reviewed rigorously rather than depend on the monitor.

## Modules

- `Hash`          : Hash functions for values of all types.
- `HashMap`       : Map from keys to values via their hash.
- `HashSet`       : Set of elements via their hash.
- `List`          : Operations for lists, i.e. `[]` and `x::xs`.
- `ListPair`      : Operations for list of pairs, i.e. `(x,y)::xs`.
- `Number`        : Operations for numbers, i.e. integer and floats.
- `StencilVector` : Memory-efficient implementation of small (sparse) arrays.
- `String`        : Operations for strings
- `Unit`          : Unit testing.

## How to add a new file

To compile a module as part of the standard library, add it to the list of files in the `lib`
target of the *makefile*.

## TODO

- To conform with the Standard ML Basis Library, we should have the files conform to a `CamelCase`
  style.
- To fake namespaced import, e.g. `List.length`, the library should export a struct instead. Only
  certain functions should "pollute" the global namespace.
- Quite a lot of the standard library is not documented in any way. What is the purpose of each
  function and each module? The [modules](#modules) above are the ones that have been updated and
  documented.
- There are a lot of things in here - some of it dead. Can we merge/remove some things?
