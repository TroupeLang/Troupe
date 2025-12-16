COMPLETED

Go over the test corpus, and in all the places where
the import is qualified but the libraries that are imported
are obviously non-conflicting, or there is only one library being imported, remove the qualified import and also rewrite the program from `<lib>.foo` to just `foo`

---

## Summary of Changes

The following files were dequalified (changed from `import qualified` to `import` and removed library prefixes from function calls):

### tests/rt/pos/core/ (6 files)
- `p01.trp` - List only
- `manyprocs.trp` - List only
- `gettype.trp` - List only
- `libimport.trp` - List only
- `par12.trp` - List only
- `string_manipulations.trp` - String only

### tests/rt/pos/ifc/ (3 files)
- `decl2.trp` - declassifyutil (unqualified) + List
- `blockendorseto02.trp` - List only
- `mapsp.trp` - List only

### tests/lib/ (7 files)
- `Hash.trp` - Hash only
- `String.trp` - Unit + String (non-conflicting)
- `Number.trp` - Unit + Number (non-conflicting)
- `ListPair.trp` - Unit + ListPair (non-conflicting)
- `List.trp` - Unit + List (non-conflicting)
- `StencilVector.trp` - Unit + StencilVector (non-conflicting)
- `HashSet.trp` - Unit + Hash + HashSet (kept qualified for HashMap/HashSet module prefixes due to shadowed bindings)
- `HashMap.trp` - Unit + Hash + HashMap (kept qualified for HashMap/HashSet module prefixes due to shadowed bindings)

### Note on HashSet.trp and HashMap.trp
These files use a pattern where local bindings shadow the module function names:
```
val empty     = HashSet.empty Hash.hash
val singleton = HashSet.singleton Hash.hash
val fromList  = HashSet.fromList Hash.hash
```
These shadowed bindings must keep the qualified prefixes to avoid recursive/undefined references.