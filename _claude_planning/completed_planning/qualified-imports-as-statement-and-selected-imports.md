We want to add two more quality of life features to qualified imports 

1. `as` statement to allow redefining the name under which the library 
   is imported.
 
   This should support both 

   `import qualified List as L`
   
   and 

   `import List as L` 

   so that in the user code one can then write `L.map` to refer to the map function. 

2. limited import, so that we have 


   `import List (map)` 

   which will import the map function from List and nothing else.

   This can be combined with as and qualified imports.

Let's create a detailed plan in this file that will specify the steps to implement 
these features. 


------

# Implementation Plan

## Design Decisions

1. **Alias scope**: For `import List as L`, both direct access (`head`) AND alias access (`L.head`) work
2. **Syntax order**: Alias comes before selection: `import List as L (map)`
3. **Selective + qualified access**: For `import List (map)`, both `map` and `List.map` work

## Syntax Examples (After Implementation)

**Note:** Uses `only` keyword before selective imports to avoid parser conflicts.

```troupe
import List                              (* Unqualified: head, tail available directly *)
import qualified List                    (* Qualified: List.head, List.tail *)
import List as L                         (* Both direct 'head' AND 'L.head' work *)
import qualified List as L               (* Only L.head, L.tail *)
import List only (head, tail)            (* Both 'head' and 'List.head' work, only head/tail *)
import qualified List only (head) as L   (* Only L.head *)
```

## Current Implementation Overview

**Key Files:**
- `compiler/src/Lexer.x` - Tokenizer
- `compiler/src/Parser.y` - Grammar (lines 140-144)
- `compiler/src/Basics.hs` - `Imports`, `ImportMode` types (lines 102-119)
- `compiler/src/Core.hs` - Name resolution in `mapFromImports` (lines 264-283) and `rename` (lines 369-389)
- `compiler/src/ProcessImports.hs` - Loads `.exports` files

**Current Import Tuple:** `(LibName, Maybe [VarName], ImportMode)`
- `LibName`: The library name
- `Maybe [VarName]`: Export list (filled from .exports file)
- `ImportMode`: Qualified | Unqualified

## Implementation Steps

### Step 1: Add `as` Token to Lexer

**File:** `compiler/src/Lexer.x`

Add `as` as a reserved keyword token (around line 105 with other keywords).

### Step 2: Extend Import Data Types

**File:** `compiler/src/Basics.hs`

Change the import representation to include optional alias and selective imports:

```haskell
-- Current:
-- data Imports = Imports [(LibName, Maybe [VarName], ImportMode)]

-- New:
data ImportDecl = ImportDecl
  { importLib      :: LibName          -- Original library name
  , importAlias    :: Maybe LibName    -- Optional alias (from "as X")
  , importExports  :: Maybe [VarName]  -- Exports from .exports file (filled by ProcessImports)
  , importSelected :: Maybe [VarName]  -- Selective imports (user-specified)
  , importMode     :: ImportMode       -- Qualified | Unqualified
  } deriving (Eq, Show, Ord, Generic)

data Imports = Imports [ImportDecl]
```

### Step 3: Update Parser Grammar

**File:** `compiler/src/Parser.y`

Extend `ImportDecl` production to handle new syntax:

```haskell
ImportDecl
    : import VAR OptAlias OptSelection ImportDecl
        { (mkImportDecl (varTok $2) $3 $4 Unqualified) : $5 }
    | import qualified VAR OptAlias OptSelection ImportDecl
        { (mkImportDecl (varTok $3) $4 $5 Qualified) : $6 }
    | { [] }

OptAlias
    : as VAR  { Just (LibName (varTok $2)) }
    | { Nothing }

OptSelection
    : '(' VarList ')'  { Just $2 }
    | { Nothing }

VarList
    : VAR              { [varTok $1] }
    | VAR ',' VarList  { (varTok $1) : $3 }
```

### Step 4: Update ProcessImports

**File:** `compiler/src/ProcessImports.hs`

Preserve the alias and selection while loading exports:

```haskell
processImport :: ImportDecl -> IO ImportDecl
processImport imp = do
  troupeEnv <- getTroupeHome
  let LibName lib = importLib imp
  let fname = troupeEnv ++ defaultLibFolder ++ lib ++ ".exports"
  input <- readFile fname
  return imp { importExports = Just (lines input) }
```

### Step 5: Add Validation for Selective Imports

**File:** `compiler/src/ProcessImports.hs` or new validation module

After loading exports, validate that all selected names exist in the library:

```haskell
validateSelection :: ImportDecl -> Either String ImportDecl
validateSelection imp =
  case (importSelected imp, importExports imp) of
    (Just selected, Just exports) ->
      let missing = filter (`notElem` exports) selected
      in if null missing
         then Right imp
         else Left $ "Library '" ++ libName ++ "' does not export: " ++ show missing
    _ -> Right imp
```

### Step 6: Update Environment Building in Core.hs

**File:** `compiler/src/Core.hs`

Modify `mapFromImports` to use aliases and respect selective imports:

```haskell
mapFromImports :: Imports -> LibEnv
mapFromImports (Imports imports) =
  let
    -- Get effective name (alias or original)
    effectiveName imp = fromMaybe (importLib imp) (importAlias imp)

    -- Get effective exports (selected or all)
    effectiveExports imp = fromMaybe (fromJust $ importExports imp) (importSelected imp)

    -- Build unqualified environment (function name -> original lib)
    unqualifiedImports = [imp | imp <- imports, importMode imp == Unqualified]
    unqualEnv = foldl (insUnqual) Map.empty unqualifiedImports

    -- Build qualified environment (alias -> original lib, exports)
    -- Key is the alias/effective name, value includes original lib for runtime
    libExports = Map.fromList
      [ (effectiveName imp, (importLib imp, Set.fromList (effectiveExports imp)))
      | imp <- imports
      ]
  in
    (unqualEnv, libExports)
```

### Step 7: Update Qualified Name Resolution

**File:** `compiler/src/Core.hs`

Modify `tryQualifiedAccess` to use the new environment structure:

```haskell
tryQualifiedAccess = case t of
  Var (RegVar v) | not (Map.member v m) -> do
    (_, libExports) <- ask
    case Map.lookup (LibName v) libExports of
      Just (originalLib, exports) ->
        if Set.member f exports
        then return $ Just (Var (LibVar originalLib f))  -- Use original lib for codegen
        else lift $ throwError $
          "'" ++ v ++ "' does not export '" ++ f ++ "'"
      Nothing -> return Nothing
  _ -> return Nothing
```

### Step 8: Update All Pattern Matches

Search for all uses of `Imports` and `ImportDecl` types and update pattern matches to handle the new structure.

### Step 9: Create Tests

**Tests to create in `tests/rt/pos/core/`:**
- `qualified-import-as.trp` - Test `import qualified List as L`
- `qualified-import-selective.trp` - Test `import List (head, tail)`
- `qualified-import-combined.trp` - Test `import qualified List as L (head)`
- `qualified-import-as-unqualified.trp` - Test `import List as L`

**Negative tests in `tests/rt/neg/core/`:**
- `import-selective-nonexistent.trp` - Error when selecting non-existent export

### Step 10: Update Documentation

Update language documentation to reflect new import syntax.

## Risk Assessment

- **Low risk**: Lexer/Parser changes are isolated
- **Medium risk**: Changes to `Imports` type require updating all pattern matches
- **Testing**: Must verify backward compatibility with existing imports




---

## IMPLEMENTED: JavaScript-style Selective Imports

The selective import syntax has been refactored to use curly braces immediately after `import`:

### New Syntax

```troupe
import { head, tail } List              (* Selective unqualified *)
import qualified { head } List          (* Selective qualified *)
import { head } List as L               (* Selective with alias *)
import qualified { head } List as L     (* Selective qualified with alias *)

(* Existing syntax unchanged *)
import List                             (* Full unqualified *)
import qualified List                   (* Full qualified *)
import List as L                        (* Full unqualified with alias *)
import qualified List as L              (* Full qualified with alias *)
```

### Grammar Changes

**Parser.y** - No shift/reduce conflicts with this grammar:

```haskell
ImportDecl: import '{' VarList '}' VAR OptAlias ImportDecl
          | import qualified '{' VarList '}' VAR OptAlias ImportDecl
          | import VAR OptAlias ImportDecl
          | import qualified VAR OptAlias ImportDecl
          | { [] }

OptAlias : as VAR   { Just (LibName (varTok $2)) }
         | { Nothing }

VarList : VAR              { [varTok $1] }
        | VAR ',' VarList  { (varTok $1) : $3 }
```

### Why No Conflicts

The `{` immediately after `import` is unambiguous because:
- After `import`, the parser can only see `{`, `qualified`, or `VAR`
- `{` cannot start an expression at this position (expressions come after `ImportDecl` and `AtomsDecl`)
- No need for a disambiguating keyword like `only`

### Tests Updated

- `tests/rt/pos/core/qualified-import-selective.trp` - Uses `import { head, tail } List`
- `tests/rt/pos/core/qualified-import-combined.trp` - Uses `import qualified { head } List as L`
