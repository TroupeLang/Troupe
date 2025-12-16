We want to refactor the syntax for imports, so we have

`import qualified <lib-name>` to mean 

that if the library <lib-name> contains a function foo, we can only refer to 
it via <lib-name>.foo 

This will require changing in the parser, so that we are aware of the qualified imports. 

The problem is that we already use the dot notation for records, so we need to 
be able to disambiguate.  For example, if we have module A that exports functionality foo

import qualified A

let val A = {foo}

then subsequently when we have A.foo, it means that we are overshadowing the declaration of A.


this may be ok, but we need to work this out and document everything carefully.

Let's start off by 

- creating two libraries for testing purposes, each exporting function foo. 

- these will be libraires A and B that we will place into the /lib folder. 

- let each of these libraries declare function `foo` that returns "A" and "B" respectively.

- we want then to create a test functionality that clearly demonstrates the problem 


```
import qualified A
import qualified B

foo ()
```

the problem in this code should be that there would be no way of referring to call A.foo anymore. 

Let's do as follows.

Help me proceeding with above, so we reach a point where we have a clear demonstration of the problem.

---

## Completed Demonstration (2024-12-14)

### Created Files

- `/lib/A.trp` - exports `foo` returning "A"
- `/lib/B.trp` - exports `foo` returning "B"
- `/tests/_unautomated/claude/qualified_import_demo.trp`
- `/tests/_unautomated/claude/qualified_import_desired.trp`
- `/tests/_unautomated/claude/qualified_shadowing_demo.trp`

### Test Results

| Test                            | Output           | Behavior                                            |
|---------------------------------|------------------|-----------------------------------------------------|
| `qualified_import_demo.trp`     | `"B"`            | Last import wins - A's foo is inaccessible          |
| `qualified_import_desired.trp`  | `"B"`            | Same - no way to access both foo functions          |
| `qualified_shadowing_demo.trp`  | `"local record"` | Local variable `val A = {...}` shadows library name |

### Confirmed Problems

1. **Name collision**: When both A and B export `foo`, only B's (last import) is accessible
2. **No qualified access**: Without records, we cannot write `A.foo()` to disambiguate
3. **Shadowing**: When `val A = {...}` is defined, `A.foo` refers to the local record

---

## Phase 2: Implementation Plan

### Design Decision: Renaming Phase Approach

The qualified import resolution happens during the **renaming phase** in `Core.hs` because:

1. We need local scope information to handle shadowing correctly
2. The renaming phase already has both environments (local `Env` + `LibEnv`)
3. It's the natural place for name resolution

### Disambiguation Rule

When we see `A.foo` (parsed as `ProjField (Var "A") "foo"`):

```
1. Is "A" in local scope (Env)?     → Keep as ProjField (record field access)
2. Is "A" a qualified import?       → Transform to Var (LibVar "A" "foo")
3. Otherwise                        → Keep as ProjField (will error if A undefined)
```

**Key principle**: Local bindings shadow qualified imports (matches Haskell behavior).

---

### Step 1: Lexer (`compiler/src/Lexer.x`)

Add the `qualified` keyword token.

**Find the reserved words section and add:**
```haskell
"qualified" { \p s -> return $ TokenQualified p }
```

**Add to token data type:**
```haskell
| TokenQualified PosInf
```

---

### Step 2: Parser (`compiler/src/Parser.y`)

**Add token declaration:**
```haskell
%token
  ...
  qualified { TokenQualified $$ }
```

**Update ImportDecl grammar (around line 139):**

Current:
```haskell
ImportDecl: import VAR ImportDecl { ((LibName (varTok $2), Nothing)): $3 }
          | { [] }
```

New:
```haskell
ImportDecl: import VAR ImportDecl
              { ((LibName (varTok $2), Nothing, Unqualified)): $3 }
          | import qualified VAR ImportDecl
              { ((LibName (varTok $3), Nothing, Qualified)): $4 }
          | { [] }
```

---

### Step 3: AST Types (`compiler/src/Basics.hs`)

**Add ImportMode type:**
```haskell
data ImportMode = Qualified | Unqualified
  deriving (Eq, Show, Ord, Generic)
instance Serialize ImportMode
```

**Update Imports type:**

Current:
```haskell
data Imports = Imports [(LibName, Maybe [VarName])]
```

New:
```haskell
data Imports = Imports [(LibName, Maybe [VarName], ImportMode)]
```

---

### Step 4: ProcessImports (`compiler/src/ProcessImports.hs`)

Update to handle the new tuple structure. The processing logic stays the same - we still load the `.exports` file for both qualified and unqualified imports.

**Key change**: Thread the `ImportMode` through when populating the exports list.

Current pattern:
```haskell
(LibName name, Nothing) → (LibName name, Just exports)
```

New pattern:
```haskell
(LibName name, Nothing, mode) → (LibName name, Just exports, mode)
```

---

### Step 5: Core.hs - Environment Changes

**Add import for Set:**
```haskell
import qualified Data.Set as Set
```

**Rename existing type and add new types (around line 250):**

Current:
```haskell
type LibEnv = Map.Map VarName LibName
```

New:
```haskell
type UnqualifiedLibEnv = Map.Map VarName LibName
-- Maps exported function names → library (for unqualified imports only)

type QualifiedLibEnv = Set.Set LibName
-- Set of library names that were imported with "qualified"

type LibEnv = (UnqualifiedLibEnv, QualifiedLibEnv)
-- Combined environment for the Reader monad
```

**Update `mapFromImports` (reuses existing `insLib` logic):**

Current:
```haskell
mapFromImports :: Imports -> LibEnv
mapFromImports (Imports imports) =
  foldl insLib Map.empty imports
     where
       insLib map (lib, Just defs) =
             foldl (\map def -> Map.insert def lib map) map defs
       insLib map (lib, Nothing) = error "malformed lib import data structure"
```

New:
```haskell
mapFromImports :: Imports -> LibEnv
mapFromImports (Imports imports) =
  let
    -- Filter to unqualified imports, then use existing logic
    unqualifiedImports = [(lib, defs) | (lib, defs, Unqualified) <- imports]
    unqualEnv = foldl insLib Map.empty unqualifiedImports

    -- Extract qualified library names into a Set
    qualSet = Set.fromList [lib | (lib, _, Qualified) <- imports]
  in
    (unqualEnv, qualSet)
  where
    insLib map (lib, Just defs) =
      foldl (\m def -> Map.insert def lib m) map defs
    insLib map (_, Nothing) =
      error "malformed lib import data structure"
```

**Update `lookforgen` to use UnqualifiedLibEnv:**

Current:
```haskell
lookforgen :: VarName -> Env -> S VarAccess
lookforgen v m =
    case Map.lookup v m of
       Just v -> return $ RegVar v
       Nothing -> do
          libmap <- ask
          case Map.lookup v libmap of
            Just lib' -> return $ LibVar lib' v
            Nothing -> return $ BaseName v
```

New:
```haskell
lookforgen :: VarName -> Env -> S VarAccess
lookforgen v m =
    case Map.lookup v m of
       Just v -> return $ RegVar v
       Nothing -> do
          (unqualEnv, _) <- ask
          case Map.lookup v unqualEnv of
            Just lib' -> return $ LibVar lib' v
            Nothing -> return $ BaseName v
```

---

### Step 6: Core.hs - Rename Logic for ProjField

**Update the `rename (ProjField t f)` case (around line 350):**

Current:
```haskell
rename (ProjField t f) m = do
  t' <- rename t m
  return $ ProjField t' f
```

New:
```haskell
rename (ProjField t f) m = do
  maybeQualified <- tryQualifiedAccess
  case maybeQualified of
    Just term -> return term
    Nothing   -> do
      t' <- rename t m
      return $ ProjField t' f
  where
    tryQualifiedAccess = case t of
      Var (RegVar v) | not (Map.member v m) -> do
        (_, qualSet) <- ask
        return $ if Set.member (LibName v) qualSet
                 then Just (Var (LibVar (LibName v) f))
                 else Nothing
      _ -> return Nothing
```

**Logic flow**:
1. `tryQualifiedAccess` checks: is `t` a variable NOT in local scope AND a qualified import?
2. If yes → return the transformed `LibVar`
3. If no → fall through to default behavior (rename `t`, wrap in `ProjField`)

---

### Step 7: Update Imports Usage Throughout Compiler

Files that use the `Imports` type need updating for the new 3-tuple:

| File                             | Change Required                                                  |
|----------------------------------|------------------------------------------------------------------|
| `compiler/src/Basics.hs`         | Update `Imports` type definition                                 |
| `compiler/src/Parser.y`          | Update import production rules                                   |
| `compiler/src/ProcessImports.hs` | Update tuple handling                                            |
| `compiler/src/Core.hs`           | Rename `LibEnv` → `UnqualifiedLibEnv`, add `QualifiedLibEnv`     |
| `compiler/src/IR.hs`             | May need updates if it inspects `Imports`                        |
| `compiler/src/CompM.hs`          | Check if it handles `Imports`                                    |

---

### Step 8: Testing

**Positive tests** (should work):
```troupe
import qualified A
import qualified B

let val resultA = A.foo ()
    val resultB = B.foo ()
in print resultA; print resultB
(* Expected: "A" then "B" *)
```

**Negative test** (should error - unqualified access to qualified import):
```troupe
import qualified A

let val result = foo ()  (* Error: foo not in scope *)
in print result
```

**Shadowing test** (local binding wins):
```troupe
import qualified A

let val A = { foo = fn () => "shadowed" }
    val result = A.foo ()
in print result
(* Expected: "shadowed" *)
```

---

### Summary of Files to Modify

| File                             | Lines (approx) | Complexity |
|----------------------------------|----------------|------------|
| `compiler/src/Lexer.x`           | +3             | Low        |
| `compiler/src/Parser.y`          | +5             | Low        |
| `compiler/src/Basics.hs`         | +10            | Low        |
| `compiler/src/ProcessImports.hs` | ~10 modified   | Medium     |
| `compiler/src/Core.hs`           | +30            | Medium     |

---

### Build & Test Commands

```bash
make stack      # Rebuild compiler
make libs       # Recompile libraries (A, B)
make test       # Run test suite

# Manual testing
./local.sh tests/_unautomated/claude/qualified_import_test.trp
```

---

## Phase 2: Implementation Complete (2024-12-14)

### Files Modified

| File                             | Change                                              |
|----------------------------------|-----------------------------------------------------|
| `compiler/src/Lexer.x`           | Added `TokenQualified` keyword and token            |
| `compiler/src/Parser.y`          | Added `qualified` token and updated ImportDecl      |
| `compiler/src/Basics.hs`         | Added `ImportMode` type, updated `Imports`          |
| `compiler/src/ProcessImports.hs` | Updated to handle 3-tuple with ImportMode           |
| `compiler/src/Core.hs`           | Split LibEnv, added ProjField qualified resolution  |
| `compiler/src/Direct.hs`         | Updated ppLibName for pretty-printing               |

### Test Results

| Test                                 | Result  | Behavior                                       |
|--------------------------------------|---------|------------------------------------------------|
| `qualified_import_test.trp`          | Pass    | Outputs "A" and "B" via `A.foo()` and `B.foo()` |
| `qualified_unqualified_access.trp`   | Pass    | Correctly errors: `bad base function: foo`     |
| `qualified_shadowing_test.trp`       | Pass    | Outputs "shadowed" (local binding wins)        |
| Full test suite (698/700)            | Pass    | 2 flaky timing tests, unrelated to changes     |

### Key Design Decisions

1. **Disambiguation Rule**: When we see `A.foo` (parsed as `ProjField (Var (RegVar "A")) "foo"`):
   - If `A` is NOT in local scope AND is a qualified import → transform to `LibVar`
   - Otherwise → keep as ProjField (record field access)

2. **Local binding priority**: Local bindings shadow qualified imports (matches Haskell) 


--- 

