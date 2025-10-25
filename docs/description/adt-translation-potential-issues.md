# ADT Translation Error Message Quality Issues

*This is a machine-generated document analyzing PR #53's ADT implementation.*

## Overview

PR #53 introduces Algebraic Data Types (ADTs) to Troupe as syntactic sugar over tagged records. The implementation successfully provides ergonomic ADT syntax, though error messages could be more informative since the transformation to records occurs early in the compilation pipeline, resulting in loss of ADT-specific information that could enhance error reporting.

## The Current Limitation

When ADTs are transformed to tagged records during the `AtomFolding` phase (immediately after parsing), certain semantic information is not preserved:
- Which constructors belong to which datatypes
- Constructor names and their expected arities
- The complete set of constructors for exhaustiveness checking

The `ADTTag` boolean flag (which becomes `_isADT` in the runtime) is preserved, indicating that a record originated from ADT syntax. This flag is currently used only for display formatting rather than error reporting. Subsequent compilation phases and runtime process regular records, producing generic error messages that reveal implementation details rather than ADT-level abstractions.

## Concrete Examples

*Note: In the examples below, "Suggested Improvement" sections represent subjective proposals for how error messages could be more informative and user-friendly. These are not objective requirements but rather ideas for potential enhancements.*

### Example 1: Non-exhaustive Pattern Match

#### User Code (ADT Syntax)
```sml
datatype option = NONE | SOME of a

let val myOption = SOME 42
in case myOption of NONE => print "was none"
end
```

#### Internal Transformation
```sml
(* SOME 42 becomes: *)
{tag="SOME", value=42}  (* with _isADT=true *)

(* Pattern match becomes: *)
case {tag="SOME", value=42} of
     {tag="NONE"} => print "was none"
```

#### Actual Error Message
```
pattern match failed
```

#### Suggested Improvement
```
pattern match failed: SOME 42 did not match any case
```
or potentially even better:
```
Non-exhaustive pattern match in case expression:
  Missing constructor: SOME
  Value: SOME 42
```
*Note: These are subjective suggestions for how error messages could be more helpful, not requirements or expectations.*

### Example 2: Missing Pattern Cases

#### User Code
```sml
datatype result = OK of a | ERROR of string | PENDING

fun processResult (OK v) = v
  | processResult (ERROR msg) = raise Fail msg
(* Missing PENDING case *)

let val r = PENDING
in processResult r
end
```

#### Internal Transformation
The function `processResult` lacks a case for the `PENDING` constructor, which becomes a record containing only `{tag="PENDING"}`.

#### Actual Error Message
```
pattern match failure in function processResult
```

#### Suggested Improvement
```
pattern match failure in function processResult
  Unhandled constructor: PENDING
  Value: PENDING
```

### Example 3: Type Mismatch in Pattern

#### User Code
```sml
datatype result = OK of a | ERROR of string

let val r = ERROR "failed"
in case r of OK v => v + 1  (* ERROR constructor, but matching OK *)
end
```

#### Internal Transformation
```sml
case {tag="ERROR", value="failed"} of
     {tag="OK", value=v} => v + 1
```

#### Actual Error Message
```
pattern match failed
```

#### Suggested Improvement
```
Pattern match failed:
  Expected constructor: OK
  Actual constructor: ERROR with value "failed"
```

### Example 4: Alternative Pattern Structure

#### User Code
```sml
datatype expr = NUM of int | ADD of expr * expr

(* User might expect to destructure the tuple *)
let val e = ADD (NUM 1, NUM 2)
in case e of NUM n => n
           | ADD x => 0  (* Pattern binds entire tuple to x *)
end
```

#### Internal Transformation
The pattern `ADD x` transforms to `{tag="ADD", value=x}` where `x` matches the entire tuple `(NUM 1, NUM 2)` rather than decomposing it.

#### Actual Behavior
The code compiles and runs with `x` bound to the tuple, potentially causing type errors later if used incorrectly.

#### Suggested Improvement (with more ADT awareness)
```
Warning: Constructor ADD expects a tuple pattern
  Found: ADD x
  Suggested: ADD (e1, e2)
```


## Root Cause Analysis

### Transformation Pipeline

1. **Parser** (`Parser.y`)
   - Parses ADT syntax into AST with `DataTypeDecl`, `DataTypePattern` nodes
   - Maintains constructor information temporarily

2. **AtomFolding** (`AtomFolding.hs`) - **WHERE INFORMATION IS LOST**
   ```haskell
   -- Nullary constructor becomes:
   Record [("tag", Just (Lit (LString nm)))] True

   -- Non-nullary constructor becomes:
   Abs (Lambda [VarPattern var]
       (Record [("tag", Just (Lit (LString nm))),
                ("value", Just (Var var))] True))
   ```
   After this phase, ADTs become records with `ADTTag=True`. This flag persists through compilation and becomes `_isADT` in the runtime

3. **CaseElimination** (`CaseElimination.hs`)
   - Generates generic error messages:
   ```haskell
   Error (Lit (LString "pattern match failed"))
   Error (Lit (LString $ "pattern match failure in function " ++ f))
   ```
   - Lacks awareness that records originated from ADTs

4. **Runtime** (`Record.mts`)
   - Receives `_isADT` flag indicating ADT origin
   - Uses this flag only for display formatting in `stringRep()`
   - Error handling code does not leverage the `_isADT` flag for enhanced error messages

### Why Information Is Lost

The transformation occurs at the `AtomFolding` stage because it:
1. Simplifies implementation through syntax tree rewriting
2. Avoids threading ADT information through compilation phases
3. Reuses existing record pattern matching machinery

Consequently:
- **Constructor metadata** is not preserved beyond AtomFolding (except the boolean ADT flag)
- **Error messages cannot reference** constructor names or datatypes (despite runtime awareness via `_isADT`)
- **Compile-time validation** of constructor usage is unavailable
- **Exhaustiveness checking** is not possible
- **The `_isADT` flag alone is insufficient** for generating detailed error messages

### Specific Code Locations

#### Current Error Generation Approach

1. **CaseElimination.hs:61**
```haskell
transLambdaWithError lam (Error (Lit (LString "pattern match failed") ) NoPos)
```

2. **CaseElimination.hs:202**
```haskell
Error (Lit (LString "pattern match failure in let declaration")) pos
```

3. **CaseElimination.hs:215**
```haskell
Error (Lit (LString $ "pattern match failure in function " ++ f)) pos
```

These hardcoded strings become the generic error messages users see.

## Impact on Users

### Developer Experience Considerations

1. **Debugging Complexity**
   - Users must mentally translate record errors to ADT terms
   - Error messages display internal representation rather than surface syntax
   - Implementation details appear in error messages

2. **Differences from ML-family Languages**
   - Lacks compile-time checking of constructor names
   - Lacks exhaustiveness warnings
   - Typos in constructor names produce runtime errors

3. **Generic Error Messages**
   - Pattern match failures omit specific constructor information
   - Displays tuple destructuring errors instead of ADT-specific messages
   - Uses identical "pattern match failed" message for different failure modes

4. **Learning Curve**
   - Users familiar with ML-family languages may expect different error behavior
   - The relationship between ADT syntax and record implementation requires explanation

## Potential Improvements

### Improvement 1: Enhanced Error Messages in Compiler (Minimal Change)

Enhance error messages during compilation without modifying core transformation:

1. **In CaseElimination.hs**, detect patterns involving ADT records:
   ```haskell
   -- Instead of:
   Error (Lit (LString "pattern match failed")) pos

   -- Generate more informative errors when ADTTag is true:
   Error (Lit (LString "pattern match failed on ADT value")) pos
   ```

2. **In AtomFolding.hs**, preserve constructor names in error paths:
   ```haskell
   -- Add constructor name to error message during pattern compilation
   case findConstructor nm atms of
     Just (cons, _) ->
       -- Include constructor name in generated error literals
       Error (Lit (LString $ "pattern match failed: constructor " ++ nm))
   ```

3. **Thread ADT information through pattern compilation**
   - Modify `compilePattern` in CaseElimination to track ADT-originated patterns
   - Generate distinct error messages for ADT versus regular record patterns

### Improvement 2: Include Value in Error Messages (Better Debugging)

Modify error generation to include the failing value:

1. **Change error generation in CaseElimination.hs**:
   ```haskell
   -- Instead of static error messages:
   transLambdaWithError lam (Error (Lit (LString "pattern match failed")))

   -- Generate code that includes the value:
   transLambdaWithError lam errorWithValue
     where errorWithValue =
             -- Generate code to construct error message with value
             App (Base "raiseMatchError") (Var scrutineeVar)
   ```

2. **Add built-in function for enhanced error reporting**:
   ```haskell
   -- In IR.hs, add to built-ins:
   "raiseMatchError"  -- Function that formats match errors with values
   ```

### Improvement 3: Preserve Constructor Metadata (More Complete)

Retain constructor information further through the compilation pipeline:

1. **Extend Record representation** in intermediate phases:
   ```haskell
   -- In Direct.hs or Core.hs:
   data Term = ...
             | Record Fields ADTTag (Maybe (DataTypeName, ConstructorName))
   ```

2. **Preserve metadata through transformations**:
   ```haskell
   -- In AtomFolding.hs:
   visitTerm atms (DataTypeConstructor nm args) =
     Record fields True (Just (datatypeName, nm))
   ```

3. **Use metadata for error messages**:
   ```haskell
   -- In CaseElimination.hs:
   case term of
     Record _ True (Just (dt, cons)) ->
       Error (Lit (LString $ "Constructor " ++ cons ++
                            " of type " ++ dt ++ " did not match"))
   ```

## Recommendations

### Short Term (Minimal Effort)
1. Improve error literals in `CaseElimination.hs` to include position information
2. Add source position to all pattern match errors
3. Document the limitation in user guides

### Medium Term (Moderate Effort)
1. Implement Improvement 1 - preserve minimal ADT context for errors
2. Add compile-time warnings for obvious issues (undefined constructors)
3. Enhance runtime `stringRep` to show ADT values in errors

### Long Term (If More ADT Features Are Desired)
1. Consider Improvement 2 or 3 for additional ADT capabilities
2. Potentially add exhaustiveness checking
3. Consider constructor arity validation at compile time

## Conclusion

The current ADT implementation achieves its goal of providing syntactic sugar with zero runtime cost, adhering to the design principle of maintaining runtime simplicity. The trade-off involves generic rather than ADT-specific error messages. The transformation in the AtomFolding phase results in ADT-specific information being unavailable for error reporting in subsequent phases.

The design successfully delivers ADT functionality with minimal runtime modifications. Should more informative error messages become a priority, the suggested improvements offer approaches ranging from minimal adjustments to comprehensive enhancements, providing flexibility based on available resources and user requirements.

The central observation is that enhanced error messages would necessitate preserving additional semantic information about ADT origins throughout the compilation pipeline, introducing complexity to the current straightforward transformation approach.