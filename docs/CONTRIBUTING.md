# Contributing

> **Scope:** conventions for changing the codebase — the test-suite layout and a worked example of
> adding a built-in function. For background on how the system is structured see
> [ARCHITECTURE.md](ARCHITECTURE.md); for build/test commands see [DEVELOPMENT.md](DEVELOPMENT.md).

## Test suite layout

Tests live in `tests/`:

- `cmp/` — negative compiler tests
- `lib/` — standard-library tests
- `rt/` — runtime tests
  - `pos/` — positive tests (should succeed): `core/`, `ifc/` (with `ifc/sandbox/`), `preamble/`
  - `neg/` — negative tests (should fail)
  - `timeout/` — tests with timeouts
  - `warn/` — tests that should produce warnings
  - `multinode-tests/` — multinode (networking) tests

Throwaway/experimental tests go in `tests/_unautomated/` (see `CLAUDE.md`).

Non-networking tests pair a `.trp` source with a `.golden` expected-output file; the `bin/golden` utility compares output using a diff that discards timestamped values. Multinode tests do not use golden files — see `tests/rt/multinode-tests/README.md`.

## Adding a built-in function

Adding a built-in requires changes in both the compiler and the runtime. The easiest way to
list all existing built-ins is to inspect `compiler/src/IR.hs`.

### 1. Compiler registration

Add the function name to the built-in list in `compiler/src/IR.hs`:

```haskell
wfir (Base fname) =
    if  fname `elem`[
        -- existing built-ins...
        , "yourNewFunction"  -- add your function name here
        ]
```

### 2. Runtime implementation

Create `rt/src/builtins/yourFunction.mts`:

```typescript
'use strict'
import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';

export function BuiltinYourFunction<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        yourNewFunction = mkBase((larg) => {
            // Implementation. Use assertIsX functions for type checking
            // and lub() for security-level calculations.
            return this.runtime.ret(new LVal(result, resultLevel));
        }, "yourNewFunction")
    }
}
```

### 3. Runtime registration

In `rt/src/UserRuntime.mts`, import the built-in and add it to the composition chain (order matters for dependencies):

```typescript
import { BuiltinYourFunction } from './builtins/yourFunction.mjs'

export const UserRuntime =
    BuiltinYourFunction (
    // ... rest of the existing chain
```

### 4. Build and test

```bash
make compiler   # rebuild compiler
make rt         # rebuild runtime
make test       # run tests
```

Notes:

- Built-ins must handle information flow control using `lub()` for security levels.
- Use the `assertIsX` helpers from `Asserts.mjs` for type safety.
- The name in `IR.hs` must exactly match the runtime function name.
