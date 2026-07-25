# Contributing

> **Scope:** conventions for changing the codebase — the test-suite layout and a worked example of
> adding a built-in function. For the pull-request policy see the repository-root
> [CONTRIBUTING.md](../CONTRIBUTING.md). For background on how the system is structured see
> [ARCHITECTURE.md](ARCHITECTURE.md); for build/test commands see [DEVELOPMENT.md](DEVELOPMENT.md).

## Test suite layout

Tests live in `tests/`:

- `cmp/` — negative compiler tests, with `modules/` and `synvar/` subdirectories
- `lib/` — standard-library tests
- `rt/` — runtime tests
  - `pos/` — positive tests (should succeed): `bigint/`, `core/`, `ifc/` (with
    `ifc/blocking_pini_leaks/`, `ifc/nmifc/`, `ifc/projection-typelabel/`, `ifc/sandbox/`),
    `modules/`, `preamble/`, `synvar/`
  - `neg/` — negative tests (should fail): `bigint/`, `core/`, `ifc/` (with `ifc/nmifc/`,
    `ifc/projection-progress/`), `preamble/`
  - `timeout/` — `blocking/` (with `blocking/neg/`) and `diverging/`; both run under an 8-second
    external `timeout` and must not terminate on their own, and differ only in the diff wrapper
    used (`diverging/` compares the first 100 lines of output)
  - `warn/` — tests that should produce warnings
  - `multinode-tests/` — multinode (networking) tests
  - `result-socket/` — result-socket tests, driven by their own scripts
- `_util/` — the diff wrappers the golden runner invokes
- `_old_tests/` — retired tests, not run

Throwaway/experimental tests go in `tests/_unautomated/` (see `CLAUDE.md`).

The golden runner (`compiler/test/Golden.hs`) collects `.trp` files from `tests/cmp`,
`tests/rt/pos`, `tests/rt/neg`, `tests/rt/warn`, `tests/rt/timeout/blocking`,
`tests/rt/timeout/diverging`, and `tests/lib`. At startup it locates the Troupe root — from the
installed `bin/golden` path, by searching upward from the working directory for the `.troupe-root`
marker file, or from `$TROUPE` — and changes into it, so it can be run from any subdirectory of a
checkout. Files under a directory component named `modsrc` are excluded from collection: they are
module sources compiled by the test that imports them, not tests themselves.

Non-networking tests pair a `.trp` source with a `.golden` expected-output file, and with a
`.nocolor.golden` file for runs under `bin/golden --no-color`. The `bin/golden` utility compares
output using a diff wrapper (`tests/_util/diff.sh`) that discards timestamped values and uuids. A
test may also carry a `<name>.trp.input` file, whose contents are fed to the program on standard
input, and a `<name>.trp.options` file, whose contents are parsed shell-style and appended to the
`local.sh` invocation (`#` lines are comments). A program that imports program-relative modules
carries a `<name>.deps.json` pinning each module by content hash. Multinode tests do not use golden
files — see `tests/rt/multinode-tests/README.md`.

By default the runner executes every collected test twice, once with Raw optimization and once with
`--no-rawopt`; `bin/golden --quick` runs only the optimized pass.

## Adding a built-in function

Adding a built-in requires changes in both the compiler and the runtime. The list of all existing
built-ins is the `wfir (Base fname)` case in `compiler/src/IR.hs`; it is the only place the
compiler checks base-function names.

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
make lib        # rebuild the Troupe libraries against the new compiler
make rt         # rebuild runtime
make test       # run tests (bin/golden for the golden suite alone)
```

Notes:

- Built-ins must handle information flow control using `lub()` for security levels.
- Use the `assertIsX` helpers from `Asserts.mjs` for type safety.
- The name in `IR.hs` must exactly match the runtime function name.
