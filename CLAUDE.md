# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Troupe is an actor-based programming language with dynamic information flow control. The codebase consists of three main components:

1. **Compiler** (`/compiler/`) - Haskell-based compiler that transforms Troupe code to JavaScript
2. **Runtime** (`/rt/`) - TypeScript/JavaScript runtime implementing the actor model and information flow control. Note that the folder (`/rt/built`) is used as target for the generated code; it should be ignored for source code analysis.
3. **Standard Library** (`/lib/`) - Built-in libraries written in Troupe

### Additional tools 

In addition to the core language runtime  and the compiler, the codebase includes a few tools. 

1. **P2P** tools (`/p2p-tools`). 

   1. **Libp2p relay** (`relay/`) - TypeScript/JavaScript implementation of a libp2p relay. In multinode deployments we want to offer to our users at least one relay in order to allow communicating with Troupe nodes behind NAT.



## Folders with executables

- Executable binaries (`/bin`). Contains compiled binaries. Important to not add any scripts or other version-controllable artifacts in here, because this folder is to be ignored by version control. 

- Executiable scripts (`/scripts`). Contains useful executable scripts that are version controlled. 


## Essential Commands

### Building the Project

```bash
# Build everything (recommended for significant changes)
make all

# Build individual components
make stack      # Build the compiler
make rt         # Build the runtime
make libs       # Compile standard libraries
make service    # Compile service module
```

### Running Tests

```bash
# Run full test suite
make test

# Run golden tests with options
bin/golden

# Run a test with specific pattern. Beware that slashes are not allowed in the patterns.
bin/golden -p <the-pattern>
```

### Running Troupe Programs
There are two convenient scripts for running Troupe programs. They are in the root folder, and should remain there because of how frequently they are accessed.


```bash
# Local execution (no P2P networking, faster startup)
./local.sh myprogram.trp

# Network execution (with P2P support)
./network.sh myprogram.trp

# With debugging
./local.sh myprogram.trp --debug
```

### Development Commands

```bash
# Clean build artifacts
make clear-built-rt

# Interactive Haskell REPL for compiler development
cd compiler && make ghci-troupec

# Check parser info
cd compiler && make parser-info
```

## Architecture Overview

### Compilation Pipeline

The Troupe compiler transforms source code through multiple stages:

1. **Parsing** (`Parser.y`, `Lexer.x`) - Parse `.trp` files into AST
2. **Core Transformations**:
   - Pattern elimination (`DirectWOPats.hs`)
   - Function/let lowering
   - Alpha renaming
   - CPS transformation (`RetCPS.hs`, `RetDFCPS.hs`)
   - CPS optimization (`CPSOpt.hs`)
   - Closure conversion (`ClosureConv.hs`)
3. **Code Generation**:
   - IR → Raw (`IR2Raw.hs`)
   - Raw → Stack (`Raw2Stack.hs`)
   - Stack → JavaScript (`Stack2JS.hs`)

### Runtime Architecture

The runtime implements:

- **Actor System**: Process spawning, message passing, mailbox management
- **Information Flow Control**: Security levels, label tracking, declassification
- **P2P Networking**: libp2p integration for distributed actors
- **Built-in Functions**: Located in `/rt/src/builtins/`
- **Level System**: Various label implementations in `/rt/src/levels/`

### Key Runtime Components

- `troupe.mts` - Main entry point
- `runtimeMonitored.mts` - Gluing point for most of the runtime
- `Scheduler.mts` - Scheduler
- `MailboxProcessor.mts` - Message handling
- `TrustManager.mts` - Trust and security management
- `p2p/p2p.mts` - P2P networking layer
- `builtins` - Many language built-ins.

## Testing Strategy


Tests are organized in `/tests/`, with the following subfolders

- `cmp` - Negative compiler tests.
- `rt` - Runtime tests 
   - `pos/` - Positive tests (should succeed)
      - `core/` - Core language features
      - `ifc/` - Information flow control
      - `sandbox/` - Sandboxing tests
   - `neg/` - Negative tests (should fail)
   - `timeout/` - Tests with timeouts
   - `warn/` - Tests that should produce warnings
   - `multinode/` - Multinode (networking) tests


## Creating new tests

Do not put tests into the folders with existing .golden files, without explicit permission! 

### Testing non-networking functionality

Non-network tests have a `.trp` source file and a `.golden` file with expected output. The comparison of the expected output is handled using the golden utility. This is needed because outputs often include timestamped value and that utility invokes the special diff that discards the timestamps. 


### Multinode tests 

Multinode tests do not use the `golden` functionality. They should instead use the functionality described in 
`tests/rt/multinode-tests/README.md` using the scripts in the `scripts` folder. 

When creating new multinode tests, do not create any ids or aliases or trustmaps. Instead, this needs to be coordinated using the corresponding `config.json`.

### Guidelines for creating new tests 

- Use Troupe syntax (unless working specifically on negative parsing tests)! Please consult both the existing positive test corpus for examples of Troupe programs and the user-guide referenced in this document for how to write Troupe programs.

- The easiest way to list all the built-ins is by inspecting the `compiler/src/IR.hs` where they are included in a long list.

- Remember that there is Troupe standard library that may have useful functionality. 

- Troupe compiler `troupec` can be used to test tests for syntactic validity.

- Local tests can be executed directly using `local.sh` script. 

#### Golden files for new tests

When creating new tests, do not create `.golden` files manually. They are auto-generated by the `bin/golden` utility upon detection of a missing `.golden` file, for the non-network tests; and are not required at all for the multinode tests.




## Information Flow Control

Troupe implements dynamic information flow control with:

- **Security Levels**: High/Low, DC labels, custom lattices
- **PC (Program Counter) Label**: Tracks implicit flows
- **Declassification**: Controlled information release
- **Sandboxing**: Isolated execution with label constraints


## File Extensions

- `.trp` - Troupe source files
- `.picox`, `.pico`, `.femto`, `.atto` - Test file variants
- `.golden` - Expected test outputs
- `.exports` - Library export definitions

## Troupe language user guide

Troupe language user guide is available at https://troupelang.github.io/troupe-user-guide-jb/. The guide currently uses the V1 label syntax `` `{alice}` ``, which the present Troupe runtime interprets as DC Labels corresponding to `` `<alice ; alice >` ``. 


## Development Tips

1. Use `./local.sh` for quick testing without P2P overhead
2. The compiler must be rebuilt after changes to Haskell code
3. Runtime changes require `make rt` to recompile TypeScript
4. Library changes require `make libs` to recompile
5. VSCode with Haskell and TypeScript extensions provides good IDE support
6. Set `TROUPE` environment variable to the repository root

### Adding New Built-in Functions

To add a new built-in function to Troupe, you need to make changes in both the compiler and runtime:

#### 1. Compiler Registration
Add the function name to the built-in list in `/compiler/src/IR.hs` (around lines 262-337):
```haskell
wfir (Base fname) =
    if  fname `elem`[ 
        -- existing built-ins...
        , "yourNewFunction"  -- Add your function name here
        ]
```

#### 2. Runtime Implementation
Create a new file `/rt/src/builtins/yourFunction.mts`:
```typescript
'use strict'
import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';

export function BuiltinYourFunction<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        yourNewFunction = mkBase((larg) => {
            // Your implementation here
            // Use assertIsX functions for type checking
            // Use lub() for security level calculations
            return this.runtime.ret(new LVal(result, resultLevel));
        }, "yourNewFunction")
    }
}
```

#### 3. Runtime Registration
Update `/rt/src/UserRuntime.mts`:
1. Import your new built-in:
   ```typescript
   import { BuiltinYourFunction } from './builtins/yourFunction.mjs'
   ```
2. Add it to the composition chain (order matters for dependencies):
   ```typescript
   export const UserRuntime =
       BuiltinYourFunction (
       // ... rest of the existing chain
   ```

#### 4. Build and Test
```bash
make stack      # Rebuild compiler
make rt         # Rebuild runtime
make test       # Run tests
```

#### Notes:
- Built-in functions must handle Troupe's information flow control using `lub()` for security levels
- Use appropriate `assertIsX` functions from `Asserts.mjs` for type safety
- The function name in IR.hs must exactly match the runtime function name
- Consider adding tests in `/tests/rt/pos/core/` for your new built-in




### Temporary test generation

For temporary test generation, please use the folder `tests/_unautomated/claude`. 

### Testing the compiler-generated output.

The compiler binary `troupec` has an option for verbose output `-v`, and the generated files are written into the `/out` folder, named with different stages of the compilation. 


#### Turning off raw optimizations

Some bugs may be further caught by turning off the raw ouptimizations. There is a flag `--no-rawopt` that disables Raw optimizations. It can be helpful for some corner compiler-related bugs.

#### Troupe parser implementation.

There should be no shift/reduce or reduce/reduce conflicts in the Troupe parser.


### Troupe programs

#### Syntax 

See the user guide for the exact language syntax, that is useful when creating tests.

#### Syntax highlighting

Troupe programs use Standard ML - style syntax, and that can be used for syntax highlighting.

## Markdown Formatting

When creating markdown tables, align columns for readability in raw view:

```markdown
| Column One                | Column Two   | Column Three                |
|---------------------------|--------------|------------------------------|
| `short`                   | `value`      | Description here             |
| `longer_entry`            | `val`        | Another description          |
```

## Common Pitfalls

- Remember to rebuild both compiler and runtime after pulling changes
- Use absolute paths in the runtime code
- P2P initialization can be slow; use `--localonly` for local testing.
- Test files may require specific `.input` files for stdin
- Golden tests are sensitive to output formatting

