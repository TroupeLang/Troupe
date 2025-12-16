# No-Color Implementation

This document describes the implementation of the `--no-color` option in the Troupe programming language system, which provides explicit control over colored terminal output.

## Overview

The no-color feature implements the [NO_COLOR standard](https://no-color.org/) and provides a `--no-color` CLI flag to disable colored output throughout the Troupe system. This affects both runtime error messages and the golden test infrastructure.

## Implementation Architecture

### Runtime Components

#### 1. CLI Argument Handling (`rt/src/TroupeCliArgs.mts`)

The `--no-color` flag is defined in the CLI arguments enum and interface:

```typescript
export enum TroupeCliArg {
    // ... other args
    NoColor = 'no-color',
}

export interface ParsedArgs {
    // ... other args
    [TroupeCliArg.NoColor]?: boolean;
}
```

The yargs configuration includes special coercion logic to handle yargs' negation behavior:

```typescript
.option(TroupeCliArg.NoColor, { 
    type: 'boolean', 
    default: false, 
    describe: 'Disable colored output (also respects NO_COLOR env var)',
    coerce: (arg) => {
        // Handle yargs interpreting --no-color as negation
        return process.argv.includes('--no-color');
    }
})
```

#### 2. Color Configuration Module (`rt/src/colorConfig.mts`)

Central module that manages color settings across the runtime:

```typescript
import chalk from 'chalk';
import { getCliArgs, TroupeCliArg } from './TroupeCliArgs.mjs';

let colorConfigured = false;

export function configureColors(): void {
    if (colorConfigured) return;
    const argv = getCliArgs();
    // Disable colors if NO_COLOR env var is set OR --no-color flag is used
    if (process.env.NO_COLOR || argv[TroupeCliArg.NoColor]) {
        chalk.level = 0;  // Disable chalk coloring
    }
    colorConfigured = true;
}

export function isColorEnabled(): boolean {
    const argv = getCliArgs();
    return !(process.env.NO_COLOR || argv[TroupeCliArg.NoColor]);
}
```

#### 3. Runtime Integration (`rt/src/runtimeMonitored.mts`)

The runtime initializes color configuration early in the startup process:

```typescript
import { configureColors, isColorEnabled } from './colorConfig.mjs';

// Configure colors based on CLI args and environment
configureColors();

// Console creation respects color settings
let rt_xconsole = 
    new Console({ stdout: process.stdout
                , stderr: process.stderr
                , colorMode: isColorEnabled()
               });
```

#### 4. Logger Configuration (`rt/src/logger.mts`)

The Winston logger conditionally applies colorization:

```typescript
import { isColorEnabled } from './colorConfig.mjs';

// Conditionally add colorization to format list
if (isColorEnabled()) {
    formatList.unshift(format.colorize());
}
```

#### 5. Script Integration (`local.sh`)

The local execution script separates compiler and runtime arguments:

```bash
for arg in "$@"; do
    case "$arg" in
        --no-color)
            runtime_args="$runtime_args $arg"
            ;;
        *)
            compiler_args="$compiler_args $arg"
            ;;
    esac
done

# Pass runtime args to the runtime
eval "node --stack-trace-limit=1000 $TROUPE/rt/built/troupe.mjs -f=$tmp --localonly $runtime_args"
```

### Testing Infrastructure

#### 1. Golden Test Extension (`compiler/test/Golden.hs`)

Extended the golden test system with a proper tasty option:

```haskell
-- Custom option type for no-color mode
newtype NoColorOption = NoColorOption Bool
  deriving (Eq, Ord, Typeable)

-- Proper IsOption instance using tasty's flagCLParser
instance IsOption NoColorOption where
  defaultValue = NoColorOption False
  parseValue = fmap NoColorOption . safeRead
  optionName = return "no-color"
  optionHelp = return "Disable colored output (generates .nocolor.golden files)"
  optionCLParser = flagCLParser Nothing (NoColorOption True)
```

#### 2. Test Configuration

The `TestConfig` data structure includes color settings:

```haskell
data TestConfig = TestConfig 
    { tcRawOpt :: Bool     -- Raw optimization flag
    , tcNoColor :: Bool    -- No-color flag
    }
```

#### 3. Golden File Naming

Test output files are named based on color mode:

```haskell
goldenFileName :: String -> TestConfig -> String
goldenFileName troupeFile TestConfig{..} = 
    if tcNoColor 
    then replaceExtension troupeFile ".nocolor.golden"
    else replaceExtension troupeFile ".golden"
```

#### 4. Runtime Argument Generation

Test configurations generate appropriate runtime arguments:

```haskell
mkRunArgs :: TestConfig -> [String]
mkRunArgs TestConfig{..} =
    (if tcRawOpt then [] else ["--no-rawopt"]) ++
    (if tcNoColor then ["--no-color"] else [])
```

## Usage

### Command Line Usage

```bash
# Disable colors via CLI flag
./local.sh myprogram.trp --no-color

# Disable colors via environment variable
NO_COLOR=1 ./local.sh myprogram.trp

# Both methods work together
NO_COLOR=1 ./local.sh myprogram.trp --no-color
```

### Golden Testing

```bash
# Run tests with colored output (default)
./bin/golden -p mytest

# Run tests with no-color output (generates .nocolor.golden files)
./bin/golden -p mytest --no-color

# Pattern matching works with both modes
./bin/golden -p zero --no-color
```

## Color Sources

The implementation handles multiple sources of colored output:

1. **Chalk Library**: Used for terminal styling in error messages and runtime output
2. **Winston Logger**: Provides colored log levels and timestamps
3. **Console Output**: Node.js console coloring for general output

Note: User programs that generate their own colors are not affected by this setting, as those are part of the program's intended output.

## Standards Compliance

The implementation follows the [NO_COLOR standard](https://no-color.org/):

- Respects the `NO_COLOR` environment variable
- Any non-empty value of `NO_COLOR` disables colors
- Provides explicit CLI control via `--no-color`
- Maintains backwards compatibility

## File Structure

```
rt/src/
├── TroupeCliArgs.mts          # CLI argument definitions
├── colorConfig.mts            # Central color configuration
├── runtimeMonitored.mts       # Runtime initialization
└── logger.mts                 # Logger configuration

compiler/test/
└── Golden.hs                  # Golden test infrastructure

scripts/
└── local.sh                   # Execution script with argument routing
```

## Technical Details

### Initialization Order

1. CLI arguments are parsed (`TroupeCliArgs.mts`)
2. Color configuration is applied (`colorConfig.mts`)
3. Runtime components initialize with color settings
4. Logger and console respect the configuration

### Golden Test Generation

- Default mode generates `.golden` files
- No-color mode generates `.nocolor.golden` files
- Both modes can coexist in the same test suite
- Test selection is based on the `--no-color` flag to the golden executable

### Environment Variable Precedence

Both `NO_COLOR` environment variable and `--no-color` CLI flag disable colors. Either condition is sufficient to disable coloring throughout the system.