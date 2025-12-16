# Cabal + GHCup Migration Plan

This document outlines the migration from Stack to Cabal + GHCup for the Troupe compiler build system.

## Migration Overview

**Current State**: Stack-based build with LTS-21.25 (GHC 9.4.8)  
**Target State**: Cabal + GHCup managed toolchain with pinned versions  
**Estimated Effort**: 1-2 days  
**Migration Type**: Low-risk modernization with improved reproducibility

## Benefits of Migration

- **Faster Builds**: Cabal builds are typically faster for projects this size
- **Reproducible Builds**: GHCup ensures exact same toolchain versions across all environments
- **Cross-Platform Consistency**: Same GHC/Cabal versions on Linux, macOS, Windows
- **Modern Toolchain**: Aligns with current Haskell ecosystem recommendations
- **Better CI/CD**: Improved caching and faster setup with GHCup GitHub Actions
- **Version Management**: Easy switching between GHC versions for testing
- **Developer Experience**: Consistent local development environment

## Current State Analysis

### Stack Configuration
- **Resolver**: lts-21.25 (GHC 9.4.8)
- **Packages**: Single package (`.`)  
- **Extra Dependencies**: None
- **Build Tools**: alex, happy (managed by Stack)
- **System GHC**: Allowed newer minor versions

### Package Structure
- **Executables**: 4 (`troupec`, `dclabels`, `irtester`, `golden`)
- **Libraries**: Single library with 31 source files (~7,907 lines)
- **Tests**: 2 test suites (golden tests, IR transformation tests)
- **Dependencies**: 20 standard Haskell packages
- **Generated Files**: Uses alex/happy for lexer/parser

## Migration Steps

### Phase 1: GHCup Setup and Toolchain Installation (45 minutes)

1. **Install GHCup** (if not already installed)
   ```bash
   # Install GHCup
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   
   # Reload shell environment
   source ~/.bashrc  # or ~/.zshrc
   
   # Verify installation
   ghcup --version
   ```

2. **Install Pinned Toolchain Versions**
   ```bash
   # Install specific GHC version for reproducibility
   ghcup install ghc 9.6.4
   ghcup set ghc 9.6.4
   
   # Install specific Cabal version
   ghcup install cabal 3.10.2.1
   ghcup set cabal 3.10.2.1
   
   # Install build tools via GHCup (recommended)
   ghcup install alex latest
   ghcup install happy latest
   ```

3. **Create Version Configuration Files**
   ```bash
   # Create .tool-versions for asdf compatibility (optional)
   echo "ghc 9.6.4" > .tool-versions
   echo "cabal 3.10.2.1" >> .tool-versions
   
   # Create cabal.project with version constraints
   cat > cabal.project << EOF
   packages: compiler/
   
   -- Pin GHC version for reproducibility
   with-compiler: ghc-9.6.4
   
   -- Optimization flags
   optimization: 2
   
   -- Documentation generation
   documentation: False
   
   -- Parallel building
   jobs: \$ncpus
   EOF
   ```

4. **Verify Toolchain Setup**
   ```bash
   # Verify exact versions
   ghc --version          # Should show 9.6.4
   cabal --version        # Should show 3.10.2.1
   alex --version         # Latest installed version
   happy --version        # Latest installed version
   
   # Verify cabal configuration
   cabal --version
   cabal user-config init  # Initialize user config if needed
   ```

5. **Generate Cabal File from package.yaml**
   ```bash
   cd compiler
   # Use hpack to generate .cabal from package.yaml
   hpack  # or cabal build --dry-run if hpack not available
   ```

### Phase 2: Makefile Migration (45 minutes)

**File**: `compiler/Makefile`

**Current Stack Commands → Cabal Replacements:**

```makefile
# OLD (Stack-based)
all:
	stack build $(STACK_OPTS)
	mkdir -p ./../bin
	stack install $(STACK_OPTS) --local-bin-path ./../bin/

clear:
	rm *.cabal 
	stack clean

ghci-irtester:
	stack ghci --main-is Troupe-compiler:exe:irtester --no-load

ghci-troupec:
	stack ghci --main-is Troupe-compiler:exe:troupec --no-load

test: 
	stack test $(STACK_OPTS)

parser-info:
	stack exec happy -- -i src/Parser.y
```

**NEW (Cabal-based):**

```makefile
# Cabal-based build system
all:
	cabal build all
	mkdir -p ./../bin
	cabal install --install-method=copy --installdir=./../bin all

clear:
	cabal clean

ghci-irtester:
	cabal repl exe:irtester

ghci-troupec:
	cabal repl exe:troupec

test: 
	cabal test all

parser-info:
	happy -i src/Parser.y

# Development helpers
build-dev:
	cabal build --enable-optimization=0

install-dev:
	cabal install --install-method=copy --installdir=./../bin --enable-optimization=0 all
```

### Phase 3: Root Makefile Updates (15 minutes)

**File**: `Makefile` (root level)

**Update Stack Target:**
```makefile
# OLD
stack:
	$(MAKE) -C compiler 

# NEW  
stack:
	$(MAKE) -C compiler all
```

### Phase 4: Testing and Validation (30 minutes)

1. **Build Test**
   ```bash
   cd compiler
   make clear  # Clean old artifacts
   make all    # Full build with cabal
   ```

2. **Executable Verification**
   ```bash
   # Verify all executables are built and work
   ../bin/troupec --version
   ../bin/dclabels --help
   ../bin/irtester --help
   ../bin/golden --help
   ```

3. **Test Suite Execution**
   ```bash
   cd compiler
   make test   # Run all tests
   ```

4. **Integration Test**
   ```bash
   cd ..
   make stack  # Should use new cabal build
   make rt     # Verify runtime still builds
   make libs   # Verify libraries still compile
   ```

### Phase 5: CI/CD Integration with GHCup (45 minutes)

**Update Docker Configuration**:

```dockerfile
# OLD (Stack-based)
FROM fpco/stack-build:lts-21.25
ENV STACK_OPTS --system-ghc
RUN make stack

# NEW (GHCup + Cabal based)
FROM ubuntu:22.04

# Install system dependencies
RUN apt-get update && apt-get install -y \
    curl build-essential libffi-dev libffi8ubuntu1 libgmp-dev \
    libgmp10 libncurses-dev libncurses5 libtinfo5 \
    && rm -rf /var/lib/apt/lists/*

# Install GHCup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Add GHCup to PATH
ENV PATH="/root/.ghcup/bin:$PATH"

# Install specific toolchain versions
RUN ghcup install ghc 9.6.4 && ghcup set ghc 9.6.4
RUN ghcup install cabal 3.10.2.1 && ghcup set cabal 3.10.2.1
RUN ghcup install alex latest && ghcup install happy latest

# Copy project files
COPY cabal.project .
COPY compiler/ compiler/
COPY Makefile .

# Build project
RUN make stack
```

**Update GitHub Actions** (`.github/workflows/run_tests.yml`):

```yaml
name: CI
on: [push, pull_request]

jobs:
  build-and-test:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v4
    
    - name: Setup Haskell with GHCup
      uses: haskell-actions/setup@v2
      with:
        ghc-version: '9.6.4'
        cabal-version: '3.10.2.1'
        enable-stack: false
    
    - name: Install alex and happy
      run: |
        ghcup install alex latest
        ghcup install happy latest
    
    - name: Cache cabal store
      uses: actions/cache@v3
      with:
        path: |
          ~/.cabal/store
          ~/.cabal/packages
          dist-newstyle
        key: ${{ runner.os }}-cabal-${{ hashFiles('**/*.cabal', 'cabal.project') }}
        restore-keys: |
          ${{ runner.os }}-cabal-
    
    - name: Update cabal package database
      run: cabal update
    
    - name: Build project
      run: |
        make stack
        make rt
        make libs
        make service
    
    - name: Run tests
      run: make test
```

**Multi-Platform CI Support**:

```yaml
strategy:
  matrix:
    os: [ubuntu-latest, macos-latest, windows-latest]
    ghc-version: ['9.6.4']
    
runs-on: ${{ matrix.os }}

- name: Setup Haskell (Cross-platform)
  uses: haskell-actions/setup@v2
  with:
    ghc-version: ${{ matrix.ghc-version }}
    cabal-version: '3.10.2.1'
```

## Migration Checklist

### Pre-Migration
- [ ] Backup current working state
- [ ] Install GHCup if not already installed
- [ ] Install pinned toolchain versions (GHC 9.6.4, Cabal 3.10.2.1)
- [ ] Create version configuration files (cabal.project, .tool-versions)
- [ ] Generate .cabal file from package.yaml

### Migration
- [ ] Update `compiler/Makefile` with cabal commands
- [ ] Update root `Makefile` 
- [ ] Test build process with new toolchain
- [ ] Verify all 4 executables work
- [ ] Run test suite
- [ ] Test integration with runtime build

### Post-Migration
- [ ] Update CI/CD GitHub Actions with GHCup setup
- [ ] Update Docker configuration with GHCup installation
- [ ] Update documentation with GHCup setup instructions
- [ ] Remove Stack artifacts (stack.yaml, .stack-work/)
- [ ] Test multinode tests still work
- [ ] Verify cross-platform compatibility
- [ ] Document developer onboarding with GHCup

## Rollback Plan

If issues arise during migration:

1. **Quick Rollback**:
   ```bash
   git checkout HEAD -- compiler/Makefile Makefile
   cd compiler && make clear && make all
   ```

2. **Keep Both Systems** (temporary):
   - Rename current Makefile targets to `stack-*`
   - Add new `cabal-*` targets
   - Gradually transition

## Potential Issues and Solutions

### Issue 1: Alex/Happy Not Found
**Solution**: Install via cabal, system package manager, or GHCup

### Issue 2: GHC Version Mismatch
**Solution**: Update to GHC 9.6+ or use cabal's GHC version constraints

### Issue 3: Dependency Resolution Conflicts
**Solution**: Use `cabal freeze` to lock versions, or adjust bounds in package.yaml

### Issue 4: Path Issues with Executables
**Solution**: Verify `--installdir` path matches expected locations

### Issue 5: CI/CD Breaks
**Solution**: Update Docker base image and installation steps

## Performance Comparison

**Before (Stack)**:
- Initial build: ~3-5 minutes (including Stack setup)
- Incremental build: ~30-60 seconds
- CI setup time: ~2-3 minutes

**Expected After (Cabal)**:
- Initial build: ~2-3 minutes
- Incremental build: ~20-40 seconds  
- CI setup time: ~1 minute

## Long-term Benefits

1. **Reproducibility**: Exact same toolchain versions across all environments
2. **Cross-Platform**: Consistent builds on Linux, macOS, and Windows
3. **Performance**: Faster builds and CI times with better caching
4. **Maintainability**: Simpler build system with explicit version management
5. **Developer Experience**: Easy toolchain switching and updates
6. **CI/CD**: Better caching strategies and faster setup times
7. **Future-Proofing**: Easy to update GHC versions for new language features

## Next Steps After Migration

1. **Update Documentation**: Revise build instructions in README with GHCup setup
2. **Developer Onboarding**: Create setup guide for new contributors using GHCup
3. **Cross-Platform Testing**: Validate builds on macOS and Windows
4. **CI Optimization**: Implement advanced caching strategies with cabal store
5. **Docker Multi-Stage**: Create optimized multi-stage Docker builds
6. **Version Management**: Establish process for updating GHC/Cabal versions
7. **Performance Monitoring**: Track build performance improvements and regression tests

## GHCup-Specific Benefits

### Developer Experience
- **Easy Setup**: Single command installs entire Haskell toolchain
- **Version Switching**: Test with different GHC versions easily
- **Isolation**: No conflicts with system packages
- **Updates**: Simple toolchain updates and maintenance

### CI/CD Advantages
- **Caching**: Better layer caching in Docker with pinned versions
- **Speed**: Faster setup compared to Stack resolver downloads
- **Consistency**: Same environment across development and production
- **Matrix Testing**: Easy testing across multiple GHC versions

### Production Benefits
- **Reproducibility**: Bit-for-bit identical builds across environments
- **Deployment**: Consistent runtime behavior
- **Debugging**: Same toolchain for development and production issues
- **Compliance**: Auditable toolchain versions for security requirements

---

**Migration Champion**: TBD  
**Timeline**: 1-2 days  
**Risk Level**: Low  
**Dependencies**: System GHC 9.6+, alex, happy