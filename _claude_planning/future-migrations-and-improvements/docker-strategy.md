# Docker Containerization Strategy

This document outlines the modern Docker containerization strategy for the Troupe project, addressing development, CI/CD, and production deployment needs.

## Current State Analysis

### Problems with Existing Docker Configuration

The current `Dockerfile` has significant issues that need immediate attention:

1. **Outdated Base Images**:
   - `ubuntu:bionic` (Ubuntu 18.04) - **End of Life since April 2023**
   - `fpco/stack-build:lts-21.25` - Outdated Stack-based Haskell environment
   - Node.js 14.x - **End of Life since April 2023**

2. **Security Vulnerabilities**:
   - Multiple EOL operating systems with no security patches
   - Deprecated package installation methods (`apt-key add`)
   - Large attack surface with unnecessary development dependencies

3. **Build Inefficiencies**:
   - No multi-stage optimization
   - Missing build artifact caching
   - Large image sizes (~2-3GB for production use)

## Modern Docker Strategy

### Integration with Cabal Migration

This Docker strategy is designed to work seamlessly with the **Cabal + GHCup migration** outlined in `docs/cabal-migration.md`:

- **No Stack Dependencies**: Containers use GHCup-managed Cabal instead of Stack
- **Pinned Versions**: GHC 9.6.4 and Cabal 3.10.2.1 for reproducible builds
- **Consistent Toolchain**: Same versions across development, CI, and production
- **Build Performance**: Cabal's faster builds improve container build times

### libp2p Upgrade Integration

**Critical Dependency**: The Docker strategy must coordinate with the **libp2p upgrade** (v0.45.3 → v2.8.9):

- **Breaking Changes**: Complete API rewrite, ESM modules, TypeScript migration
- **Node.js Requirement**: libp2p v2.x requires Node.js 16+ (Node.js 22 recommended)
- **Container Impact**: P2P networking, multinode tests, relay servers all affected
- **Timeline**: Docker containers **MUST wait** for libp2p upgrade completion

**Container-Specific libp2p Considerations**:
```dockerfile
# P2P networking now requires ESM support
COPY package.json /app/
RUN npm install  # Will install libp2p v2.x with breaking changes

# Multinode tests need updated P2P APIs
COPY tests/rt/multinode-tests/ /app/tests/rt/multinode-tests/
RUN make libs  # Libraries may use P2P functionality

# Relay server requires libp2p v2.x compatibility
COPY p2p-tools/ /app/p2p-tools/
RUN cd p2p-tools && npm install && npm run build
```

### Node.js Version Strategy

**Node.js 22 LTS** is the current Long Term Support version (as of October 2024):

- **Support Timeline**: Active LTS until October 2025, Maintenance LTS until April 2027
- **Replaces**: Node.js 18 (EOL April 2025) and Node.js 20 (now in maintenance mode)
- **Benefits**: Latest performance improvements, security updates, and modern features
- **Production Ready**: Stable LTS release suitable for production deployments
- **libp2p Compatibility**: Node.js 22 required for libp2p v2.x ESM modules

**Key Changes from Previous Versions**:
```dockerfile
# OLD: Node.js 14 (EOL) + Stack-based build
FROM fpco/stack-build:lts-21.25
FROM node:14-alpine  # End of Life April 2023
RUN stack build && stack install

# NEW: Node.js 22 LTS + GHCup + Cabal build  
FROM ubuntu:24.04
FROM node:22-alpine  # LTS until April 2027
RUN curl -sSf https://get-ghcup.haskell.org | sh
RUN ghcup install ghc 9.6.4 && ghcup install cabal 3.10.2.1
RUN cabal build all && cabal install all
```

### Multi-Container Architecture

Instead of a single monolithic container, we'll create specialized containers for different use cases:

1. **Development Container** - Full toolchain for local development
2. **CI Fast Container** - Optimized for core tests only  
3. **CI Full Container** - Complete testing including multinode tests
4. **Production Runtime** - Minimal production deployment

### Base Image Strategy

**Development/CI Base**: Ubuntu 24.04 LTS with GHCup-managed Haskell toolchain  
**Runtime Base**: Node.js 22 LTS Alpine for minimal production footprint  
**Security**: Regular updates and vulnerability scanning integration

## Container Specifications

### 1. Development Container (`troupe-dev:latest`)

**Purpose**: Complete development environment for contributors  
**Target Size**: 2-3GB (acceptable for development)

```dockerfile
FROM ubuntu:24.04 as troupe-dev

# Install system dependencies
RUN apt-get update && apt-get install -y \
    curl build-essential libffi-dev libgmp-dev \
    libncurses-dev git vim && \
    rm -rf /var/lib/apt/lists/*

# Install GHCup and Haskell toolchain
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="$HOME/.ghcup/bin:$PATH"
RUN ghcup install ghc 9.6.4 && ghcup set ghc 9.6.4
RUN ghcup install cabal 3.10.2.1 && ghcup set cabal 3.10.2.1
RUN ghcup install alex latest && ghcup install happy latest

# Install Node.js 22 LTS
RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash -
RUN apt-get install -y nodejs

# Install TypeScript globally
RUN npm install -g typescript

# Set working directory
WORKDIR /workspace
ENV TROUPE=/workspace

# Development convenience
RUN echo 'alias ll="ls -la"' >> ~/.bashrc
RUN echo 'export TROUPE=/workspace' >> ~/.bashrc
```

**Features**:
- Complete Haskell development environment with GHCup
- Node.js 18 with TypeScript support
- Development tools and shell conveniences
- Volume mount support for live code editing

### 2. CI Fast Container (`troupe-ci-fast:latest`)

**Purpose**: Fast CI testing (core tests only, skip multinode)  
**Target Size**: 500MB-1GB  
**Target Time**: 5-10 minutes

```dockerfile
FROM ubuntu:24.04 as builder

# Install minimal build dependencies
RUN apt-get update && apt-get install -y \
    curl build-essential libffi-dev libgmp-dev && \
    rm -rf /var/lib/apt/lists/*

# Install GHCup and toolchain
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="$HOME/.ghcup/bin:$PATH"
RUN ghcup install ghc 9.6.4 && ghcup set ghc 9.6.4
RUN ghcup install cabal 3.10.2.1
RUN ghcup install alex latest && ghcup install happy latest

# Copy build configuration and source
COPY cabal.project .
COPY compiler/ compiler/
COPY Makefile .

# Build compiler with Cabal
RUN cd compiler && cabal update && cabal build all
RUN cd compiler && cabal install --install-method=copy --installdir=/workspace/bin all

# Runtime stage
FROM node:22-alpine as troupe-ci-fast

# Install minimal runtime dependencies
RUN apk add --no-cache bash diffutils

# Copy compiler binaries
COPY --from=builder /workspace/bin /app/bin
ENV PATH="/app/bin:$PATH"

# Copy runtime source
COPY rt/ /app/rt/
COPY package.json /app/
WORKDIR /app

# Build runtime
RUN npm install && npm run build

# Copy test infrastructure (core tests only)
COPY tests/rt/pos/core/ /app/tests/rt/pos/core/
COPY tests/rt/neg/core/ /app/tests/rt/neg/core/
COPY tests/cmp/ /app/tests/cmp/

ENV TROUPE=/app
```

**Features**:
- Pre-compiled Haskell binaries
- Minimal Alpine-based runtime
- Core tests only (excludes multinode tests)
- Optimized for GitHub Actions caching

### 3. CI Full Container (`troupe-ci-full:latest`)

**Purpose**: Comprehensive CI testing including multinode tests  
**Target Size**: 1-2GB  
**Target Time**: 15-25 minutes

```dockerfile
FROM troupe-ci-fast:latest as troupe-ci-full

# Add P2P networking dependencies
RUN apk add --no-cache \
    python3 py3-pip jq netcat-openbsd

# Copy P2P tools source
COPY p2p-tools/ /app/p2p-tools/
WORKDIR /app

# Build P2P tools (with libp2p v2.x)
RUN cd p2p-tools && npm install && npm run build

# Note: libp2p v2.x requires ESM modules and Node.js 22+

# Copy complete test suite
COPY tests/ /app/tests/
COPY scripts/ /app/scripts/

# Copy standard library
COPY lib/ /app/lib/
RUN make libs

# Install test dependencies
RUN pip3 install --no-cache-dir timeout-decorator

# Multinode test configuration
ENV MULTINODE_CI_MODE=true
ENV MULTINODE_SEQUENTIAL=true
ENV MULTINODE_PORT_BASE=7000
```

**Features**:
- Complete test suite including multinode tests
- P2P networking tools and relay server
- Enhanced process management for CI
- Environment variables for CI-specific behavior

### 4. Production Runtime (`troupe-runtime:latest`)

**Purpose**: Minimal production deployment container  
**Target Size**: 200-400MB

```dockerfile
FROM node:22-alpine as troupe-runtime

# Install minimal runtime dependencies
RUN apk add --no-cache bash

# Create app user for security
RUN addgroup -g 1001 -S troupe && \
    adduser -S troupe -u 1001

# Copy only production artifacts (from Cabal build)
COPY --from=builder /workspace/bin/troupec /app/bin/
COPY --from=ci-builder /app/rt/built /app/rt/built/
COPY --from=ci-builder /app/lib /app/lib/

# Set proper permissions
RUN chown -R troupe:troupe /app
USER troupe

WORKDIR /app
ENV TROUPE=/app
ENV PATH="/app/bin:$PATH"

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD troupec --version || exit 1

ENTRYPOINT ["troupec"]
```

**Features**:
- Minimal Alpine base for security
- Non-root user execution
- Health checks for container orchestration
- Only essential runtime artifacts

## Multi-Stage Build Strategy

### Optimized Dockerfile Structure

```dockerfile
# Build stages organized for maximum caching efficiency
FROM ubuntu:24.04 as base
# Common dependencies and GHCup installation

FROM base as haskell-builder  
# Haskell Cabal compilation stage

FROM node:22-alpine as nodejs-builder
# Node.js/TypeScript compilation stage

FROM nodejs-builder as ci-base
# Common CI functionality

FROM ci-base as ci-fast
# Fast CI with core tests only

FROM ci-base as ci-full  
# Full CI with multinode tests

FROM node:22-alpine as production
# Minimal production runtime
```

### Build Performance Optimization

**Layer Caching Strategy**:
```dockerfile
# Copy package files first for better caching
COPY package.json package-lock.json ./
RUN npm ci --only=production

# Copy source code last
COPY . .
RUN npm run build
```

**Parallel Build Support**:
```bash
# Build multiple stages concurrently
docker buildx build --target ci-fast --tag troupe-ci-fast:latest . &
docker buildx build --target ci-full --tag troupe-ci-full:latest . &
wait
```

## Multinode Testing Containerization

### Docker Compose for Multinode Tests

```yaml
# docker-compose.multinode.yml
version: '3.8'

services:
  relay:
    image: troupe-ci-full:latest
    command: ["node", "/app/p2p-tools/built/relay/relay.mjs", "--port", "5555"]
    networks:
      - troupe-test
    ports:
      - "5555:5555"
    healthcheck:
      test: ["CMD", "nc", "-z", "localhost", "5555"]
      interval: 5s
      timeout: 3s
      retries: 3

  client:
    image: troupe-ci-full:latest  
    command: ["./network.sh", "client.trp", "--relay", "relay:5555"]
    networks:
      - troupe-test
    depends_on:
      relay:
        condition: service_healthy
    volumes:
      - ./test-output:/app/output

  server:
    image: troupe-ci-full:latest
    command: ["./network.sh", "server.trp", "--relay", "relay:5555"]  
    networks:
      - troupe-test
    depends_on:
      relay:
        condition: service_healthy

networks:
  troupe-test:
    driver: bridge
    ipam:
      config:
        - subnet: 172.20.0.0/16

volumes:
  test-output:
```

### Container Network Isolation

**Benefits for Multinode Testing**:
- **Port Isolation**: Each test gets its own network namespace
- **Parallel Execution**: Multiple test suites can run simultaneously  
- **Clean Teardown**: Container cleanup handles all processes
- **Resource Limits**: Memory and CPU constraints per test

## CI/CD Integration

### GitHub Actions Workflow

```yaml
name: Container CI
on: [push, pull_request]

jobs:
  build-containers:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        target: [ci-fast, ci-full, production]
    
    steps:
    - uses: actions/checkout@v4
    
    - name: Set up Docker Buildx
      uses: docker/setup-buildx-action@v3
    
    - name: Cache Docker layers
      uses: actions/cache@v3
      with:
        path: /tmp/.buildx-cache
        key: ${{ runner.os }}-buildx-${{ matrix.target }}-${{ github.sha }}
        restore-keys: |
          ${{ runner.os }}-buildx-${{ matrix.target }}-
    
    - name: Build container
      uses: docker/build-push-action@v5
      with:
        context: .
        target: troupe-${{ matrix.target }}
        tags: troupe-${{ matrix.target }}:latest
        cache-from: type=local,src=/tmp/.buildx-cache
        cache-to: type=local,dest=/tmp/.buildx-cache-new,mode=max
        load: true
    
    - name: Run tests in container
      run: |
        if [ "${{ matrix.target }}" = "ci-fast" ]; then
          docker run --rm troupe-ci-fast:latest sh -c "cd compiler && cabal test"
        elif [ "${{ matrix.target }}" = "ci-full" ]; then
          docker run --rm troupe-ci-full:latest make test
        fi

  multinode-tests:
    runs-on: ubuntu-latest
    needs: build-containers
    
    steps:
    - uses: actions/checkout@v4
    
    - name: Run multinode tests with Docker Compose
      run: |
        docker-compose -f docker-compose.multinode.yml up --build --abort-on-container-exit
        docker-compose -f docker-compose.multinode.yml down
```

### Container Registry Strategy

**Image Naming Convention**:
- `ghcr.io/troupelang/troupe-dev:latest` - Development environment
- `ghcr.io/troupelang/troupe-ci-fast:latest` - Fast CI testing
- `ghcr.io/troupelang/troupe-ci-full:latest` - Full CI testing  
- `ghcr.io/troupelang/troupe-runtime:v1.0.0` - Production runtime (tagged)

**Automated Builds**:
```yaml
- name: Push to registry
  if: github.ref == 'refs/heads/master'
  uses: docker/build-push-action@v5
  with:
    push: true
    tags: |
      ghcr.io/troupelang/troupe-${{ matrix.target }}:latest
      ghcr.io/troupelang/troupe-${{ matrix.target }}:${{ github.sha }}
```

## Implementation Dependencies

### Coordination with Other Migrations

**Dependencies**:
1. **Cabal Migration** (`docs/cabal-migration.md`) - **Should complete FIRST**
   - Containers will use Cabal build system
   - GHCup toolchain management
   - Updated Makefiles with Cabal commands

2. **libp2p Upgrade** (`docs/libp2p-upgrade-plan.md`) - **Critical P2P dependency**
   - Major breaking changes: v0.45.3 → v2.8.9
   - ESM modules, TypeScript rewrite, interface consolidation
   - Affects multinode testing and P2P networking in containers

3. **Multinode Testing Strategy** (`docs/multinode-testing-ci.md`) - **Depends on libp2p upgrade**
   - Container network isolation supports multinode tests
   - Docker Compose orchestration for P2P tests

**Recommended Sequence**:
1. **Week 1**: Complete Cabal migration locally
2. **Week 2**: Complete libp2p upgrade (critical for P2P functionality)
3. **Week 3**: Implement Docker containers with updated dependencies
4. **Week 4**: Integrate multinode testing with containers

## Implementation Timeline

### Phase 1: Foundation (Week 1)
- [ ] Create multi-stage Dockerfile with modern base images (Ubuntu 24.04)
- [ ] Implement GHCup + Cabal integration (aligned with cabal-migration.md)
- [ ] Replace Stack commands with Cabal equivalents in containers
- [ ] Set up basic CI container builds in GitHub Actions
- [ ] Update security practices (remove deprecated methods)

### Phase 2: Dependencies and Upgrades (Week 2-3)
- [ ] **WAIT**: Complete libp2p upgrade (v0.45.3 → v2.8.9) before container implementation
- [ ] Update Node.js dependencies to support libp2p v2.x ESM modules
- [ ] Test P2P functionality with new libp2p version
- [ ] Update multinode test compatibility with new libp2p APIs

### Phase 3: Container Implementation (Week 4)  
- [ ] Create specialized CI containers (fast vs. full) with updated dependencies
- [ ] Implement Docker layer caching in GitHub Actions
- [ ] Test core functionality with containerized CI
- [ ] Add container health checks and monitoring

### Phase 4: Multinode Testing (Week 5)
- [ ] Create Docker Compose setup for multinode tests with libp2p v2.x
- [ ] Implement network isolation and port management
- [ ] Test parallel multinode execution in containers
- [ ] Add comprehensive cleanup and error handling

### Phase 5: Production Ready (Week 6)
- [ ] Create minimal production runtime container
- [ ] Add security scanning and vulnerability management
- [ ] Implement container signing and verification
- [ ] Document deployment procedures

## File Structure

```
/Users/aslan/Prime/Troupe/
├── docker/
│   ├── Dockerfile                    # Multi-stage build
│   ├── docker-compose.yml            # Local development
│   ├── docker-compose.multinode.yml  # Multinode testing
│   ├── .dockerignore                 # Build context optimization
│   └── README.md                     # Container usage guide
├── .github/
│   └── workflows/
│       ├── container-ci.yml          # Container builds and tests
│       └── container-publish.yml     # Registry publishing
└── scripts/
    ├── docker-build.sh               # Container build automation
    ├── docker-test.sh                # Containerized test runner
    └── docker-clean.sh               # Development cleanup
```

## Performance and Resource Targets

### Build Performance
- **Container Build Time**: < 15 minutes with caching
- **CI Fast Tests**: < 10 minutes total
- **CI Full Tests**: < 25 minutes total
- **Cache Hit Rate**: > 80% for unchanged code

### Resource Usage
- **Development Container**: 2-3GB disk, 2GB RAM
- **CI Fast Container**: 500MB-1GB disk, 1GB RAM  
- **CI Full Container**: 1-2GB disk, 2GB RAM
- **Production Container**: 200-400MB disk, 512MB RAM

### Security Requirements
- **Base Image Updates**: Weekly automated security patches
- **Vulnerability Scanning**: Zero high/critical vulnerabilities
- **Non-root Execution**: All production containers run as non-root
- **Minimal Attack Surface**: Only essential dependencies in production

## Monitoring and Maintenance

### Health Checks
```dockerfile
# Example health check for runtime container
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD troupec --version && node -e "console.log('Runtime OK')" || exit 1
```

### Logging Strategy  
```bash
# Structured logging for container orchestration
docker run --log-driver=json-file --log-opt max-size=10m --log-opt max-file=3 \
    troupe-runtime:latest
```

### Cleanup Automation
```bash
# Automated cleanup script
#!/bin/bash
# Clean up unused containers and images
docker system prune -f
docker image prune -f --filter="label!=keep"
```

## Migration Path

### From Current Setup
1. **Parallel Development**: Build new containers alongside existing setup
2. **Gradual Migration**: Start with CI-fast, then CI-full, finally production
3. **Validation**: Extensive testing of containerized vs. native builds  
4. **Rollback Plan**: Keep existing Dockerfile as backup during transition

### Risk Mitigation
- **Feature Flags**: CI environment variables to switch between native/container
- **Performance Monitoring**: Track build times and success rates
- **Gradual Rollout**: Enable containers for specific branches first

---

**Document Owner**: TBD  
**Last Updated**: 2025-06-22  
**Implementation Status**: Planning Phase