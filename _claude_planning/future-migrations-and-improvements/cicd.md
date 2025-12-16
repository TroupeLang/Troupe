# CI/CD Pipeline Documentation

This document outlines the CI/CD strategy and implementation for the Troupe project.

## Current State

### Existing CI/CD Infrastructure

The project already has basic CI/CD infrastructure in place:

- **GitHub Actions Workflows**:
  - `run_tests.yml` - Comprehensive test execution on Ubuntu 22.04
  - `build_docker_image.yml` - Docker image building and publishing
- **Docker Support**: Multi-stage Dockerfile with Haskell build environment
- **Test Infrastructure**: Golden test framework with 300+ tests across multiple categories

### Test Categories

1. **Compiler Tests** (`/tests/cmp/`) - Negative compilation tests
2. **Runtime Tests** (`/tests/rt/`):
   - Core language features (`pos/core/`, `neg/core/`)
   - Information flow control (`pos/ifc/`, `neg/ifc/`)
   - Timeout and warning scenarios
   - **Multinode Tests** - Complex distributed system tests requiring P2P networking

### Build Process

The project uses a multi-stage build process:
1. Haskell compiler build (`make stack`)
2. TypeScript runtime compilation (`make rt`)
3. Standard library compilation (`make libs`)
4. Service module compilation (`make service`)
5. P2P tools compilation (`make p2p-tools`)

## Proposed CI/CD Strategy

### Overview

Multi-tiered CI/CD approach balancing fast feedback with comprehensive testing:

### Workflow Design

#### Trigger Strategy
- **Push to any branch**: Fast CI (core tests only)
- **Pull requests to master**: Comprehensive CI (all tests including multinode)
- **Scheduled runs**: Weekly comprehensive testing for environmental drift detection
- **Release tags**: Full CI + Docker image building + deployment
- **Manual dispatch**: On-demand testing for debugging and special cases

#### Workflow Tiers

**Fast CI** (5-10 minutes):
- Compiler tests
- Core runtime tests
- Basic integration tests
- Skip multinode tests for speed

**Comprehensive CI** (20-30 minutes):
- All Fast CI tests
- Multinode distributed tests
- Full integration test suite
- Performance regression checks

**Release CI** (30-45 minutes):
- All Comprehensive CI tests
- Docker image building
- Security scanning
- Production deployment preparation

### Docker Strategy

[TO BE DETERMINED]

### Testing Strategy

[TO BE DETERMINED]

## Implementation Plan

### Phase 1: Foundation
- [ ] Update existing Docker configuration
- [ ] Optimize build process for CI environments
- [ ] Set up basic CI workflow

### Phase 2: Comprehensive Testing
- [ ] Implement multinode testing in CI
- [ ] Set up test result reporting
- [ ] Add performance monitoring

### Phase 3: Advanced Features
- [ ] Implement deployment strategies
- [ ] Add security scanning
- [ ] Set up monitoring and alerting

## Technical Requirements

### System Dependencies
- **Haskell Stack** with GHC 9.2.7
- **Node.js 18+** with TypeScript
- **System packages**: build-essential, libnuma-dev, diffutils, jq
- **Python 3.8+** for tooling support

### CI Environment Considerations
- **Network Access**: Required for P2P multinode tests
- **Process Management**: Multinode tests spawn multiple processes
- **Resource Requirements**: Tests can be memory and CPU intensive
- **Timeout Handling**: Some tests run up to 60 seconds

## Security Considerations

- P2P networking components need proper isolation
- Multinode tests require careful cleanup of spawned processes
- Network ports and relay servers need proper management
- Container security for production deployments

## Performance Considerations

- Full test suite includes 300+ individual tests
- Compiler rebuild required for Haskell changes
- Multinode tests are time-intensive
- Build process can take several minutes

## Questions for Resolution

1. **CI Triggers**: What events should trigger CI runs?
2. **Test Strategy**: How to balance speed vs. thoroughness?
3. **Platform Support**: Which operating systems to support?
4. **Docker Strategy**: Development vs. production container needs?
5. **Performance vs. Thoroughness**: Fast feedback vs. comprehensive testing?

---

*This document will be updated as requirements are clarified and implementation progresses.*