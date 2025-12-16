# Multinode Testing Strategy for CI/CD

This document outlines the strategy for running Troupe's multinode tests reliably in CI/CD environments.

## Overview

Troupe's multinode tests validate distributed actor communication using P2P networking. These tests are complex, requiring process orchestration, network coordination, and careful resource management.

**Current State**: Tests run locally with `./scripts/run-multinode-tests.sh`  
**Target State**: Reliable execution in GitHub Actions CI environment  
**Key Challenge**: Network isolation, port management, and process cleanup in CI

## Multinode Test Architecture

### Test Structure

Each multinode test consists of:
- **Configuration** (`config.json`) - Defines nodes, timeouts, network setup
- **Scripts** (`*.trp`) - Troupe source files for each node role  
- **Identity System** - P2P keys and aliases for node communication
- **Orchestration** - Coordinated startup and message passing

### Infrastructure Components

1. **P2P Relay Server** - Enables NAT traversal using libp2p circuit relay
2. **Node Processes** - Individual Troupe runtime instances  
3. **Process Coordination** - Sequential or parallel node startup
4. **Output Management** - Merged and filtered test outputs

## CI/CD Challenges and Solutions

### Challenge 1: Port Conflicts

**Problem**: Multiple tests require unique P2P ports, potential conflicts in parallel execution

**Solution - Port Pool Strategy**:
```bash
# Reserve port ranges for CI
BASE_PORT=7000
RELAY_PORT_BASE=5000
MAX_CONCURRENT_TESTS=5

# Calculate ports per test
get_test_ports() {
    local test_index=$1
    local relay_port=$((RELAY_PORT_BASE + test_index))
    local node_port_base=$((BASE_PORT + (test_index * 10)))
    echo "$relay_port $node_port_base"
}
```

### Challenge 2: Process Cleanup

**Problem**: Background processes (relay servers, nodes) can persist if tests fail

**Solution - Enhanced Cleanup Strategy**:
```bash
# Global process tracking
declare -a BACKGROUND_PIDS=()
declare -a TEMP_DIRS=()

cleanup_ci() {
    echo "CI Cleanup: Killing background processes"
    for pid in "${BACKGROUND_PIDS[@]}"; do
        kill -TERM "$pid" 2>/dev/null || true
        sleep 1
        kill -KILL "$pid" 2>/dev/null || true
    done
    
    # Clean temporary directories
    for dir in "${TEMP_DIRS[@]}"; do
        rm -rf "$dir" 2>/dev/null || true
    done
    
    # Kill any remaining Troupe processes
    pkill -f "troupe.*multinode" || true
    pkill -f "relay.*multinode" || true
}

# Set trap for all exit conditions
trap cleanup_ci EXIT INT TERM
```

### Challenge 3: Network Reliability

**Problem**: P2P networking can be flaky in containerized CI environments

**Solution - Robust Network Strategy**:
```json
{
  "network": {
    "relay_port": "dynamic",
    "connection_timeout": 30,
    "retry_attempts": 3,
    "local_discovery": true,
    "relay_fallback": true
  }
}
```

### Challenge 4: Test Isolation

**Problem**: Tests can interfere with each other through shared resources

**Solution - Isolation Strategy**:
- **Sequential Execution**: Run multinode tests one at a time
- **Unique Workspaces**: Temporary directories per test with unique names
- **Resource Cleanup**: Comprehensive cleanup between tests

## Implementation Strategy

### Phase 1: Sequential CI Integration (Low Risk)

**Approach**: Run multinode tests sequentially with enhanced cleanup

```yaml
# GitHub Actions workflow
- name: Run multinode tests (sequential)
  run: |
    # Set strict error handling
    set -euo pipefail
    
    # Run tests with enhanced logging
    ./scripts/run-multinode-tests.sh -v
  timeout-minutes: 30
  env:
    MULTINODE_CI_MODE: true
    MULTINODE_SEQUENTIAL: true
```

**Benefits**:
- Minimal changes to existing test infrastructure
- Reduces port conflicts and resource contention
- Easier debugging and log analysis

**Drawbacks**:
- Longer CI times (sequential execution)
- May not scale with more tests

### Phase 2: Parallel CI with Resource Management (Medium Risk)

**Approach**: Enable parallel execution with careful resource allocation

```yaml
# Matrix strategy for parallel execution
strategy:
  matrix:
    test-group: [group1, group2, group3]
    
- name: Run multinode test group
  run: |
    # Run subset of tests in parallel
    ./scripts/run-multinode-tests.sh -p "group${{ matrix.test-group }}"
  env:
    MULTINODE_PORT_BASE: ${{ 6000 + (strategy.job-index * 100) }}
```

### Phase 3: Containerized Testing (Advanced)

**Approach**: Use Docker containers for complete isolation

```dockerfile
# Multinode test container
FROM troupe-base:latest

# Install network tools
RUN apt-get update && apt-get install -y \
    iproute2 netcat-openbsd

# Create isolated network namespace  
RUN ip netns add troupe-test

# Copy test infrastructure
COPY scripts/ scripts/
COPY tests/rt/multinode-tests/ tests/rt/multinode-tests/

ENTRYPOINT ["./scripts/run-multinode-tests.sh"]
```

## Recommended Implementation Plan

### Step 1: Enhanced Sequential Testing (Week 1)

1. **Update CI Workflow**:
   ```yaml
   - name: Build all components
     run: |
       make stack
       make rt  
       make libs
       make service
       make p2p-tools
   
   - name: Run core tests
     run: make test-core  # Exclude multinode
   
   - name: Run multinode tests  
     run: ./scripts/run-multinode-tests.sh -v
     timeout-minutes: 20
   ```

2. **Enhance Cleanup Scripts**:
   - Add comprehensive process tracking
   - Implement timeout-based cleanup
   - Add CI-specific logging

3. **Port Management**:
   - Use dynamic port allocation
   - Add port conflict detection
   - Reserve CI port ranges

### Step 2: Test Reliability Improvements (Week 2)

1. **Timeout Management**:
   ```bash
   # Per-test timeout with grace period
   GLOBAL_TIMEOUT=300  # 5 minutes max per test
   GRACE_PERIOD=30     # 30 second cleanup grace
   ```

2. **Retry Mechanism**:
   ```bash
   # Retry flaky tests once
   run_test_with_retry() {
       local test_name=$1
       if ! run_single_test "$test_name"; then
           echo "Retrying $test_name..."
           sleep 5
           run_single_test "$test_name"
       fi
   }
   ```

3. **Resource Monitoring**:
   - Add memory usage monitoring
   - Track process counts
   - Log resource cleanup

### Step 3: Performance Optimization (Week 3)

1. **Caching Strategy**:
   ```yaml
   - name: Cache multinode dependencies
     uses: actions/cache@v3
     with:
       path: |
         bin/
         rt/built/
         p2p-tools/built/
       key: ${{ runner.os }}-multinode-${{ hashFiles('**/package.json', 'compiler/**') }}
   ```

2. **Build Optimization**:
   - Pre-compile runtime and P2P tools
   - Cache P2P identity generation
   - Optimize relay server startup

## Configuration Updates

### Enhanced config.json Format

```json
{
  "test_name": "enhanced-test",
  "timeout": 60,
  "coordination": "parallel",
  "ci_settings": {
    "max_retries": 1,
    "cleanup_timeout": 30,
    "resource_limits": {
      "max_memory_mb": 512,
      "max_processes": 10
    }
  },
  "network": {
    "relay_port": "auto",
    "port_range": "auto", 
    "connection_timeout": 15,
    "discovery_timeout": 10
  },
  "nodes": [
    {
      "id": "client",
      "script": "client.trp",
      "port": "auto",
      "start_delay": 2,
      "expected_exit_code": 0,
      "resource_limits": {
        "timeout": 30,
        "memory_mb": 256
      }
    }
  ]
}
```

### CI-Specific Environment Variables

```bash
# CI configuration
export MULTINODE_CI_MODE=true
export MULTINODE_SEQUENTIAL=true
export MULTINODE_VERBOSE=true
export MULTINODE_PORT_BASE=7000
export MULTINODE_TIMEOUT_SCALE=1.5  # 50% longer timeouts in CI
```

## Monitoring and Debugging

### Test Result Reporting

```bash
# Enhanced test reporting
generate_ci_report() {
    local total_tests=$1
    local passed_tests=$2
    local failed_tests=$3
    
    echo "=== Multinode Test Results ==="
    echo "Total Tests: $total_tests"
    echo "Passed: $passed_tests"
    echo "Failed: $failed_tests"
    echo "Success Rate: $(( passed_tests * 100 / total_tests ))%"
    
    # Output GitHub Actions summary
    cat >> "$GITHUB_STEP_SUMMARY" << EOF
## Multinode Test Results
- **Total Tests**: $total_tests
- **Passed**: $passed_tests  
- **Failed**: $failed_tests
- **Success Rate**: $(( passed_tests * 100 / total_tests ))%
EOF
}
```

### Debug Information Collection

```bash
# Collect debug info on failure
collect_debug_info() {
    local test_name=$1
    local debug_dir="debug-$test_name-$(date +%s)"
    
    mkdir -p "$debug_dir"
    
    # System state
    ps aux > "$debug_dir/processes.txt"
    netstat -tulpn > "$debug_dir/network.txt"
    df -h > "$debug_dir/disk.txt"
    
    # Test artifacts
    cp -r "tests/rt/multinode-tests/$test_name/ids" "$debug_dir/" 2>/dev/null || true
    cp "tests/rt/multinode-tests/$test_name/config.json" "$debug_dir/" 2>/dev/null || true
    
    # Logs
    journalctl --since="5 minutes ago" > "$debug_dir/system.log" 2>/dev/null || true
    
    echo "Debug info collected in $debug_dir"
}
```

## Success Metrics

### Performance Targets
- **Total multinode test time**: < 15 minutes
- **Individual test timeout**: < 2 minutes  
- **Test reliability**: > 95% success rate
- **Resource cleanup**: 100% process cleanup

### Quality Gates
- All multinode tests pass on every PR
- No resource leaks (processes, ports, files)
- Deterministic test results
- Clear failure diagnostics

## Risk Assessment

### Low Risk Items
- Sequential test execution
- Enhanced cleanup scripts
- Timeout improvements

### Medium Risk Items  
- Parallel test execution
- Dynamic port allocation
- Network reliability improvements

### High Risk Items
- Container-based isolation
- Cross-platform multinode testing
- Advanced resource management

## Next Steps

1. **Immediate (Week 1)**: Implement sequential CI integration
2. **Short-term (Month 1)**: Add reliability improvements and monitoring
3. **Long-term (Quarter 1)**: Explore parallel execution and containerization

---

**Document Owner**: TBD  
**Last Updated**: 2025-06-22  
**Status**: Draft - Awaiting Implementation