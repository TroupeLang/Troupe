# libp2p Migration Tests

This directory contains specialized tests for validating the libp2p upgrade from v0.45.3 to v2.x. These tests focus on low-level P2P functionality that is not covered by the existing multinode tests.

## Overview

The libp2p v2 upgrade involves significant architectural changes:
- Migration to ESM-only modules
- Complete TypeScript rewrite
- New PeerId architecture (no embedded private keys)
- Async stream operations
- New error handling (.code → .name)
- Service extraction to separate packages

These tests ensure that all P2P functionality works correctly after the upgrade.

## Test Categories

### 1. P2P Connection Tests (`p2p-connection-tests/`)
Tests for connection lifecycle, direct connections without relay, whereis blocking behavior, and transport fallback mechanisms.

**CRITICAL**: The `whereis` operation in Troupe blocks INDEFINITELY with NO built-in timeout. It will wait forever until the service is found. This has major implications:
- Tests must implement their own timeout mechanisms
- A whereis call to a non-existent service will hang forever
- Connection tests must carefully manage this blocking behavior

### 2. P2P Stream Tests (`p2p-stream-tests/`)
Tests for async stream operations (critical for v0.46+), stream lifecycle management, and concurrent stream handling.

### 3. P2P Identity Tests (`p2p-identity-tests/`)
Tests for PeerId generation with new crypto APIs, identity persistence, and peer discovery with new formats.

### 4. P2P Error Tests (`p2p-error-tests/`)
Tests for error type mapping, network error handling, and graceful degradation scenarios.

### 5. P2P Performance Tests (`p2p-performance-tests/`)
Performance benchmarks for connection establishment, message throughput, and memory usage.

### 6. P2P Migration Tests (`p2p-migration-tests/`)
Tests for version compatibility, feature flags, and rollback scenarios.

### 7. Test Utilities (`test-utils/`)
Shared helper functions and utilities for P2P testing.

## Running Tests

### Run all libp2p migration tests:
```bash
# From the Troupe root directory
for test in tests/rt/libp2p-migration-tests/p2p-*/*/config.json; do
    scripts/multinode-runner.sh "$test"
done
```

### Run a specific test category:
```bash
# Run all connection tests
for test in tests/rt/libp2p-migration-tests/p2p-connection-tests/*/config.json; do
    scripts/multinode-runner.sh "$test"
done
```

### Run a single test:
```bash
scripts/multinode-runner.sh tests/rt/libp2p-migration-tests/p2p-connection-tests/direct-connection/config.json
```

### Run with verbose P2P debugging:
```bash
scripts/multinode-runner.sh -v tests/rt/libp2p-migration-tests/p2p-connection-tests/direct-connection/config.json
```

## Test Development Guidelines

1. **Finite Execution**: All tests must have bounded execution time and clear exit conditions.

2. **Blocking Operations**: `whereis` blocks INDEFINITELY with no timeout. It will wait forever for a service. This means:
   - Never call `whereis` for a service that might not exist
   - Always ensure services are registered before calling `whereis`
   - Consider spawning `whereis` in a separate process with your own timeout
   - The test harness timeout is your only protection against hanging

3. **Error Handling**: Tests should handle both expected and unexpected errors gracefully.

3. **Diagnostics**: Use the `--debug-p2p` flag in extra_argv for detailed P2P logging.

4. **Performance Metrics**: Capture timing data where relevant for regression detection.

5. **Baseline Comparison**: Where possible, run the same test on both v0.45.3 and v2.x for comparison.

## Test Configuration

Tests use the standard multinode test configuration format with some P2P-specific options:

```json
{
  "test_name": "test-name",
  "timeout": 30,
  "network": {
    "use_relay": false,      // Set to false for direct connection tests
    "relay_port": 5555,      // Only used if use_relay is true
    "bootstrap_peers": []    // Control peer discovery
  },
  "nodes": [
    {
      "id": "node-id",
      "script": "script.trp",
      "port": 6789,
      "expected_exit_code": 0,
      "extra_argv": "--debug-p2p"  // Enable P2P debugging
    }
  ]
}
```

## Test Development Priority

1. **High Priority** (Must have before migration):
   - Connection tests (fundamental P2P operations)
   - Identity tests (core system changes)
   - Stream async operations (v0.46+ compatibility)
   - Error type mapping (prevents runtime failures)

2. **Medium Priority** (Should have):
   - Performance benchmarks
   - Circuit relay v2 tests
   - Multi-transport tests
   - DHT operations

3. **Low Priority** (Nice to have):
   - Load tests
   - Network simulation
   - Extended migration scenarios

## Adding New Tests

1. Create a new directory under the appropriate category
2. Add a `config.json` file with test configuration
3. Create `.trp` files for each node in the test
4. Optionally add an `expected.golden` file for output validation
5. Document the test purpose and what it validates

## Debugging Tips

- Use `--debug-p2p` flag to enable libp2p debug logging
- Check `/tmp/troupe-multinode-*/output/` for detailed node outputs
- Use `print` statements liberally to track test progress
- Monitor relay output when debugging connection issues

## Known Limitations

- Tests cannot directly inspect P2P internal state
- Version compatibility tests may be limited by breaking changes
- Performance measurements may vary based on system load
- Some edge cases may be difficult to reproduce reliably