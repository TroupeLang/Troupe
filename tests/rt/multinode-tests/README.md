# Troupe Multi-Node Testing

This directory contains multi-node networking tests for Troupe. These tests validate distributed functionality including P2P communication, service discovery, process spawning, and network reliability.

## Architecture

The multi-node testing system consists of:

1. **Test Orchestrator** (`scripts/multinode-runner.sh`) - Manages node coordination, network setup, and output synchronization
2. **Test Runner** (`scripts/run-multinode-tests.sh`) - Executes all tests and provides summary results  
3. **Test Cases** - Individual multi-node scenarios with finite execution
4. **Configuration** - JSON files defining node behavior and expectations

## Test Structure

Each test lives in its own directory with:
- `config.json` - Test configuration (nodes, timeouts, network setup)
- `*.trp` - Troupe source files for each node

The oracle is exit codes; no golden output file is read (see "Test Assertions" below).

### Configuration Format

```json
{
  "test_name": "test-name",
  "timeout": 30,
  "coordination": "parallel|sequential",
  "network": {
    "relay_port": 5555,
    "use_relay": "relay-only|no-relay|both"
  },
  "nodes": [
    {
      "id": "node1",
      "script": "node1.trp",
      "port": 6789,
      "start_delay": 0,
      "expected_exit_code": 0,
      "extra_argv": "--rspawn"
    }
  ],
  "trust": [ { "from": "node1", "to": "node2", "level": "bot" } ],
  "output": { "merge_strategy": "timestamp", "filter_patterns": ["uuid", "timestamp", "peer_id"] }
}
```

`trust` generates each node's `trustmaps/<id>-trustmap.json` and adds `--trustmap` to its command
line; a test that needs no trust map sets it to `null`. `extra_argv` is appended to that node's
`network.sh` invocation. `output.merge_strategy` controls how the per-node logs are interleaved.

#### `use_relay` options:
- `"relay-only"`: Run test with relay server (default behavior)
- `"no-relay"`: Run test without relay server
- `"both"`: Run test twice - once with relay, once without. Output shows separate test names with suffixes: `test-name (relay)` and `test-name (no-relay)`

## Running Tests

### All Tests
```bash
scripts/run-multinode-tests.sh
```

### Specific Pattern
```bash
scripts/run-multinode-tests.sh -p echo
```

### Single Test
```bash
scripts/multinode-runner.sh tests/rt/multinode-tests/basic-echo/config.json
```

### Verbose Output
```bash
scripts/run-multinode-tests.sh -v
```

## Test Categories

### Basic Connectivity
- **basic-echo** - Simple client-server message exchange
- **multi-client** - One server handling multiple clients

### Advanced Patterns
- **cross-spawn** - Cross-node process spawning
- **ring-echo**, **ring-echo-3**, **ring-echo-5** - Forwarding through 1, 3 and 5 intermediaries
- **synvar-exchange** - Independently compiled identical datatype declarations across nodes
- **quarantine-echo** - Echo under a non-bottom trust level
- **module-closure-net-missing**, **module-spawn-net-missing** - Receiver lacks an imported module
- **trust-flow-issue-42** - Illegal trust flow on remote spawn

## Key Design Principles

### Finite Execution
Unlike the indefinite server examples, all tests have bounded execution:
- Handle specific number of messages/requests
- Use timeouts to prevent hanging
- Clear success/failure exit codes

### Network Behavior Testing
Tests validate:
- Node discovery and connection
- Message delivery and ordering
- Service registration/lookup
- Error handling and recovery
- Trust and security constraints

### Deterministic Output
- Filter timestamps, UUIDs, and peer IDs
- Merge outputs by timestamp or sequence
- Provide expected output patterns

## Writing New Tests

1. Create test directory: `tests/rt/multinode-tests/new-test/`
2. Write Troupe scripts with finite execution patterns
3. Create `config.json` with node specifications
4. Test manually: `scripts/multinode-runner.sh tests/rt/multinode-tests/new-test/config.json`
5. Add expected output if needed

### Test Template

```troupe
(* Test Template *)
let 
    val timeout_pid = spawn(fn () => 
        let val _ = sleep 25000
        in exit(authority, 124) (* Timeout *)
        end)
    
    fun test_function() = 
        let (* Test logic here *)
        in if success_condition then
            (print "TEST: Success"; exit(authority, 0))
           else  
            (print "TEST: Failed"; exit(authority, 1))
        end

in test_function()
end
```

## CI/CD Integration

Tests can be integrated into CI/CD pipelines:

```yaml
- name: Multi-node tests
  run: |
    make rt  # Build runtime
    scripts/run-multinode-tests.sh
```

## Debugging

### Verbose Mode
Use `-v` flag to see detailed orchestration logs:
```bash
scripts/multinode-runner.sh -v tests/rt/multinode-tests/basic-echo/config.json
```

### Manual Node Testing
Start nodes manually for debugging:
```bash
cd tests/rt/multinode-tests/basic-echo
$TROUPE/network.sh echo-server.trp --id ids/server.json --aliases aliases.json --port 6789
```

### Log Analysis
Each test run creates temporary output files for each node in `/tmp/troupe-multinode-*/output/`

## Troubleshooting

### Tests fail in VSCode but pass in Terminal (macOS)

On macOS Sequoia and later, multinode tests may fail when run from VSCode's integrated terminal but work fine in a regular Terminal. Symptoms include:
- Nodes start but cannot discover each other
- Client prints "Starting echo client" but never finds the server
- No `libp2p:mdns peer found` messages in debug output

**Cause:** macOS Sequoia introduced stricter privacy controls. VSCode needs explicit permission to access the local network, which is required for mDNS peer discovery.

**Solution:** Grant VSCode local network access:
1. Open **System Settings → Privacy & Security → Local Network**
2. Find **Visual Studio Code** and enable the toggle
3. Restart VSCode

If VSCode doesn't appear in the list, try running a multinode test first to trigger the permission prompt, then check the settings again.

**Reference:** [VSCode Issue #228862](https://github.com/microsoft/vscode/issues/228862)

## Limitations

- Tests require finite execution patterns
- Network timing can cause non-deterministic results
- Relay server must be available for NAT traversal
- P2P identity generation adds setup overhead

## Future Enhancements

- Docker-based node isolation
- Network condition simulation (latency, packet loss)
- Chaos engineering tests (random failures)
- Performance benchmarking
- Integration with golden test system, but right now we do not create .golden files for these tests.