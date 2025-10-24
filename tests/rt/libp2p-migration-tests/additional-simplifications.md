# Additional libp2p Test Simplifications

## Summary
Now that we've implemented NetHealth library, here are additional simplifications we can make to reduce complexity while maintaining test coverage.

## 1. Test Consolidation

### Combine whereis-blocking and whereis-timeout tests
These two tests demonstrate related concepts and can be merged into a single "whereis-behavior" test:
- One test file that demonstrates both blocking behavior AND timeout pattern
- Reduces from 4 files to 2 files
- Maintains full coverage of whereis edge cases

### Simplify direct-connection test
The current test has redundant message exchanges:
- The "GET_TIMING" exchange is unnecessary - bidirectional communication is already proven by CONNECTION_ACK
- Can reduce from 2 round-trips to 1 round-trip

## 2. Message Pattern Simplification

### async-stream-ops test
Currently expects exactly 5 messages. Can simplify to 3 core tests:
- Small message (basic async)
- Large message (chunking behavior)  
- Concurrent messages (multiplexing)
This reduces complexity while maintaining coverage of critical v0.46+ features.

## 3. Code Simplifications

### Remove duplicate performance reporting
Some tests report the same metric twice (e.g., connection time). NetHealth already handles this consistently.

### Standardize sleep patterns
Replace arbitrary sleep values with named constants:
```troupe
val STARTUP_DELAY = 1000  (* Give nodes time to register *)
val MESSAGE_DELIVERY_DELAY = 1000  (* Ensure messages are delivered *)
```

### Simplify test result checking
Many tests use verbose if/else patterns that can be simplified:
```troupe
(* Current *)
in if result = "success" then
    exitSuccess(config, "SERVER", "Test completed successfully", 1000)
   else
    exitFailure(config, "SERVER", "Test failed")
end

(* Simplified *)
in exitOnResult(config, "SERVER", result = "success", "Test completed", 1000)
end
```

## 4. Configuration Standardization

### Create config templates
Many config.json files are nearly identical. Create a template:
```json
{
  "test_name": "p2p-${TEST_NAME}",
  "timeout": 30,
  "coordination": "parallel",
  "network": {
    "use_relay": false
  },
  "nodes": [
    {
      "id": "client",
      "script": "client.trp",
      "port": 6789,
      "expected_exit_code": 0
    },
    {
      "id": "server", 
      "script": "server.trp",
      "port": 6790,
      "expected_exit_code": 0
    }
  ]
}
```

## 5. NetHealth Library Extensions

Add these utility functions to NetHealth:
```troupe
(* Single exit point based on boolean *)
fun exitOnResult({print, auth, ..}, nodeName, success, message, delayMs) =
    if success then
        exitSuccess({print = print, auth = auth}, nodeName, message ^ " successfully", delayMs)
    else
        exitFailure({print = print, auth = auth}, nodeName, message ^ " failed")

(* Standard delays *)
val STARTUP_DELAY = 1000
val MESSAGE_DELIVERY_DELAY = 1000
```

## 6. Remove Unnecessary Tests

The peerId-generation test doesn't actually test PeerId generation - it just tests basic message passing which is covered by other tests. This test should be removed or completely rewritten to actually test identity-related functionality.

## Benefits

1. **Reduced Files**: Consolidation reduces test count from 8 files to ~4-5 files
2. **Clearer Intent**: Each test focuses on one specific libp2p behavior
3. **Less Duplication**: Common patterns extracted to NetHealth
4. **Faster Execution**: Fewer redundant message exchanges
5. **Easier Maintenance**: Simpler code is easier to update during migration

## Implementation Priority

1. **High**: Consolidate whereis tests (biggest reduction in complexity)
2. **Medium**: Simplify message patterns in existing tests
3. **Low**: Configuration templates and minor code cleanups