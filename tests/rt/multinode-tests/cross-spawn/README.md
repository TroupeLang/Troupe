# Cross-Spawn Test

This test demonstrates remote process spawning capabilities in Troupe.

## Overview

- **spawner**: Registers a "coordinator" service under its own name, then spawns a task on `@target`
- **target**: Hosts the spawned task; it runs with `--rspawn` and otherwise only waits
- **Pattern**: spawner spawns on target → the remote task looks up "coordinator" on `@spawner` →
  sends `TASK_COMPLETE` back

The test verifies that processes can be spawned across node boundaries with proper authority delegation.

## Key Configuration Options

- **coordination**: "parallel" - Nodes start together for spawn timing
- **start_delay**: 1 second for the spawner; the target has none
- **timeout**: 45 seconds - Accounts for remote spawn overhead
- **expected_exit_code**: 0 for both nodes