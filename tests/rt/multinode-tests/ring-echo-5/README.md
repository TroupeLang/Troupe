# Ring Echo with 5 Intermediaries

Extended version of ring-echo with 5 intermediary nodes.

## Overview

- **Pattern**: Client → Intermediary1 → Intermediary2 → Intermediary3 → Intermediary4 → Intermediary5 → Server → Client
- Messages flow through all 5 intermediaries sequentially before reaching the server
- Server responds directly to the client

See ring-echo for the basic pattern.

## Key Configuration Options

- **start_delay**:
  - All intermediaries: 1 second - Concurrent startup
  - Client: 5 seconds - Ensures complete chain formation
- **ports**: 6789-6795 - Seven unique ports required
- **expected_exit_code**: 124 for all 5 intermediaries - Continuous proxy operation
- **timeout**: 120 seconds - Extended chain requires more time
- **sleep delays**: Client sleeps 1.5 seconds before its first lookup, for chain stability