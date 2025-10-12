# libp2p Migration Execution Plan

## Context
This document contains the complete plan for migrating Troupe from libp2p v0.45.3 to v2.10.0. This is a COMPLEX migration with significant breaking changes in how P2P connectivity works, particularly circuit relay functionality.

## ⚠️ CRITICAL WARNING
**The multinode tests MUST pass for the migration to be considered successful.** Local execution working is NOT a validation of P2P functionality - they are completely orthogonal. ALL multinode tests must work.

## Current Status (After Initial Attempt)
- ✅ Basic runtime builds and compiles
- ✅ Local (non-P2P) tests pass
- ❌ **Multinode tests fail - nodes cannot find each other through relay**
- ❌ **Circuit relay v2 connectivity broken**
- ❌ **Whereis functionality times out**

## Pre-Migration Checklist
- [ ] Working on branch: `libp2p-v2-migration` (not `ir-inliner`)
- [ ] Current libp2p version: 0.45.3
- [ ] Target libp2p version: 2.10.0
- [ ] **Understand that this is a MAJOR breaking change requiring significant refactoring**

## Key Files That Need Changes

### Main Runtime Files
1. `/Users/aslan/Prime/Troupe/package.json` - Main dependencies
2. `/Users/aslan/Prime/Troupe/rt/src/p2p/p2p.mts` - Core P2P implementation (700+ lines)
3. `/Users/aslan/Prime/Troupe/p2p-tools/mkid.mts` - Key generation tool
4. `/Users/aslan/Prime/Troupe/p2p-tools/relay/relay.mjs` - Relay server

### Test Files (for validation)
- `/Users/aslan/Prime/Troupe/tests/rt/multinode-tests/` - Must ALL pass
- `/Users/aslan/Prime/Troupe/tests/rt/libp2p-migration-tests/` - Specific migration tests

## Phase 1: Incremental Migration v0.45 → v0.46

### 1.1 Update Dependencies
In `/Users/aslan/Prime/Troupe/package.json`:
```json
"libp2p": "^0.46.0"
```

### 1.2 Code Changes
- No stream closing operations found in codebase
- No stat property access found
- v0.46 changes are minimal

### 1.3 Test
```bash
make rt
./scripts/multinode-runner.sh tests/rt/multinode-tests/basic-echo/config.json
```

## Phase 2: Major Breaking Changes v0.46 → v1.0.0

### 2.1 Update Dependencies
In `package.json`:
```json
"libp2p": "^1.0.0",
"@libp2p/bootstrap": "^9.0.0",
"@libp2p/kad-dht": "^10.0.0",
"@libp2p/mdns": "^9.0.0",
"@libp2p/tcp": "^8.0.0",
"@libp2p/websockets": "^7.0.0",
"@libp2p/crypto": "^2.0.0",
"@libp2p/peer-id": "^3.0.0",
"@libp2p/identify": "^1.0.0",
"@libp2p/circuit-relay-v2": "^1.0.0",
"@libp2p/interface": "^1.0.0"
```

### 2.2 Critical Code Changes

#### Update Imports (p2p.mts)
```typescript
// OLD
import { createFromJSON, createEd25519PeerId } from '@libp2p/peer-id-factory';
import { identifyService } from 'libp2p/identify';
import { circuitRelayTransport } from 'libp2p/circuit-relay';

// NEW
import { generateKeyPair, unmarshalPrivateKey } from '@libp2p/crypto/keys';
import { peerIdFromPrivateKey } from '@libp2p/peer-id';
import type { PeerId } from '@libp2p/interface';
import { identify } from '@libp2p/identify';
import { circuitRelayTransport } from '@libp2p/circuit-relay-v2';
```

#### Replace PeerId Handling (p2p.mts ~line 264)
```typescript
// Replace entire obtainPeerId function with:
async function obtainPrivateKey(nodeId): Promise<any> {    
  let privateKey: any = null;
  if(nodeId && nodeId.privKey) {
    try {
      // IMPORTANT: Convert base64pad string to Uint8Array
      const privKeyBytes = Uint8Array.from(Buffer.from(nodeId.privKey.trim(), 'base64'));
      privateKey = await unmarshalPrivateKey(privKeyBytes);
      const id = await peerIdFromPrivateKey(privateKey);
      debug(`Loaded id from file: ${id.toString()}`);
    } catch (err) {
      error(`Error creating private key from protobuf: ${err}`);
      throw err;    
    }
  } else {
    try {
      debug("Creating new key pair...");
      privateKey = await generateKeyPair('Ed25519');
      const id = await peerIdFromPrivateKey(privateKey);
      debug(`Created new id: ${id.toString()}`);
    } catch (err) {
      error(`Error creating key pair: ${err}`);
      throw err;
    }
  }
  return privateKey;
}
```

#### Update startp2p (p2p.mts ~line 147)
```typescript
// Change from:
let id : PeerId = await obtainPeerId(nodeId);

// To:
let privateKey = await obtainPrivateKey(nodeId);
let id: PeerId;  // Declare outside try block

// In try block:
let nodeListener: Libp2p = await createLibp2p({
  privateKey: privateKey,  // Changed from peerId
});
// After start:
id = nodeListener.peerId;
```

#### Update services configuration (p2p.mts ~line 253)
```typescript
services: {
  dht: kadDHT(),
  identify: identify(),  // Changed from identifyService()
},
```

### 2.3 Update mkid.mts Tool
```typescript
// Replace imports
import { keys } from '@libp2p/crypto';
import { peerIdFromPrivateKey } from '@libp2p/peer-id';

// Replace key generation
const privateKey = await keys.generateKeyPair('Ed25519');
const peerid = await peerIdFromPrivateKey(privateKey);
const privKeyBytes = keys.privateKeyToProtobuf(privateKey);
const publicKey = privateKey.publicKey;
const pubKeyBytes = keys.publicKeyToProtobuf(publicKey);
```

## Phase 3: Final Migration v1.0.0 → v2.10.0

### 3.1 Update Dependencies
```json
"libp2p": "^2.10.0",
"@libp2p/bootstrap": "^11.0.0",
"@libp2p/kad-dht": "^14.0.0",
"@libp2p/mdns": "^11.0.0",
"@libp2p/tcp": "^10.0.0",
"@libp2p/websockets": "^9.0.0",
"@libp2p/crypto": "^5.0.0",
"@libp2p/peer-id": "^5.0.0",
"@libp2p/identify": "^3.0.0",
"@libp2p/circuit-relay-v2": "^3.0.0",
"@libp2p/interface": "^2.0.0"
```

Remove: `@libp2p/interface-connection-manager` (no longer exists)

### 3.2 API Changes for v2.10.0

#### Update crypto imports (correct API for v5)
```typescript
import { keys } from '@libp2p/crypto';
// Use keys.privateKeyFromProtobuf instead of unmarshalPrivateKey
// Use keys.generateKeyPair for key generation
```

#### Update error handling (p2p.mts lines 977-1018)
```typescript
// Support both err.code and err.name for compatibility
if(err.name || err.code) {
  const errorId = err.name || err.code;
  switch (errorId) {
    case 'NetworkUnreachableError':
    case 'ENETUNREACH':
      // handle error
    // ... etc
  }
}
```

#### Update configuration
```typescript
// Change from connectionEncryption to connectionEncrypters
connectionEncrypters: [noise()]
```

#### Update DHT configuration
```typescript
services: {
  dht: kadDHT({
    clientMode: false,  // Run as both client and server
    protocol: '/ipfs/kad/1.0.0'
  }),
  identify: identify(),
},
```

### 3.3 Update Relay Server (relay.mjs)

#### CRITICAL: Add console output for test runner
```typescript
// The test runner looks for these specific outputs:
console.log('Listening on:');
node.getMultiaddrs().forEach((ma) => console.log(`  ${ma.toString()}`));
console.log('RELAY_READY');  // REQUIRED for test runner
```

#### Update imports
```typescript
import { circuitRelayServer } from '@libp2p/circuit-relay-v2';
import { identify } from '@libp2p/identify';
import { keys } from '@libp2p/crypto';
import { peerIdFromPrivateKey } from '@libp2p/peer-id';
```

#### Update configuration
```typescript
connectionEncrypters: [noise()],  // Changed from connectionEncryption
services: {
  identify: identify(),  // Changed from identifyService()
  relay: circuitRelayServer({
    reservations: {
      defaultDurationLimit: 2147483647,
      defaultDataLimit: BigInt(4294967295),
    }
  }),
}
```

## Phase 4: Circuit Relay v2 Issues (UNRESOLVED)

### Known Problems
1. **keepAliveRelay function needs refactoring** - Circuit relay v2 doesn't use custom protocols
2. **dialRelay function broken** - `_RELAY_PROTOCOL` doesn't exist in v2
3. **getPeerInfo relay addresses** - Format `/p2p/${_relayId}/p2p-circuit/p2p/${id}` might be wrong
4. **Peer discovery through relay fails** - Nodes can't find each other even when connected to same relay

### Attempted Fixes That Didn't Work
1. ❌ Adding relay listen address: `listenAddrs.push(${relays[0]}/p2p-circuit)`
2. ❌ Using `_node.dial(relayId)` instead of `_node.dialProtocol(relayId, _RELAY_PROTOCOL)`
3. ❌ Simplifying keepAliveRelay to just dial once

### What Needs Investigation
1. How circuit relay v2 reservations work
2. Whether nodes need to explicitly make reservations on the relay
3. How peer discovery through relay works in v2
4. Whether the relay address format has changed
5. If additional configuration is needed for circuit relay transport

## Testing & Validation

### Critical Test Commands
```bash
# Build runtime
make rt

# Build P2P tools
cd p2p-tools && tsc

# Test basic echo (MUST PASS)
./scripts/multinode-runner.sh tests/rt/multinode-tests/basic-echo/config.json

# Test direct connection
./scripts/multinode-runner.sh tests/rt/libp2p-migration-tests/p2p-connection-tests/direct-connection/config.json

# Run all multinode tests (ALL MUST PASS)
./scripts/run-multinode-tests.sh
```

### Success Criteria
- [ ] Basic echo test passes
- [ ] Direct connection test passes
- [ ] Cross-spawn test passes
- [ ] Ring echo tests pass
- [ ] Trust flow test passes
- [ ] ALL multinode tests pass without timeout

### Debugging Tips
1. Use `DEBUG=libp2p:* node ...` to see libp2p debug output
2. Check that relay prints "RELAY_READY" - test runner waits for this
3. Watch for "Timeout" in test output - indicates whereis failure
4. Nodes should show "Successfully found service" not timeout

## Rollback Instructions
If migration fails:
```bash
git checkout -- package.json
git checkout -- rt/src/p2p/p2p.mts
git checkout -- p2p-tools/mkid.mts
git checkout -- p2p-tools/relay/relay.mjs
make clean && make all
```

## Next Steps for Future Attempts
1. **Research circuit relay v2 deeply** - The documentation is insufficient
2. **Create minimal test case** - Isolate P2P connectivity from Troupe logic
3. **Consider staying on v1.0.0** - v2 might have too many breaking changes
4. **Engage libp2p community** - The circuit relay v2 changes are not well documented
5. **Test incrementally** - Get one multinode test working before proceeding

## Important Warnings
- **DO NOT** consider the migration complete if only local tests pass
- **DO NOT** skip multinode tests - they are the ONLY validation that matters
- **DO NOT** assume circuit relay works the same in v2 - it's fundamentally different
- **EXPECT** to spend significant time debugging P2P connectivity issues