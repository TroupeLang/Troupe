# Force Relay Option Analysis for Troupe P2P

## Executive Summary

This document analyzes the current P2P implementation in Troupe's runtime to propose adding a CLI option that forces all peer communication to go through a relay server. This would be valuable for testing purposes, especially in multinode test scenarios where we want to simulate NAT traversal conditions or ensure consistent network behavior.

## Current P2P Architecture Analysis

### Relay Usage in Current Implementation

The current P2P implementation in `rt/src/p2p/p2p.mts` has several mechanisms for peer discovery and connection:

1. **Direct Connection**: Peers try to connect directly first
2. **Peer Discovery**: Uses bootstrapping, mDNS, and DHT for peer discovery
3. **Relay as Fallback**: Circuit relay is configured as a transport option but only used when direct connections fail

### Key Components

#### 1. Circuit Relay Transport Configuration (lines 221-223)
```typescript
circuitRelayTransport({
  discoverRelays: 1,
})
```
Currently configured to discover 1 relay automatically.

#### 2. Peer Discovery Flow (getPeerInfo function, lines 330-380)
The peer discovery process follows this hierarchy:
1. Check known nodes from p2pconfig (direct IP addresses)
2. Check peerStore for cached addresses
3. Use peerRouting (DHT) to find peers
4. **Only then** add relay addresses as circuit routes

#### 3. Relay Connection Management (lines 609-654)
- `keepAliveRelay()` maintains connection to relay
- `dialRelay()` establishes relay connections
- Relay ID is stored globally (`_relayId`)

### Current Limitations for Testing

1. **Relay is Optional**: Relay usage only occurs when direct connections fail
2. **No Force Mechanism**: No way to bypass direct connection attempts
3. **Discovery Order**: Direct connections are always attempted first
4. **Configuration Dependency**: Relies on p2pconfig.mjs for relay addresses

## Proposed Solution: --force-relay CLI Option

### Implementation Approach

#### 1. Add CLI Argument
Add `ForceRelay` to `TroupeCliArgs.mts`:
```typescript
export enum TroupeCliArg {
    // ... existing args
    ForceRelay = 'force-relay',
}
```

#### 2. Modify libp2p Configuration
When `--force-relay` is enabled:
- Disable direct transports (TCP, WebSockets)
- Keep only circuit relay transport
- Force relay discovery

#### 3. Modify Peer Discovery Logic
Update `getPeerInfo()` function to:
- Skip known nodes lookup when force-relay is enabled
- Skip peerStore direct address lookup
- Skip peerRouting for direct addresses
- **Only** use relay addresses for all peers

### Detailed Implementation Plan

#### Phase 1: CLI Integration
```typescript
// In TroupeCliArgs.mts
.option(TroupeCliArg.ForceRelay, { 
    type: 'boolean', 
    default: false, 
    describe: 'Force all P2P communication through relay servers' 
})
```

#### Phase 2: Configuration Modification
```typescript
// In createLibp2p() function
async function createLibp2p(_options) {
  const argv = getCliArgs();
  const forceRelay = argv[TroupeCliArg.ForceRelay];
  
  const defaults = {
    addresses: {
      listen: forceRelay ? [] : [`/ip4/0.0.0.0/tcp/${__port}`]
    },
    transports: forceRelay ? 
      [circuitRelayTransport({ discoverRelays: 2 })] :
      [tcp(), webSockets(), circuitRelayTransport({ discoverRelays: 1 })]
    // ... rest of config
  };
}
```

#### Phase 3: Peer Discovery Override
```typescript
// In getPeerInfo() function
async function getPeerInfo(id: PeerId): Promise<void> {
  const argv = getCliArgs();
  const forceRelay = argv[TroupeCliArg.ForceRelay];
  
  if (forceRelay) {
    // Skip all direct connection methods
    // Only add relay circuit addresses
    if(_relayId) {
      await _node.peerStore.merge(id, {
        multiaddrs: [
          multiaddr(`/p2p/${_relayId}/p2p-circuit/p2p/${id.toString()}`)
        ]
      });
    }
    return;
  }
  
  // ... existing logic for non-force-relay mode
}
```

### Testing Benefits

1. **Consistent Network Behavior**: All nodes communicate through relay, eliminating direct connection variables
2. **NAT Simulation**: Tests behavior when nodes are behind NAT without actual network setup
3. **Relay Performance Testing**: Focus testing on relay performance and reliability
4. **Multinode Test Reliability**: Reduces network-related test flakiness

### Potential Challenges

#### 1. Dependency on Relay Availability
- Solution: Enhanced relay health checking
- Fallback to multiple relays if primary fails

#### 2. Performance Impact
- Relay adds latency overhead
- Solution: Make this a testing-only option with clear documentation

#### 3. Bootstrap Process
- Need to ensure relay connection is established before peer discovery
- Solution: Wait for relay connection in `startp2p()` when force-relay is enabled

### Integration with Multinode Testing

The multinode testing framework can be enhanced to:

1. **Auto-enable Force Relay**: Add `force_relay: true` option to test configurations
2. **Relay Health Checks**: Verify relay is running before starting tests
3. **Network Isolation**: Ensure nodes only communicate through relay

### Example Usage

```bash
# Force relay for single node
./network.sh myprogram.trp --force-relay

# In multinode test config
{
  "test_name": "relay-only-test",
  "force_relay": true,
  "nodes": [...]
}
```

## Conclusion

Adding a `--force-relay` option to Troupe's P2P runtime would significantly improve testing capabilities by:

1. Providing deterministic network behavior
2. Enabling relay-specific testing scenarios  
3. Improving multinode test reliability
4. Facilitating NAT traversal testing

The implementation requires modifications to CLI parsing, libp2p configuration, and peer discovery logic, but the changes are well-contained and backward-compatible.

## Next Steps

1. Implement CLI argument in `TroupeCliArgs.mts`
2. Modify `createLibp2p()` configuration logic
3. Update `getPeerInfo()` peer discovery flow
4. Add force-relay option to multinode test configurations
5. Test with existing multinode test suite
6. Document usage and limitations

---

**Author**: Analysis conducted on 2024-12-19
**Status**: Proposal - Ready for Implementation