# Quarantine Echo Example

This example demonstrates trust-based information flow control between two nodes with asymmetric trust.

## Scenario

- **Client** trusts the **Server** at level `{alice}`
- **Server** does NOT trust the **Client** at level `{alice}` (trusts at `BOT` only)

When the client sends information labeled at `{alice}` to the server:
1. The client can send because it trusts the server at that level
2. Because the server does not trust the client, the message arrives with null integrity and a
   per-message **quarantine authority** in its metadata record. The server's gate —
   `enableRangedReceive`, a ranged `rcv`, then `disableRangedReceive`, `blockdown` and explicit
   `downgrade` calls with its own authority — brings the payload back to `{}` before use, and it
   sends the reply under that quarantine
   authority (`invokeAtLevel`). The client extracts `quarantineAuth` from the reply metadata with
   `rcvp` and calls `endorse` on the response it receives.

This demonstrates quarantining data from an untrusted peer: nothing arrives usable, and each side
has to downgrade or endorse explicitly. Both nodes run with `--debugquarantine`.

## Trust Configuration Approaches

### Static Approach (Recommended for this example)

Trust relationships are defined in trustmap JSON files that are loaded at node startup. This approach is:
- Simple and explicit
- Good for fixed network topologies
- Easy to understand and debug
- Suitable for examples and testing

### Programmatic Approach

Trust could also be configured at runtime. This would be useful for:
- Dynamic trust negotiation
- Trust that changes over time
- Complex trust policies

For this initial example, we use the static approach.

## Quick Start

```bash
cd examples/network/quarantine-echo-01
make setup   # First-time only: creates identifiers and trustmap
make run     # Runs both server and client
```

## Manual Setup (Alternative)

If you prefer to run server and client in separate terminals:

1. **First-time only**: Set up identifiers and trustmap:
   ```bash
   make setup
   ```

2. **Run the server** (in one terminal):
   ```bash
   make server
   ```

3. **Run the client** (in another terminal):
   ```bash
   make client
   ```

## Cleanup

```bash
make clean   # Removes generated files and kills any running server
```

## Expected Output

Each side prints the values it handles with `printWithLabels`, so the labels appear next to the
data rather than on lines of their own.

**Server output:**
```
SERVER: waiting for messages...
SERVER: Received echo request
SERVER: Sending node id
SERVER: Received msg with labels:
<the received string, printed with its labels>
SERVER: Sent reply
```

**Client output:**
```
CLIENT: Starting echo client
CLIENT: Found echo server
CLIENT: test_msg with labels:
CLIENT: Sending message at level {alice}
CLIENT: Received response (raw) with labels:
CLIENT: Got quarantine authority:
CLIENT: Response after endorsement with labels:
CLIENT: Echo test completed
```

The key observation is that data from an untrusted peer arrives with null integrity and a quarantine
authority attached, so the client's response is unusable until it endorses it with that authority.

## Next Steps

This example is part of a series exploring quarantine mechanisms:
1. Basic skeleton (this example)
2. Accessing message metadata
3. Record-based metadata approach
4. Quarantine protocol
5. Gate call idiom
