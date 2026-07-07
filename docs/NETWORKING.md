# Networking

> **Scope:** the P2P runtime — how nodes connect, identity and IDs, libp2p relaying, node discovery,
> and known stability caveats. For running programs locally without networking see
> [DEVELOPMENT.md](DEVELOPMENT.md); for the runtime architecture see [ARCHITECTURE.md](ARCHITECTURE.md).

When the program starts, once the keys are loaded (or generated), the runtime starts by connecting to
a number of nodes to bootstrap its discovery. Note that this process takes a human-observable time.


## Local only mode
To skip network connection, one can provide `--localonly` flag to the runtime, but
observe that in this case all external I/O will result
in a runtime error.


## Generating new persistent IDs
See [p2p-tools/mkid.mts](../p2p-tools/mkid.mts).

## Auto-created IDs

If the id file is omitted, a new id (via a fresh key/pair) is generated upon
start. Observe that this induces a bigger runtime overhead than loading a key pair
from a file.

## Stability issues.

Libp2p is a fast-moving project, and there are stability issues. Workarounds live in the runtime
code — for example, relay keep-alive messages and the `--relay-fault-tolerance` and `--disable-relay`
runtime flags handled in `rt/src/p2p/p2p.mts`. The libp2p versions are pinned in the root
`package.json`; there is no separate patch-application step during installation.


## Notes on the p2p runtime

The p2p runtime is implemented using [libp2p](https://libp2p.io/) library (part of IPFS project). This
means that nodes at runtime are now libp2p nodes.  We inherit from libp2p that every node has an
associated pair of public/private keys, and an id of the node is the hash of its
public key.

We use libp2p's functionality of relaying messages. This means that programs or
processes running behind NAT (e.g., something running on a developer laptop) are
accessible from the outside.


## Navigating the code base

The main p2p runtime module is in [rt/src/p2p/p2p.mts](../rt/src/p2p/p2p.mts).


## How node discovery works

Peer discovery uses three mechanisms (`rt/src/p2p/p2p.mts`):

- **bootstrap** — connecting to the public libp2p bootstrap nodes listed in `p2p.mts`
- **mDNS** — discovering peers on the local network
- **kad-DHT** — the Kademlia distributed hash table

All three are disabled in relay-only mode. NAT traversal is handled by circuit relay v2
(`@libp2p/circuit-relay-v2`): a node behind NAT is reachable through a relay.
