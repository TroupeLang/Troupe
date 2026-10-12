# Networking

> **Scope:** the P2P runtime — how nodes connect, identity and IDs, libp2p relaying, node discovery,
> and known stability caveats. For running programs locally without networking see
> [DEVELOPMENT.md](DEVELOPMENT.md); for the runtime architecture see [ARCHITECTURE.md](ARCHITECTURE.md).

When the program starts, once the keys are loaded (or generated), the runtime starts by connecting to
a number of nodes to bootstrap its discovery. Note that this process takes a human-observable time.


## Local only mode
To skip network connection, one can provide `--localonly` flag to the runtime, but
observe that in this case all network operations will yield
a runtime error.


## Generating new persistent IDs
See [p2p-tools/mkid.mts](../p2p-tools/mkid.mts). [p2p-tools/mkaliases.ts](../p2p-tools/mkaliases.ts)
generates an aliases file from a set of id files. Both are built by `make p2p-tools`.

## Auto-created IDs

If the id file is omitted, a new id (via a fresh key/pair) is generated upon
start. Observe that this induces a bigger runtime overhead than loading a key pair
from a file.

## Stability issues.

Libp2p is a fast-moving project, and there are stability issues. Workarounds live in the runtime
code — for example, relay keep-alive messages and the `--relay-fault-tolerance` and `--disable-relay`
runtime flags handled in `rt/src/p2p/p2p.mts`. The root `package.json` declares libp2p dependency
ranges (`^3.0.0` and the like); exact versions are pinned by `package-lock.json`. There is no
separate patch-application step during installation.


## Notes on the p2p runtime

The p2p runtime is implemented using [libp2p](https://libp2p.io/) library (part of IPFS project). This
means that nodes at runtime are now libp2p nodes.  We inherit from libp2p that every node has an
associated pair of public/private keys, and an id of the node is the hash of its
public key.

We use libp2p's functionality of relaying messages. This means that programs or
processes running behind NAT (e.g., something running on a developer laptop) are
accessible from the outside.

A node takes its relays from `--relay` (repeatable), else from `p2pconfig.json` in the
working directory or the Troupe root; with neither it uses no relay. The public relays are
`/dns4/relay-2.troupe-lang.net/tcp/5555/ws/p2p/12D3KooWSg5fxhEZ4SMtVCwfj81iBUNo15o3d3rEriTAVE6Lubgp`
(Amsterdam) and
`/dns4/relay-sfo.troupe-lang.net/tcp/5555/ws/p2p/12D3KooWS5pvNcKKGevDutCU4RGAbzAtGgVt7PkQdGse8FK9jZKo`
(San Francisco); `make test/public-relay` runs the echo pair through them, by hand, outside
`make ci`. A relay that cannot be reached at start is skipped;
`--relay-fault-tolerance=fatal` makes it stop the node instead.


## Relay node

A node started with `--start-relay <port>` is a relay node: its libp2p instance runs the libp2p
relay service (`circuitRelayServer`) and listens for WebSocket connections on `<port>`; peer
discovery is off. The flag excludes `--relay`, `--localonly`, `--disable-relay` and
`--no-p2p-circuit`, and needs `--id`. A client reserves a slot with `--relay
/ip4/<host>/tcp/<port>/ws/p2p/<relay id>`.

The relay program governs the relay service through the library `P2PRelayControl` (native module
of the same name, node host only). Each function takes the root authority and returns `OK` or
`ERR NOT_RELAY`:

| Function                                           | Effect                                                                                                                     |
|----------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------|
| `connectionGater (authority, pid)`                 | the local process `pid` answers the connection gater's questions                                                           |
| `setLimits (authority, limits)`                    | sets `reservationTtl`, `maxReservations`, `defaultDurationLimit` and `defaultDataLimit` (ms, bytes) for later reservations |
| `removeReservation (authority, peer, endCircuits)` | removes the peer's reservation; `endCircuits = true` also ends its relayed connections                                     |
| `reservations authority`                           | the reservations as `{peer, expiresAt, durationLimit, dataLimit}`                                                          |

The questions arrive as `("denyInboundRelayReservation", peer, reply)` and
`("denyOutboundRelayedConnection", (src, dst), reply)`. They carry libp2p's polarity: `reply true`
denies and `reply false` admits. With no process set, or a dead one, every question is denied.
A client gives up on a reservation that is not answered within 5 s.

The questions, the peer ids in them and the entries of `reservations` are labelled at the pc of
the call that set the process or made the query; the peer's trust level is not part of the label. `reply`,
`setLimits` and `removeReservation` act on the network and need a pc and arguments that flow to
BOT.

`tests/libp2p-contract/contract.mjs` (`make test/libp2p-contract`, part of `make ci`) checks the
libp2p internals the module uses against the installed libp2p; `tests/relay-node/` runs relay
programs with clients over loopback.

### The client side

A node started with `--relay` asks the relay for a reservation once, at startup. Under the default
`--relay-fault-tolerance no-fatal`, a refused node starts and runs on: it keeps its connection to
the relay node and can `whereis`, `send` and `receive` with processes there, but no peer reaches
it through the relay, and nothing in the runtime or in libp2p asks again. The library
`P2PRelayClient` (native module of the same name, node host only) gives the program the second
request and the events that tell it when one is needed. Each function takes the root authority:

| Function                                         | Effect                                                                                 |
|--------------------------------------------------|----------------------------------------------------------------------------------------|
| `reserve (authority, relayAddr)`                 | asks the relay at `relayAddr` for a reservation; `OK {relay, expiresAt}` or `ERR e`    |
| `relayEvents (authority, pid)`                   | the local process `pid` receives the relay messages below                              |
| `keepReserved (authority, relayAddr, onRefused)` | `reserve`; on `ERR PERMISSION_DENIED`, `onRefused ()` and `reserve` once more          |
| `watch (authority, relayAddr, onRefused)`        | makes the calling process the one of `relayEvents`; `keepReserved`, again on each loss |

`relayAddr` is the relay's multiaddr with `/p2p/<relay id>`, as given to `--relay`. `reserve` blocks
the calling thread until the relay answers. Its errors are `PERMISSION_DENIED` (the relay's
connection gater refused), `TIMEOUT` (no answer within 5 s), `DIAL` (the relay could not be
dialled), `NO_NETWORK` (`--localonly` or `--disable-relay`) and `FAILED message`. A reservation
already held and not due for a refresh is returned without asking the relay. The pc and the
arguments must flow to BOT, since the relay sees the request. A node started without `--relay`
dials later peers through the first relay it reserves on.

The messages to the process of `relayEvents`, labelled at the pc of that call:

| Message                                    | Sent when                                                   |
|--------------------------------------------|-------------------------------------------------------------|
| `("relayReservation", relay, expiresAt)`   | a reservation is made or refreshed                          |
| `("relayReservationLost", relay)`          | a refresh fails or the connection to the relay closes       |
| `("relayDialFailed", peer, relay, status)` | one attempt of the runtime's dial loop to dial `peer` fails |

`relay` and `peer` are peer ids, `expiresAt` is in ms since the epoch. `status` is the relay's
status when the relay answered the dial (`NO_RESERVATION`: the peer holds no reservation;
`PERMISSION_DENIED`: the relay's connection gater refused the connection), and libp2p's error
name otherwise. The dial loop makes up to twelve attempts, the second two seconds after the
first and each further one twice as long after the one before, and each failed attempt gives one
message; the message is sent only while the node has a relay. A relay that removes a reservation does not
tell the client: the client learns of it at its next refresh, which libp2p makes five minutes
before the expiry and no sooner than 30 s after the reservation.

`watch` returns only on a refusal that outlasts `onRefused`, or on `ERR NO_NETWORK`; after any
other error it tries again 30 s later. `spawn (fn () => watch (authority, relayAddr, onRefused))`
keeps a reservation as the v1 runtime did.

#### Admission by token

A relay can admit a node it does not know when the node presents a token. The node's
`onRefused` looks up the relay's admission process with `whereis`, sends it `("token", token,
self ())` and receives `"admitted"` or `"refused"`. On the relay, `P2PRelayControl` provides the
admission process:

| Function                                      | Effect                                                                                                                    |
|-----------------------------------------------|---------------------------------------------------------------------------------------------------------------------------|
| `receiveToken authority`                      | the next `("token", token, pid)` from the mailbox, as `(token, pid)` at BOT                                               |
| `admissionProcess (authority, gater, secret)` | on a token equal to `secret`, `("admit", peer)` to `gater` and `"admitted"` to the sender; otherwise `"refused"`; repeats |

The connection gater's process keeps the set of admitted peers and adds `peer` on `("admit",
peer)`; the relay program registers the admission process under a name the clients know.

A message from a node that the relay's trust map leaves out, or maps below `{}`, is quarantined
on ingress: its labels are rewritten to quarantined labels, which no label written in a program
bounds from below, so neither `receive` nor `rcvp` takes it. `receiveToken` takes it in five
steps, each with the root authority:

1. `enableRangedReceive` opens a receive region from BOT to null integrity.
2. `rcv` over that interval takes the message; its values come out at null integrity.
3. `disableRangedReceive` closes the region, and `blockdown` lowers the blocking label that the
   receive raised.
4. `endorse` lowers the token and the pid to BOT.
5. `blockdown` lowers the blocking label that the endorse raised; without it, the answers would
   leave at null integrity.

The answer reaches the client unquarantined when the client's trust map holds the relay at a
level that covers `{}`. `secret` must be at `{}`; a labelled secret makes the comparison carry
its label, and the answer to an untrusted node is then refused.

## Navigating the code base

The main p2p runtime module is in [rt/src/p2p/p2p.mts](../rt/src/p2p/p2p.mts). The relay is a
separate program under [p2p-tools/relay/](../p2p-tools/relay/); see its `README.md`. The runtime's
network-related command-line options are declared in
[rt/src/TroupeCliArgs.mts](../rt/src/TroupeCliArgs.mts), which also rejects inconsistent
combinations (for example `--disable-relay` with `--relay` or `--relay-only`).


## How node discovery works

Peer discovery uses three mechanisms (`rt/src/p2p/p2p.mts`):

- **bootstrap** — connecting to the public libp2p bootstrap nodes listed in `p2p.mts`
- **mDNS** — discovering peers on the local network
- **kad-DHT** — the Kademlia distributed hash table

All three are disabled in relay-only mode. NAT traversal is handled by circuit relay v2
(`@libp2p/circuit-relay-v2`): a node behind NAT is reachable through a relay.
