# Relay node tests

Programs and scripts that run a relay node (`--start-relay`) whose program answers the
libp2p relay service's questions through `P2PRelayControl.connectionGater` and acts on its
reservations with `setLimits`, `removeReservation` and `reservations`, with client nodes
reserving a slot on it, through `P2PRelayClient` and a token when the relay does not know them,
and echoing through it. The folder is not part of `make ci`;
whether it joins the suite is the maintainer's decision.

| File                         | Role                                                                   |
|------------------------------|------------------------------------------------------------------------|
| `src/relay-no-gater.trp`     | relay program that never calls `connectionGater`                       |
| `src/relay-list.trp`         | admits alice and bob, every relayed connection                         |
| `src/relay-time-limit.trp`   | a sleeper denies at 2 s; an admin process admits at 3 s                |
| `src/relay-labels.trp`       | prints each peer id's label and admits on it                           |
| `src/relay-limits.trp`       | `setLimits` with a 20 s lifetime; prints `reservations` every 5 s      |
| `src/relay-remove-end.trp`   | `removeReservation` with `endCircuits = true` during an echo           |
| `src/relay-remove-keep.trp`  | `removeReservation` with `endCircuits = false` during an echo          |
| `src/echo-server.trp`        | bob: registers `echo`, answers every request                           |
| `src/echo-client.trp`        | alice: one echo to bob after 3 s                                       |
| `src/echo-client-late.trp`   | alice: one echo to bob after 600 s idle                                |
| `src/echo-client-loop.trp`   | alice: one echo to bob a second for 20 s                               |
| `src/client-idle.trp`        | holds its reservation for 9 s                                          |
| `src/client-hold.trp`        | holds its reservation for 70 s                                         |
| `src/relay-token.trp`        | admits bob, and peers that the library's admission process admits      |
| `src/relay-revoke.trp`       | as `relay-token`, 20 s lifetime; revokes the first admission after 5 s |
| `src/client-token.trp`       | alice: `keepReserved` with the token `s3cret`; serves one echo         |
| `src/client-token-wrong.trp` | alice: `keepReserved` with a wrong token                               |
| `src/client-watch.trp`       | alice: `watch` with the token `s3cret`; serves echo                    |
| `src/client-dial-carol.trp`  | alice: prints the relay messages while it looks up carol               |
| `src/echo-to-alice.trp`      | bob: one echo to alice through the relay node                          |
| `src/*.json`                 | aliases and trust maps, with `@@NAME@@` for the peer ids               |

A relay program answers each question by calling `reply` with a boolean, in libp2p's
polarity: the questions are named after libp2p's connection gater hooks,
`denyInboundRelayReservation` and `denyOutboundRelayedConnection`, so `reply true`
denies and `reply false` admits.

`setup.sh` creates `ids/{relay,alice,bob,carol}.json` with `p2p-tools/built/mkid.mjs`
(an existing id is kept), fills the ids into `src/` to give `gen/`, and compiles the
programs to `built/`. The three folders are ignored by git. Run it after `make rt`,
`make lib` and `make p2p-tools`.

The cases are `no-gater`, `list`, `time-limit`, `labels`, `limits` (80 s), `remove-end`,
`remove-keep`, `idle` (10 minutes), `token`, `token-wrong`, `revoke` (40 s) and `dial-failed`.
In the token cases the relay's trust map leaves alice out and alice's holds the relay at
`{#ROOT}`. A client program that names `@@RELAYADDR@@` is compiled by `run.sh` with the relay
address filled in, to `built/<program>-at-relay.js`. Each prints what it expects, then the nodes' stdout
and the lines of their libp2p debug output that concern reservations, denials and errors.
`CLIENT_DEBUG` replaces the clients' `DEBUG` (default `libp2p:circuit-relay*`).

## Local mode

The relay node listens on 127.0.0.1:`PORT` (WebSocket, default 7560) and `PORT`+1 (TCP);
the clients alice, bob and carol run beside it on `PORT`+2 to `PORT`+4.

```
tests/relay-node/setup.sh
tests/relay-node/run.sh local                 # every case but idle
PORT=7640 tests/relay-node/run.sh local limits
tests/relay-node/run.sh local idle
```

## Remote mode

The relay node runs on another host, one case at a time, and the clients here dial
`/ip4/<host>/tcp/<port>/ws/p2p/<relay peer id>`. Both sides need the same `ids/`: run
`setup.sh` here, copy `ids/` to the same folder on the host, and run `setup.sh` there.

```
host$  tests/relay-node/run.sh relay list 7560        # the relay node, in the foreground
here$  tests/relay-node/run.sh remote <host> 7560 list
```

The relay node's output stays on the host. In case `idle`, the clients wait for the
relay node to stop after the echo; stop it on the host with SIGTERM.
