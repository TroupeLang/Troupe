# Relay node tests

Programs and scripts that run a relay node (`--start-relay`) whose program answers the
libp2p relay service's questions through `P2PRelayControl.connectionGater`, with client
nodes reserving a slot on it and echoing through it. The folder is not part of `make ci`;
whether it joins the suite is the maintainer's decision.

| File                         | Role                                                          |
|------------------------------|---------------------------------------------------------------|
| `src/relay-no-gater.trp`     | relay program that never calls `connectionGater`              |
| `src/relay-list.trp`         | admits alice and bob, every relayed connection                 |
| `src/relay-time-limit.trp`   | a sleeper denies at 2 s; an admin process admits at 3 s       |
| `src/relay-labels.trp`       | prints each peer id's label, `endorse` and `blockdown`, admits |
| `src/echo-server.trp`        | bob: registers `echo`, answers every request                  |
| `src/echo-client.trp`        | alice: one echo to bob after 3 s                              |
| `src/echo-client-late.trp`   | alice: one echo to bob after 600 s idle                        |
| `src/client-idle.trp`        | holds its reservation for 9 s                                 |
| `src/*.json`                 | aliases and trust maps, with `@@NAME@@` for the peer ids       |

`setup.sh` creates `ids/{relay,alice,bob,carol}.json` with `p2p-tools/built/mkid.mjs`
(an existing id is kept), fills the ids into `src/` to give `gen/`, and compiles the
programs to `built/`. The three folders are ignored by git. Run it after `make rt`,
`make lib` and `make p2p-tools`.

The cases are `no-gater`, `list`, `time-limit`, `labels` and `idle` (10 minutes). Each
prints what it expects, then the nodes' stdout and the lines of their libp2p debug
output that concern reservations, denials and errors.

## Local mode

The relay node listens on 127.0.0.1:7560 (WebSocket) and 7561 (TCP); the clients run
beside it.

```
tests/relay-node/setup.sh
tests/relay-node/run.sh local                 # no-gater list time-limit labels
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
