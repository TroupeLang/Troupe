# Hostile-peer tests

Tests where a raw libp2p peer — not a cooperating Troupe node — attacks one
victim node with crafted or malformed protocol input. These cover inputs no
Troupe program can produce, and assert that the victim **survives**: a hostile
peer must not be able to terminate a node.

This differs from `tests/rt/multinode-tests/`, where every actor is a Troupe
node and the oracle is exit codes plus golden output.

## Running

```
scripts/run-hostile-peer-tests.sh            # all tests
scripts/run-hostile-peer-tests.sh <pattern>  # tests whose name matches
make test/hostile-peer
```

## Layout of a test

```
tests/rt/hostile-peer/<name>/
  victim.trp   the node under attack; prints "VICTIM: ready", then blocks. A
               spawned guard exits with the survival code after a few seconds.
  attack.mjs   one or more hostile peers; argv[2] is the victim multiaddr.
               Uses the shared primitives in _lib/peer.mjs.
  test.json    victim port, survival exit code, attack list, and the oracle.
```

`_lib/peer.mjs` holds the libp2p dial and frame-send primitives shared by
attack scripts.

## The oracle

A test passes when, after all attacks run, the victim:

- is still alive immediately after the attacks (did not crash mid-attack);
- exits with `victim.survive_exit_code` (its guard fired — it ran to the end);
- logs every string in `oracle.victim_log_contains` (the expected drop);
- logs none of `oracle.victim_must_not_contain` (crash signatures such as
  `Unhandled error case` or the Node.js crash banner).

## Runtime notes

The victim identity is generated per run with `p2p-tools/built/mkid.mjs`; no
keys are committed. The runner removes the generated id and log afterward.
Attack scripts import libp2p from the repo's `node_modules`, so `make rt` (or a
full build) must have installed dependencies first.
