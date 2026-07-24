# module-spawn-net-missing

Regression test: a node stays alive when a remote SPAWN carries a closure over a
module it does not have.

`spawner` has the `Stamp` module (pinned in `spawner.deps.json`) and spawns on
`target` (running with `--rspawn`) a closure that references `Stamp.stamp`.
`target` does not have `Stamp`, so reconstructing the spawned closure fails to
link the module. `target` drops the spawn and keeps running.

Both nodes exit through their timeout guards (exit 124): `target` blocks in
`receive`, and `spawner` blocks on the spawn that the target rejected. Before the
inbound-error handling in `spawnFromRemote`, the missing-module link failure
crashed `target` instead.
