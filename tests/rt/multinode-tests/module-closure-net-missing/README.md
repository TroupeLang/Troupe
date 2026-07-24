# module-closure-net-missing

Regression test: a node stays alive when it receives a closure over a module it
does not have.

`nodea` has the `Stamp` module (pinned in `nodea.deps.json`) and sends `nodeb` a
closure that references `Stamp.stamp`. `nodeb` does not have `Stamp`, so
reconstructing the received closure fails to link the module. The receiver drops
the message and keeps running rather than terminating.

`nodeb` blocks in `receive`; its only exit is the timeout guard (exit 124).
Before the inbound-error handling in `receiveFromRemote`, the missing-module
link failure crashed `nodeb` instead.
