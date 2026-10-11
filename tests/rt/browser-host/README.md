# The golden corpus through the browser host

The runtime's browser host (`rt/src/hosts/browser/`) runs the golden corpus
in two ways, both from the same harness:

- `node tests/rt/browser-host/run-corpus.mjs` (`make test/browser-host`) runs
  each test in a Node worker thread with the browser host's modules, as a
  browser worker runs it, with the libraries fetched over HTTP.
- `node tests/rt/browser-host/server.mjs` serves `page/`, which runs each test
  in a browser runtime worker, `build/browser/troupe-worker.mjs` (`make
  browser`). `window.corpusRun(filter)` runs it from a driver.

A test is compiled by `bin/troupec` as the golden suite compiles it, run with
`--localonly --no-color` and its `.options`, fed its `.input` as lines, and
its stdout is compared with `.nocolor.golden` with the same lines ignored
that `tests/_util/diff.sh` ignores.

What the host leaves out is decided per test in `corpus.mjs`
(`exclusionReason`): programs that need gzip, the Node host's native modules
(SimpleFiles, Tty), signals or content-addressed modules. A test that persists
values needs the origin private file system, so it runs in the browser and is
skipped under Node.

A persisted closure is restored through the node's compiler. Both runners
start the compiler worker when `bin/troupec.wasm` is built (`make
compiler-wasm`) and connect every node to it, as a page does; without it the
nodes run without a compiler and a test that restores a closure says so.

The page's "Closure round trip" (`window.roundTrip()`) is the Node-to-browser
closure round trip: the server runs `round-trip/save.trp` on a Node node,
which persists a closure; the page writes the persisted value into this
origin's storage, and a browser node runs `round-trip/restore.trp`, which
restores the closure through the compiler and applies it; the output is
compared with `round-trip/restore.expected`.
