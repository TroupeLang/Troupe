# Troupe in the browser

A Troupe node, its compiler and its terminal, all in a web page: a program typed into the editor is
compiled by the Haskell compiler cross-built to WebAssembly, run on the runtime in a worker, and its
stdio is a terminal in the page. The page also shows every intermediate stage of the compiler and
compiles received IR the way a node does when a closure arrives.

## Running it

```sh
make compiler-wasm                 # builds bin/troupec.wasm
npm install                        # once, for the WASI shim, xterm.js and CodeMirror
make web-demo                      # assembles the site in build/web-demo
node examples/web-demo/serve.mjs   # serves it on http://localhost:8080/
```

Pass a port as the first argument to use a different one. The page needs a server because a page
cannot `fetch()` from `file://` and the `.wasm` needs its content type; `serve.mjs` serves the
directory and nothing else, and adds the cross-origin isolation headers, which give the page the
finer `performance.now()` clamp.

## The site

`build/web-demo` is self-contained and every URL in it is relative, so a static host serves it
from any path. `scripts/deploy-web-demo.sh <repository url>` publishes it as one commit to a
repository GitHub Pages serves from.

| Path                      | What                                                            |
|---------------------------|-----------------------------------------------------------------|
| `index.html`, `demo.mjs`, `theme.mjs`, `style.css`, `favicon.svg` | the page, from this directory |
| `samples/`                | the sample programs and their manifest, `samples.json`          |
| `troupe-page.mjs`         | the page's side of a node (`rt/src/hosts/browser/page.mts`)     |
| `troupe-editor.mjs`       | the program editor, CodeMirror with the Troupe mode (`editor.mjs` here) |
| `troupe-worker*.mjs`      | the runtime worker and its chunks                               |
| `compiler-worker.mjs`     | the compiler worker                                             |
| `troupec.wasm`            | the compiler                                                    |
| `lib/out/`                | the compiled libraries and their interfaces; `lib-exports.json` lists the interfaces |
| `trp-rt/out/service.js`   | the runtime's service module                                    |
| `xterm/`                  | the terminal and its fit addon, from the `@xterm/xterm` and `@xterm/addon-fit` packages |

## What it does

**Compile and run.** `run` compiles the editor's program and starts a Troupe node on it: a runtime
worker running with `--localonly`, fetching the libraries it links from `lib/out/` on demand and
the service module from `trp-rt/out/`. The node's stdio is the terminal (xterm.js): what the
program writes appears there, and a line typed there is a line of its stdin, so `inputLine` blocks
until Enter. The page supplies the line discipline — echo, backspace, Enter — since the node reads
lines; Ctrl-C terminates the worker. The node relinks the closures it receives — a value restored
with `restore`, for one — through the page's compiler worker, which the page connects to the node
before it starts (`compilerPort` in `troupe-page.mjs`). The editor is CodeMirror 6, bundled by
`make editor-bundle` from `editor.mjs`, whose Troupe mode follows `compiler/src/Lexer.x` rule for
rule and takes its colours from the `--syn-*` properties in `style.css`. Ctrl-Enter in the editor
runs; Tab inserts two spaces.

**The compiler's stages.** `troupec -v` writes `out/out.syntax`, `out/out.cps`, `out/out.stack` and
the rest as it runs. Natively those land on disk; here the filesystem is a `Map` the page owns, so
the same run leaves its stage dumps in memory and the page reads them back. No compiler flag was
added for this: it is the CLI's existing verbose output, read from a filesystem that happens to be
JavaScript objects.

**Received IR.** The second panel takes a base64 `TRPI` blob and runs the compiler's `--json-ir`
mode over it, which is exactly the path `rt/src/deserialize.mts` drives when a closure arrives from
another node: one base64 line per blob, an `!ECHO` marker bracketing the batch, and a JSON
`{code, sourceMap?}` back for each. The button lifts a blob out of the program just compiled.

## How it works

The page starts the runtime's compiler worker and talks to it through the runtime's compiler
service (`rt/src/hosts/browser/compilerService.mts`), the object a browser Troupe node registers to
relink the closures it receives; the runtime worker and the page's side of both are described in
`rt/src/hosts/browser/protocol.mts` and `page.mts`. The worker runs
`rt/src/hosts/browser/troupecWasm.mts`; `tests/rt/compiler-service/test-compiler-service.mjs` checks
the same service against the native compiler under Node, and `tests/rt/browser-host/` runs the
golden corpus through the runtime's browser host.

`bin/troupec.wasm` is an ordinary WASI command module: its only imports are
`wasi_snapshot_preview1`, and it exports `memory` and `_start`. There is no JavaScript glue inside
it and no Haskell shim — the driver builds a filesystem out of
[`@bjorn3/browser_wasi_shim`](https://github.com/bjorn3/browser_wasi_shim) objects, instantiates the
module, and reads the files it wrote. Each compile gets a fresh instance and a fresh filesystem
containing `/p.trp` (the source), `/out/` (empty; the compiler writes stage dumps there and does not
create the directory) and `/troupe/lib/out/*.exports` (the library interfaces, so `import <Library>`
resolves), with the environment `TROUPE=/troupe`. Library resolution depends on that variable:
`getExecutablePath` cannot locate an install root under WASI, so `getTroupeHome` reads the
environment instead, as `compiler/src/ProcessImports.hs:37-48` documents.

The one-shot model — a fresh instance per compile — is correct because the `--json-ir` loop is
stateless per line; the worker memoizes relinked blobs, so a blob a node has seen is not compiled
again. The JavaScript emitted in the browser is byte-identical to what native `bin/troupec` emits
for the same source, the oracle `scripts/wasm-differential.sh` applies under wasmtime.

## Verified

Driven headlessly in Chrome as part of developing it: every sample runs on the node in the page
with the expected output and result; a program reading a line blocks until it is typed into the
terminal; a loop over typed lines with a backspace edit ends on an empty line; Ctrl-C stops a node
waiting for input; the error sample reports its position with the source line.
