# Troupe in the browser

The Haskell compiler, cross-built to WebAssembly, compiling Troupe source inside a web page — with
every intermediate stage of the pipeline visible — and the runtime, in a worker, running what it
compiled. A second panel compiles received IR the way a node does when a closure arrives.

## Running it

```sh
make compiler-wasm                        # builds bin/troupec.wasm
npm install                               # once, for the WASI shim
make browser                              # bundles the two workers and the page's side of them
node examples/wasm-compiler-demo/serve.mjs
```

Then open <http://localhost:8080/>. Pass a port as the first argument to use a different one.

The page needs a server because a page cannot `fetch()` from `file://` and the `.wasm` needs its
content type; `serve.mjs` does nothing else.

## What it demonstrates

**Compilation, staged.** `troupec -v` writes `out/out.syntax`, `out/out.cps`, `out/out.stack` and
the rest as it runs. Natively those land on disk. Here the filesystem is a `Map` the page owns, so
the same run leaves its stage dumps in memory and the page reads them back. Twelve stages, from
`parsed` to `stack`, then the emitted JavaScript.

No compiler flag was added to make this work, and none was needed — this is the CLI's existing
verbose output, read from a filesystem that happens to be JavaScript objects.

**Running.** `run` compiles the source and starts a Troupe node on it: a runtime worker
(`build/browser/troupe-worker.mjs`) running with `--localonly`, fetching the libraries it links from
`lib/out/` and the service module from `trp-rt/out/`, with its output and the main thread's result
shown in the page. Lines in the input box are fed to the program as its stdin. The node relinks the
closures it receives — a value restored with `restore`, for one — through this page's compiler
worker, which the page connects to the node before it starts (`compilerPort` in
`build/browser/troupe-page.mjs`). The server sends the cross-origin isolation headers, so
`performance.now()` has its finer resolution in the page.

**Compiling received IR.** The second panel takes a base64 `TRPI` blob and runs the compiler's
`--json-ir` mode over it, which is exactly the path `rt/src/deserialize.mts` drives when a closure
arrives from another node: one base64 line per blob, an `!ECHO` marker bracketing the batch, and a
JSON `{code, sourceMap?}` back for each. It is the shape the eventual runtime integration takes,
demonstrated on real blobs — the button lifts one out of the program you just compiled.

## How it works

The page starts the runtime's compiler worker, `build/browser/compiler-worker.mjs`, and talks to
it through the runtime's compiler service (`rt/src/hosts/browser/compilerService.mts`), the object
a browser Troupe node registers to relink the closures it receives; the runtime worker and the
page's side of both are described in `rt/src/hosts/browser/protocol.mts` and `page.mts`. The worker runs
`rt/src/hosts/browser/troupecWasm.mts`, which is where everything below happens;
`tests/rt/compiler-service/test-compiler-service.mjs` checks the same service against the native
compiler under Node.

`bin/troupec.wasm` is an ordinary WASI command module: its only imports are
`wasi_snapshot_preview1`, and it exports `memory` and `_start`. There is no JavaScript glue inside
it and no Haskell shim — the driver builds a filesystem out of
[`@bjorn3/browser_wasi_shim`](https://github.com/bjorn3/browser_wasi_shim) objects, instantiates the
module, and reads the files it wrote.

Each run gets a fresh instance and a fresh filesystem containing:

| Path                     | What                                                              |
|--------------------------|-------------------------------------------------------------------|
| `/p.trp`                 | the source being compiled                                          |
| `/out/`                  | empty, because the compiler writes stage dumps there and does not create the directory |
| `/troupe/lib/out/*.exports` | the standard library interfaces, so `import <Library>` resolves |

and the environment `TROUPE=/troupe`. Library resolution depends on that variable:
`getExecutablePath` cannot locate an install root under WASI, so `getTroupeHome` reads the
environment instead, as `compiler/src/ProcessImports.hs:37-48` documents.

## Verified

Driven headlessly in Chrome as part of developing it:

- The three run samples — hello, a program reading a line of input, a closure persisted and
  restored through the compiler — run on the node in the page with the expected output and result.

- A program compiles, exit 0, with all twelve stage dumps recovered and the emitted JavaScript
  matching what the tab shows.
- The IR panel links a blob lifted from that output back into an evaluatable snippet.
- A program importing `List` compiles, exercising `$TROUPE` and the mounted interfaces.
- **The JavaScript emitted in the browser is byte-identical to what native `bin/troupec` emits for
  the same source.** This is the same differential oracle `scripts/wasm-differential.sh` applies to
  the wasm build under wasmtime, extended to the browser.

## Scope

A page over the runtime's compiler worker; the worker and the service are the component, under
`rt/src/hosts/browser/`. The one-shot model — a fresh instance per compile — is correct because the
`--json-ir` loop is stateless per line; a persistent instance would amortize roughly 50 ms of
instantiation, which is worth doing for an interactive path and is not what makes wasm slower than
native (per-blob compilation is, at about 2.5×). The worker memoizes relinked blobs, so a blob a
node has seen is not compiled again.
