# troupec in the browser

The Haskell compiler, cross-built to WebAssembly, compiling Troupe source inside a web page — with
every intermediate stage of the pipeline visible, and a second panel that compiles received IR the
way a node does when a closure arrives.

## Running it

```sh
make compiler-wasm                        # builds bin/troupec.wasm
npm install                               # once, for the WASI shim
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

**Compiling received IR.** The second panel takes a base64 `TRPI` blob and runs the compiler's
`--json-ir` mode over it, which is exactly the path `rt/src/deserialize.mts` drives when a closure
arrives from another node: one base64 line per blob, an `!ECHO` marker bracketing the batch, and a
JSON `{code, sourceMap?}` back for each. It is the shape the eventual runtime integration takes,
demonstrated on real blobs — the button lifts one out of the program you just compiled.

## How it works

`bin/troupec.wasm` is an ordinary WASI command module: its only imports are
`wasi_snapshot_preview1`, and it exports `memory` and `_start`. There is no JavaScript glue inside
it and no Haskell shim — the page builds a filesystem out of
[`@bjorn3/browser_wasi_shim`](https://github.com/bjorn3/browser_wasi_shim) objects, instantiates the
module, and reads the files it wrote.

Each run gets a fresh instance and a fresh filesystem containing:

| Path                     | What                                                              |
|--------------------------|-------------------------------------------------------------------|
| `/p.trp`                 | the source being compiled                                          |
| `/out/`                  | empty, because the compiler writes stage dumps there and does not create the directory |
| `/troupe/lib/out/*.exports` | the standard library interfaces, so `import <Library>` resolves |

and the environment `TROUPE=/troupe`. That variable is load-bearing: `getExecutablePath` cannot
locate an install root under WASI, so `getTroupeHome` falls through to the environment, which is its
documented fallback (`compiler/src/ProcessImports.hs:37-48`).

## Verified

Driven headlessly in Chrome as part of developing it:

- A program compiles, exit 0, with all twelve stage dumps recovered and the emitted JavaScript
  matching what the tab shows.
- The IR panel links a blob lifted from that output back into an evaluatable snippet.
- A program importing `List` compiles, exercising `$TROUPE` and the mounted interfaces.
- **The JavaScript emitted in the browser is byte-identical to what native `bin/troupec` emits for
  the same source.** This is the same differential oracle `scripts/wasm-differential.sh` applies to
  the wasm build under wasmtime, extended to the browser.

## Scope

A demonstration, not a component. It does not touch `rt/` or `notebook/`, and nothing in the runtime
depends on it. The one-shot model it uses — a fresh instance per compile — is correct because the
`--json-ir` loop is stateless per line; a persistent instance would amortize roughly 50 ms of
instantiation, which is worth doing for an interactive path and is not what makes wasm slower than
native (per-blob compilation is, at about 2.5×).
