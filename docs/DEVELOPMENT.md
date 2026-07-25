# Development

> **Scope:** editor setup, build/test commands, running programs, and source maps. For first-time
> installation see [INSTALL.md](INSTALL.md); for how the system is put together see
> [ARCHITECTURE.md](ARCHITECTURE.md).

## Setting up a development environment

### Using VSCode

#### Setting up remote development on a Linux remote machine (optional)
This will allow to develop on a remote machine, using VSCode on a local machine to access the project. Compilation and tools such as the Haskell Language Server will run on the remote machine.


##### On the remote machine

- Setup ssh
- Install [ghcup](https://www.haskell.org/ghcup/install/#how-to-install): `curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`
  - The installation script will ask whether to install stack and the Haskell Language Server, say yes both
  - Make sure that PATH is updated as suggested
- Reboot remote (or make sure that the newly installed environment variables are on PATH and available everywhere)


##### On the local machine

- Install Visual Studio Code
- Install the [Remote Development extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.vscode-remote-extensionpack)
  - NOTE: ssh connections might not work with the open source builds of VS Code (where the ssh extensions have to be manually downloaded and installed, or open source alternatives have to be used)
- Now you should be able to connect to the remote machine (use the green button in the bottom-left, or F1 -> "Remote-SSH: Connect to Host"). If the remote machine is added in `.ssh/config`, it should show up. You'll be asked to enter the password for the ssh key.
  - On first connect, the VSCode server will automatically be installed on the remote machine.


#### Setting up Haskell support

- Install the [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) and [Haskell Syntax Highlighting](https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell) extensions (on the remote if applicable)
  - The Haskell extension will at some point ask which way to use to manage HLS, choose GHCup
  - In the description of the Haskell extension is a table with supported GHC versions. Make sure that the project's stack resolver in `compiler/stack.yaml` is set to a version with a supported GHC version (see [stackage.org](https://www.stackage.org/)).
- Open the `compiler` folder. VSCode should start setting up the Haskell Language Server and might ask whether to download some specific versions of HLS/stack/ghc.
  - In case of a failure, it might help to reload the window, so that VSCode tries again.

Now, when having opened the `compiler` folder, the Haskell Language Server should highlight errors and hints and support "Go to definition".


#### Syntax support for Troupe files

Troupe syntax is similar to SML; the [SML Environment](https://marketplace.visualstudio.com/items?itemName=vrjuliao.sml-environment) extension adds syntax highlighting, some indentation support, and commenting with editor commands.

Use `Ctrl-k m` ("Change language mode") to set the current file's language mode to SML. The suggestions will also allow to generally associate `.trp` files with SML mode.


#### Building and running

- **Building the compiler:** With the `compiler` folder open, the task "Make all" is the default build task, so "Run Build Task" executes it.
- **Compiling a Troupe file:** With the Troupe root folder open, the task "Compile" runs `bin/troupec -v` on the currently focused file. It is the default build task there.
- **Running a Troupe file locally:** With the Troupe root folder open, the task "Run local" runs `local.sh` on the currently focused file. It is the default test task, so "Run Test Task" executes it.
- Tasks are defined in the respective `.vscode/tasks.json` file, where further tasks can be added. These files are not version-controlled (`.gitignore` excludes `*.vscode`).

<!-- #### Makefile support -->
<!-- - Install the extension [Makefile Tools](https://marketplace.visualstudio.com/items?itemName=ms-vscode.makefile-tools) -->
<!-- - Open the Troupe root folder and select the Makefile tab in the left bar -->
<!-- - Set "Build target" to "all" -->

## Building and running

### Building

The following commands build specific parts of the project and install the results to the `bin`,
`rt/built`, `lib/out`, `trp-rt/out`, and `p2p-tools/built` directories. All are targets of the
root `Makefile`.

- `make` / `make all`: run `make npm`, then build the compiler, runtime, service placeholder
  (`trp-rt`), p2p-tools, and libraries. `all` is the default goal, so a bare `make` builds
  everything, not just the compiler.
- `make npm`: `npm install`, then `npm install -g typescript`
- `make compiler`: build the compiler and install `bin/troupec`, `bin/golden`, `bin/irtester`,
  and `bin/dclabels`
- `make rt`: build the runtime (into the `rt/built` directory)
- `make lib`: compile Troupe's built-in libraries (into the `lib/out` directory)
- `make trp-rt`: compile the service module placeholder from `trp-rt/service.trp`
- `make p2p-tools`: build `p2p-tools/` and `p2p-tools/relay/`
- `make notebook`: `npm install` and `npm run build` in `notebook/`
- `make clean`, `make clean/compiler`, `make clean/rt`, `make clean/trp-rt`, `make clean/p2p-tools`,
  `make clean/lib`: remove build artifacts

`make lib` and `make trp-rt` require `bin/troupec` to exist and fail with a message pointing at
`make compiler` otherwise.

`make benchmark-deps` re-pins the per-program dependencies files (`<main>.deps.json`). It runs
`bin/troupec --update-deps` on every `.trp` file under `examples/` that has a line beginning
`import "./`. The pins are content hashes over the imported modules' codegened IR, so re-run it
after changing an imported module or rebuilding the compiler; a normal build enforces the recorded
pins.

### Tests

- `make test` runs `test/local`, `test/multinode`, and `test/result-socket`
- `make test/local` runs `stack test` in `compiler/` (all of the compiler's Haskell test suites)
  followed by the golden suite
- `bin/golden` to run the golden test suite with options
- `bin/golden -p <pattern>` to run tests matching a pattern (slashes are not allowed in patterns)
- `bin/golden --quick` to run only the optimized pass, skipping the `--no-rawopt` pass
- `bin/golden --no-color` compares against the `.nocolor.golden` files
- `make test/multinode` runs `scripts/run-multinode-tests.sh`
- `make test/prop-compiler`, `make test/prop-caseelim`, and `make test/prop-labelrt` run one
  compiler property suite each, for iterating without the full `stack test`
- `make test/prop-rt` (runtime lattice property tests) and `make test/prop-differential`
  (Haskell/TypeScript differential lattice harness) are not part of `make test`

For the test-suite layout and conventions, see [CONTRIBUTING.md](CONTRIBUTING.md#test-suite-layout).

### Running Troupe programs

```bash
./local.sh myprogram.trp            # local execution (no P2P, faster startup)
./network.sh myprogram.trp          # network execution (with P2P support)
./local.sh myprogram.trp --debug    # with debugging
```

### Development commands

```bash
make clean/rt                       # clean runtime build artifacts
cd compiler && make ghci/troupec    # interactive Haskell REPL for compiler development
cd compiler && make ghci/irtester   # interactive Haskell REPL for the IR tester
cd compiler && make parser-info     # parser info
```

### Running examples that do not require network

`local.sh` compiles the program to a temporary file and runs it with the runtime's `--localonly`
flag, which skips p2p network creation and key generation. Under `--localonly` all external I/O
operations yield a runtime error.

### Passing command-line arguments to Troupe programs

To pass arguments to a Troupe program, use `--` to separate runtime options from program arguments:

```bash
./local.sh myprogram.trp -- arg1 arg2 arg3
./network.sh myprogram.trp -- arg1 "argument with spaces" arg3
```

Arguments after `--` are accessible in the Troupe program using the `getCliArgs` built-in function, which requires root authority:

```sml
let val args = getCliArgs authority
in print args   (* ["arg1", "arg2", "arg3"] *)
end
```

Note: CLI arguments are treated as sensitive data and are labeled at the highest security level (ROOT). Only code with root authority can access them.

### Building and naming the snapshot

Script `dev-utils/build-snapshot.sh` runs `make` and copies the executables to
`bin/<git describe output>` (the current commit description via `git describe --long --dirty
--always`), so snapshots from different versions can be compared.

## Source Maps

The Troupe compiler can generate source maps to help with debugging by mapping generated JavaScript code back to the original Troupe source.

### Generating Source Maps

Use the `-m` or `--source-map` flag when compiling:

```bash
bin/troupec -m myprogram.trp -o myprogram.js
```

The source map is embedded in `myprogram.js` — as a non-enumerable `__sourceMap` property the
runtime reads for error reporting, and as a trailing base64 `//# sourceMappingURL=data:...` comment
for `node --enable-source-maps`. No separate `.map` file is written.

### Inspecting Source Maps

A tool is provided for inspecting generated source maps:

```bash
node rt/built/tools/inspect-sourcemap.js [--one-based] <file.js|file.js.map>
```

It accepts either a generated `.js` file with an embedded inline source map or a standalone JSON
`.map` file. `--one-based` (`-1`) displays columns as 1-based; the default is the 0-based source-map
spec indexing. It displays:
- Source map metadata (file, sources, version)
- All decoded mappings grouped by source file
- Line/column mappings from generated to original code
