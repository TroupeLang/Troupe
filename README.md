# Troupe programming language

Troupe is a programming language based on the actor model for concurrent and distributed
programming that provides dynamic information flow control.

## Project components

Troupe consists of three main components:

1. **Compiler** (`compiler/`) — Haskell-based compiler that transforms Troupe code to JavaScript.
2. **Runtime** (`rt/`) — TypeScript/JavaScript runtime implementing the actor model and information flow control. The `rt/built/` folder is the build target for generated code.
3. **Standard library** (`lib/`) — built-in libraries written in Troupe.

### Additional tools

- **P2P tools** (`p2p-tools/`) — utilities for the distributed runtime, including a [libp2p](https://libp2p.io/) **relay** (`p2p-tools/relay/`). In multinode deployments the relay lets users communicate with Troupe nodes behind NAT.

### Repository layout

| Path         | Contents                                                                         |
|--------------|----------------------------------------------------------------------------------|
| `compiler/`  | Haskell compiler sources (`troupec`)                                             |
| `rt/`        | TypeScript runtime; `rt/built/` is the generated build output                    |
| `lib/`       | Troupe standard library (`.trp` sources)                                         |
| `trp-rt/`    | Service module placeholder (`service.trp`), built by `make trp-rt`               |
| `bin/`       | Compiled binaries — **not** version-controlled; do not add scripts here          |
| `scripts/`   | Version-controlled executable scripts                                            |
| `dev-utils/` | Developer helper scripts (e.g. `build-snapshot.sh`)                              |
| `tests/`     | Test corpus (see [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md#test-suite-layout)) |
| `p2p-tools/` | P2P utilities and relay                                                          |
| `notebook/`  | Notebook front end and back end, built by `make notebook`                        |
| `docs/`      | Project documentation                                                            |

## Quick start

With all [dependencies installed](docs/INSTALL.md), build everything and run a program:

```bash
make all                 # build compiler, runtime, libraries, service placeholder (trp-rt), and p2p-tools
./local.sh myprogram.trp # run a program locally (no P2P)
```

To try Troupe without a manual install, use the VSCode development container in the
[Troupe/example-project](https://github.com/TroupeLang/example-project) repository.

## Documentation

| Document                                     | Contents                                                              |
|----------------------------------------------|-----------------------------------------------------------------------|
| [docs/INSTALL.md](docs/INSTALL.md)           | Dependencies and step-by-step installation, including OS X setup      |
| [docs/DEVELOPMENT.md](docs/DEVELOPMENT.md)   | Editor setup, build/test commands, running programs, source maps      |
| [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) | Compilation pipeline, runtime architecture, IFC, file extensions      |
| [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md) | Test-suite layout and how to add a built-in function                  |
| [docs/NETWORKING.md](docs/NETWORKING.md)     | P2P runtime, libp2p, node discovery, relays                           |

## User guide

The current user guide is accessible [here](https://troupe.cs.au.dk/userguide.pdf). A
[Jupyter-book version](https://troupelang.github.io/troupe-user-guide-jb/) is also available.
See [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md#note-on-dc-labels) for a note on DC label syntax.
