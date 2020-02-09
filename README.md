# Troupe programming language

Troupe is a programming language based on the actor model for concurrent and distributed
programming that provides dynamic information flow control.


## Installation


### Step 1. Install JS runtime
1. Install NodeJS.
2. Get [yarn](https://yarnpkg.com/lang/en/) package manager.
3. Install js dependencies via `yarn install`
4. Apply local js patches to the dependencies via `yarn patch-package`
5. Set TROUPE environment variable to point to the folder that contains this file (In bash this is done by adding a new line with the command `TROUPE=<path to the installation directory>` in a file such as `~/.bashrc` or `~/.bash_profile`). Read <a href="https://www.digitalocean.com/community/tutorials/how-to-read-and-set-environmental-and-shell-variables-on-a-linux-vps"> here</a> for more info on environment variables.
6. Install [TypeScript](https://www.typescriptlang.org/).
7. Compile Troupe runtime my typing `make rt`

### Step 2. Install Troupe compiler

1. Get [Haskell stack](https://www.haskellstack.org).
2. `stack install alex happy`
3. Change to the compiler directory and run `make`

The above make script copies the binary of the compiler into the
bin folder of the project under name `troupec`. That name is then used
by the runtime module.


### Step 3. Install Troupe top-level scripts

Type `make slack` to compile Troupe's bin scripts

### Step 4. Install Troupe standard library

Type `make libs` to compile Troupe's built-in libraries.


### Step 5. Running the test suite

#### Utilities for testing

On OS X, make sure to have `gtimeout` and `greadlink` utilities. These can be installed via `brew install coreutils`.

#### Checking the installation

Check that the installation works by running the local test suite: `$TROUPE/bin/golden`
(alternatively `make test` in this directory).

## User guide

LaTeX sources for the user guide are in the [user-guide](https://github.com/TroupeLang/Troupe-user-guide) repo. A (possibly outdated) PDF version is   also accessible  [here](http://lbs-troupe.troupe-lang.org/download/troupe-user-guide.pdf).


## Running examples that do not require network

For programs that do not require network access, there is a convenient script
`local.sh` thatprompts the  Troupe runtime to skip initialization of the p2p
infrastructure or key generation (which otherwise takes a few seconds).

## Networking

When the program starts, once the keys are loaded (or generated), the runtime starts by connecting to
a number of nodes to bootstraps its discovery. This takes an observable amount of  time.


### Local only mode
To skip network connection, one can provide `--localonly` flag to the runtime, but
observe that in this case all external I/O will result
in a runtime error.


### Generating new persistent IDs
See [rt/src/p2p/mkid.js](rt/src/p2p/mkid.js).

### Auto-created IDs

If the id file is omitted, a new id (via a fresh key/pair) is generated upon
start. Observe that this induces a bigger runtime overhead than loading a key pair
from a file.

### Stability issues and patches.

Libp2p is a fast-moving project, and there are stability issues. We
apply local patches to work around these issues (hence the patch application in the installation of JS runtime), but this should be used on a temporary
basis only and removed once the actual bugs are fixed (either in the official
  packages or in our codebase).


### Notes on the p2p runtime

The p2p runtime is implemented using [libp2p](https://libp2p.io/) library (part of IPFS project). This
means that nodes at runtime are now libp2p nodes (this is good for functionality
and terrible for anonymity).  We inherit from libp2p that every node has an
associated pair of public/private keys, and an id of the node is the hash of its
public key.

We use libp2p's functionality of relaying messages. This means that programs or
processes running behind NAT (e.g., something running on a developer laptop) are
accessible from the outside. Hopefully, this should facilitate development of
more examples (this is the main reason for trying out this networking
transport).


### Navigating the code base

The main p2p runtime module is in [rt/src/p2p/p2p.js](rt/src/p2p/p2p.js).


## References

### How node discovery works.
See [this ](https://github.com/libp2p/js-libp2p/tree/master/examples/discovery-mechanisms) and [this](https://github.com/libp2p/js-libp2p/tree/master/examples/peer-and-content-routing) examples for how the discovery works. We also make use of websocket-star transport/discovery, which
proxies the connections in case one of the machines is behind NAT. We currently use one of the third-party default websocket-star-rendezvous servers for convenience, but in the future we can set up our own  rendezvous server running
using [this package](https://github.com/libp2p/js-libp2p-websocket-star-rendezvous).
