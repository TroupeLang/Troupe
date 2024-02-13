# Troupe programming language

Troupe is a programming language based on the actor model for concurrent and distributed
programming that provides dynamic information flow control.

## Troupe development container

If you want to try out Troupe without manual installation (e.g., for a class exercise or just checking the system), please check out the VSCode development container available through the [Troupe/example-project](https://github.com/TroupeLang/example-project) repository.

## Installation

Once all dependencies have been installed, the whole project can be built and Troupe installed to the `bin` directory with `make all`. The following shows step-by-step which dependencies are needed for which parts and how to install them.

### Step 1. Install JS runtime
1. Install NodeJS (e.g. `sudo apt-get install nodejs`)
2. Get [yarn](https://yarnpkg.com/lang/en/) package manager (e.g. `npm install --global yarn`)
3. Install js dependencies via `yarn install`
4. Apply local js patches to the dependencies via `yarn patch-package` (might already be executed by `yarn install`)
5. Set the `TROUPE` environment variable to point to the folder that contains this README. In bash this is done by adding the following lines to a file such as `~/.bashrc` or `~/.bash_profile`:
   ```
   TROUPE=<path to the installation directory>
   export TROUPE=<path to the installation directory>
   ```
   Read <a href="https://www.digitalocean.com/community/tutorials/how-to-read-and-set-environmental-and-shell-variables-on-a-linux-vps"> here</a> for more info on environment variables.
6. Install [TypeScript](https://www.typescriptlang.org/): `npm install -g typescript`
   - To install to the home directory without root, first run `npm config set prefix ~/.npm` and add `~/.npm/bin` to your PATH
7. Compile Troupe runtime by typing `make rt`

### Step 2. Install Troupe compiler

1. Get [Haskell stack](https://www.haskellstack.org).
2. Change to the `compiler` directory and run `make`

The above make script copies the binary of the compiler into the
bin folder of the project under name `troupec`. That name is then used
by the runtime module.


### Step 3. Install Troupe top-level scripts

Type `make stack` (in the repository's root) to compile Troupe's bin scripts

### Step 4. Install Troupe standard library

Type

- `make libs` to compile Troupe's built-in libraries, and
- `make service` to compile the service module placeholder.


### Step 5. Running the test suite

#### Utilities for testing

On OS X, make sure to have `gtimeout` and `greadlink` utilities. These can be installed via `brew install coreutils`.

#### Checking the installation

Check that the installation works by running the local test suite: `$TROUPE/bin/golden`
(alternatively `make test` in this directory).


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

Now, when having opened the `compiler` folder, the Haskell Language Server should highlight errors and hints, support "Go to definition" and more.


#### Syntax support for Troupe files

As Troupe syntax is similar to SML, installing the [SML Environment](https://marketplace.visualstudio.com/items?itemName=vrjuliao.sml-environment) extension is useful. It adds syntax highlighting, some identation support and support for commenting with editor commands.

Use `Ctrl-k m` ("Change language mode") to set the current file's language mode to SML. The suggestions will also allow to generally associate `.trp` files with SML mode.


#### Building and running

- **Building the compiler:** When having opened the `compiler` folder, there is a task "Build all", which is set as the default build task, so running "Run build task" (Ctrl-F9) should execute it.
- **Running a Troupe file locally:** When having the Troupe root folder opened, there is a task "Run local" which runs `local.sh` with the currently focused file. It is set to the default test task, so "Run Test Task" should execute it. You may want to set a keybinding.
- Tasks are defined in the respective `.vscode/tasks.json` file, where further tasks can be added easily.

<!-- #### Makefile support -->
<!-- - Install the extension [Makefile Tools](https://marketplace.visualstudio.com/items?itemName=ms-vscode.makefile-tools) -->
<!-- - Open the Troupe root folder and select the Makefile tab in the left bar -->
<!-- - Set "Build target" to "all" -->

## User guide

The current user guide is accessible [here](https://troupe.cs.au.dk/userguide.pdf).

## Building and running
### Building

The following commands build specific parts of the project and install the results to the `bin`, `rt/built` and `lib` directories.

- `make all`: build everything (use this whenever significant changes have been made to the project, to be sure that everything is up-to-date)
- `make` / `make stack`: build the compiler
- `make rt`: build the runtime (into the `rt/built` directory)
- `make libs`: compile Troupe's built-in libraries (into the `lib` directory)
- `make service` compile the service module placeholder

### Tests

- `make test` to run all tests
- `bin/golden` to run the test suite with options

### Running examples that do not require network

For programs that do not require network access, there is a convenient script
`local.sh` that prompts the Troupe runtime to skip initialization of the p2p
infrastructure or key generation (which otherwise takes a few seconds).

### Building and naming the snapshot

Script `dev-utils/build.sh` runs `make` and copies the executables to `../bin/<current git HEAD hash>`.
This is useful when wanting to compile some snapshots to compare how different versions behave.

## Networking

When the program starts, once the keys are loaded (or generated), the runtime starts by connecting to
a number of nodes to bootstrap its discovery. This takes an observable amount of time.


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

The main p2p runtime module is in [rt/src/p2p/p2p.ts](rt/src/p2p/p2p.ts).


## References

### How node discovery works.
See [this ](https://github.com/libp2p/js-libp2p/tree/master/examples/discovery-mechanisms) and [this](https://github.com/libp2p/js-libp2p/tree/master/examples/peer-and-content-routing) examples for how the discovery works. We also make use of websocket-star transport/discovery, which
proxies the connections in case one of the machines is behind NAT. We currently use one of the third-party default websocket-star-rendezvous servers for convenience, but in the future we can set up our own  rendezvous server running
using [this package](https://github.com/libp2p/js-libp2p-websocket-star-rendezvous).
