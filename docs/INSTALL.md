# Installing Troupe

> **Scope:** how to obtain dependencies and build Troupe for the first time. For build, test, and
> run commands once installed, see [DEVELOPMENT.md](DEVELOPMENT.md).

## Troupe development container

If you want to try out Troupe without manual installation (e.g., for a class exercise or just checking the system), check out the VSCode development container available through the [Troupe/example-project](https://github.com/TroupeLang/example-project) repository.

## Installation

Once all dependencies have been installed, the whole project can be built and Troupe installed to the `bin` directory with `make all`. The following shows step-by-step which dependencies are needed for which parts and how to install them.

### Step 1. Install JS runtime
1. Install NodeJS (e.g. `sudo apt-get install nodejs`)
2. Install js dependencies via `npm install`
3. Set the `TROUPE` environment variable to point to the folder that contains the main README. In bash this is done by adding the following lines to a file such as `~/.bashrc` or `~/.bash_profile`:
   ```
   TROUPE=<path to the installation directory>
   export TROUPE=<path to the installation directory>
   ```
   Read [here](https://www.digitalocean.com/community/tutorials/how-to-read-and-set-environmental-and-shell-variables-on-a-linux-vps") for more info on environment variables.
6. Install [TypeScript](https://www.typescriptlang.org/): `npm install -g typescript`
   - To install to the home directory without root, first run `npm config set prefix ~/.npm` and add `~/.npm/bin` to your PATH
7. Compile Troupe runtime by typing `make rt`

### Step 2. Install Troupe compiler

1. Get [Haskell stack](https://www.haskellstack.org).
2. Change to the `compiler` directory and run `make`

The above make script copies the binary of the compiler into the
bin folder of the project under name `troupec`. That name is then used
by the runtime module.


### Step 3. Install Troupe standard library

Type

- `make lib` to compile Troupe's built-in libraries into `lib/out`, and
- `make trp-rt` to compile the service module placeholder from `trp-rt/service.trp`.

Both require `bin/troupec` from step 2.

Steps 1–3 above can be run together from the repository root with `make all` (the default goal, so
a bare `make` does the same), which runs `npm install` and `npm install -g typescript` and then
builds the compiler, runtime, service placeholder (`trp-rt`), p2p-tools, and libraries.


### Step 4. Running the test suite

#### OS X specific utilities for testing

On OS X, make sure to have `gtimeout`, `greadlink`, and GNU `diff` utilities. 

- `gtimeout` and `greadlink` can be installed via `brew install coreutils`
- GNU `diff` can be installed via `brew install diffutils`

The GNU diff is required because Troupe's test suite relies on specific diff features not available in the default macOS diff. After installation, verify that GNU diff is available:

```bash
diff --version
```

The first line must name GNU diffutils, for example:
```
diff (GNU diffutils) 3.10
```

#### Checking the installation

Check that the installation works by running the golden test suite: `$TROUPE/bin/golden`.

`make test` runs the golden suite together with the compiler's own test suites, the multinode
tests, the hostile-peer tests, and the result-socket tests.

#### Multinode tests

Multinode tests are located in `tests/rt/multinode-tests/` and can be run using the script
`scripts/run-multinode-tests.sh` (also `make test/multinode`).
