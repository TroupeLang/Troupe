# Troupe p2p Relay

This example is adapted from libp2p's Go relay and since then ported to
JavaScript's p2p implementation.

## Package

`p2p-tools/` is an npm workspace of the Troupe repository (`workspaces` in the
root `package.json`), named `troupe-p2p-tools`. Its `package.json` lists the
packages the relay, `mkid` and `mkaliases` import; their versions are fixed
by the root `package-lock.json`, the same versions the runtime uses. A root
`npm ci` installs them with everything else; `npm ci --workspace p2p-tools`
installs them alone, which is what the relay's Docker image does
(github.com/TroupeLang/troupe-relay).

## Build

The relay is written in TypeScript. To compile it to JavaScript, such that it
can run, please use the following *Make* target:

```bash
make build/relay
```

The relay needs a key pair to function; its public *id* has to be shared with
the Troupe nodes via the `default_relays` variable in
`rt/src/p2p/p2pconfig.mjs`. To generate this pair, use the corresponding target
as follows:

```bash
make build/keys
```

To remove the build artifact `relay.mjs` and the keys, use the following
target:

```bash
make clean
```

## How To Run

To start the relay server, run the corresponding *Make* target as follows:

```bash
make start
```

This starts the server with `basic` logging, including the respective 'keep
alive' messages sent from each Troupe node. To run silently, instead run

```bash
make start/silent
```

To start it with verbose information about all traffic, run the following
target:

```bash
make start/verbose
```

## Docker image

`Dockerfile` here builds the relay with the workspace's packages alone, on
`node:22-slim`; the build context is the repository root. `make image` builds
it for this machine, `make push` for linux/amd64 and linux/arm64 and pushes
it; `.github/workflows/publish_relay_image.yml` does the same on every push
to `dev` that touches `p2p-tools/` or the lockfile, tagging the image
`latest` and `troupe-<short commit id>` at
`ghcr.io/troupelang/troupe-relay`. The package on GHCR has to be set to
public once for hosts to pull it without a token.

A host runs the published image with `docker-compose.yml` here and nothing
else from the repository:

```bash
curl -O https://raw.githubusercontent.com/TroupeLang/Troupe/dev/p2p-tools/relay/docker-compose.yml
docker compose pull && docker compose up -d     # first start generates the key pair
docker compose logs relay                        # peer id and multiaddr templates
```

The key pair lives in the Docker volume `relay-keys`; the peer id is derived
from it, so a lost volume means a new relay address. Back it up with
`docker compose exec relay cat /app/keys/relay.id` and `.../relay.priv`.
`RELAY_PUBLIC_PORT=5556 docker compose up -d` publishes on another host port;
`RELAY_TAG=troupe-<id> docker compose up -d` runs a specific build. Update:
`docker compose pull && docker compose up -d`. On a fresh Ubuntu host:
`apt install -y docker.io docker-compose-v2`, `ufw allow 5555/tcp`, then the
commands above. The relay used about 55 MiB of memory in tests.

Nodes use the address the log prints:

```bash
network.sh program.trp --relay "/dns4/<HOST_NAME>/tcp/5555/ws/p2p/<PEER_ID>"
```

