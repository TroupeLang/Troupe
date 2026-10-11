help:
	cat README.md

start: start/basic

start/silent:
	node relay.mjs

start/basic:
	DEBUG=libp2p:circuit-relay:server node relay.mjs

start/verbose:
	DEBUG=libp2p:circuit-relay:server,libp2p:yamux:trace node relay.mjs

build: build/relay

build/keys:
	mkdir -p keys
	node $(TROUPE)/p2p-tools/built/mkid.mjs --privkeyfile=keys/relay.priv --idfile=keys/relay.id --verbose

build/relay:
	tsc

clean: clean/keys clean/relay

clean/keys:
	rm -rf keys

clean/relay:
	rm relay.mjs

# The relay image; build context is the repository root.
IMAGE     ?= ghcr.io/troupelang/troupe-relay
PLATFORMS ?= linux/amd64,linux/arm64

image:
	cd $(TROUPE) && docker build -f p2p-tools/relay/Dockerfile -t $(IMAGE):latest .

# Multi-arch, pushed to the registry; needs `docker login ghcr.io`.
push:
	cd $(TROUPE) && docker buildx build -f p2p-tools/relay/Dockerfile --platform $(PLATFORMS) \
	  -t $(IMAGE):latest -t $(IMAGE):troupe-$$(git rev-parse --short HEAD) --push .
