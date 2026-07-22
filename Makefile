.PHONY: rt trp-rt compiler lib p2p-tools npm clean test dist check-compiler notebook

# TODO: Rename to 'build/*' ?
all: npm compiler rt trp-rt p2p-tools lib

check-compiler:
	@if [ ! -x ./bin/troupec ]; then \
		echo "Error: Compiler not built. Run 'make compiler' first." >&2; \
		exit 1; \
	fi

npm:
	npm install
	npm install -g typescript

rt:
	cd rt; $(MAKE) all

COMPILER=./bin/troupec
compiler:
	cd compiler; $(MAKE) all

p2p-tools:
	cd p2p-tools; tsc
	cd p2p-tools/relay; tsc

lib: check-compiler
	cd lib; $(MAKE) build

trp-rt: check-compiler
	cd trp-rt/; $(MAKE) build

notebook:
	cd notebook; npm install; npm run build

# Publish every benchmark descriptor module and the report renderer as
# SimpleModule blobs (required by the suites' runall.trp drivers and the
# per-benchmark drivers). Blobs are compiled artifacts: republish after any
# compiler change, or a comparison silently measures stale code.
benchmark-modules:
	mkdir -p out
	./local.sh examples/savina/Savina.mod.trp
	./local.sh examples/savina/SavinaReport.mod.trp
	./local.sh examples/benchmarks/awfy/Awfy.mod.trp
	./local.sh examples/benchmarks/datastructures/DataStructures.mod.trp
	./local.sh examples/benchmarks/clbg/Clbg.mod.trp
	./local.sh examples/benchmarks/labeled-savina/LabeledSavina.mod.trp

savina-modules: benchmark-modules

clean: clean/compiler clean/rt clean/trp-rt clean/p2p-tools clean/lib
clean/compiler:
	cd compiler; $(MAKE) clean
clean/rt:
	cd rt; $(MAKE) clean
clean/trp-rt:
	cd trp-rt; $(MAKE) clean
clean/p2p-tools:
	cd p2p-tools; $(MAKE) clean
clean/lib:
	cd lib; $(MAKE) clean

ci-test-golden-no-color:
	mkdir -p out 
	./bin/golden --no-color

test: test/local test/multinode test/result-socket

# Test target for Docker runner (no Haskell toolchain available).
test/docker: ci-test-golden-no-color test/multinode test/result-socket

test/local:
	mkdir -p out
	cd compiler && $(MAKE) test

test/prop-compiler:
	cd compiler && stack test :dclabels-prop-test $(STACK_OPTS)
test/prop-caseelim:
	cd compiler && stack test :caseelim-prop-test $(STACK_OPTS)
test/prop-labelrt:
	cd compiler && stack test :labelrt-prop-test $(STACK_OPTS)
test/multinode:
	./scripts/run-multinode-tests.sh
test/libp2p-migration:
	./scripts/run-libp2p-migration-tests.sh
test/libp2p-migration-verbose:
	./scripts/run-libp2p-migration-tests.sh -v
test/ci-network: rt p2p-tools
	@echo "Running CI network test..."
	./tests/ci-network-test.sh
test/result-socket: test/result-socket-socat test/result-socket-node
test/result-socket-socat:
	@if command -v socat >/dev/null 2>&1; then \
		bash tests/rt/result-socket/test-result-socket.sh; \
	else \
		echo "SKIP: socat not installed"; \
	fi
test/result-socket-node:
	node tests/rt/result-socket/test-result-socket.mjs

# Runtime lattice property tests (fast-check + node:test). Not part of the
# aggregate `test:` target.
test/prop-rt: rt
	node --test 'rt/built/proptests/**/*.test.mjs'

# Differential Haskell<->TypeScript lattice harness. Not part of the aggregate
# `test:` target. Requires the `dclabels` executable and the runtime built.
test/prop-differential:
	./scripts/run-differential-lattice.sh

test/ci-relay: p2p-tools
	@echo "Running CI relay test..."
	./tests/ci-relay-test.sh

dist: stack npm rt p2p-tools lib
	rm -rf ./build/
	mkdir -p ./build/Troupe/rt/built
	mkdir -p ./build/Troupe/p2p-tools/built
	mkdir -p ./build/Troupe/bin
	cp -RP bin  ./build/Troupe
	cp -RL lib ./build/Troupe/
	cp -RL trustmap.json ./build/Troupe/trustmap.json
	cp -RL node_modules ./build/Troupe/node_modules
	cp -RL rt/built ./build/Troupe/rt/
	cp -RL p2p-tools/built ./build/Troupe/p2p-tools/
	cp rt/troupe ./build/Troupe/rt/troupe
	cp local.sh ./build/Troupe/bin/local.sh
	cp network.sh ./build/Troupe/bin/network.sh
	cp -RL tests ./build/Troupe/

build-and-push/docker:
	docker build -t jbay/troupe . && docker push jbay/troupe

build-and-push/repo:
	docker build -t jbay/troupe git@github.com:aslanix/Troupe.git\#devraft && docker push jbay/troupe
