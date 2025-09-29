.PHONY: rt compiler lib p2p-tools

# TODO: Rename to 'build/*' ?
all: npm rt compiler p2p-tools lib service

npm:
	npm install
	npm install -g typescript

rt:
	cd rt; $(MAKE) all

COMPILER=./bin/troupec
compiler:
	cd compiler; $(MAKE) build

p2p-tools:
	cd p2p-tools; tsc

lib:
	cd lib; $(MAKE) build

service:
	mkdir -p ./trp-rt/out
	$(COMPILER) ./trp-rt/service.trp -l

# TODO: Rename to 'clean/*' ?
clean: clean/compiler clean/rt clean/lib
clean/compiler:
	cd compiler; $(MAKE) clean
clean/rt:
	cd rt; $(MAKE) clean
clean/p2p-tools:
	cd p2p-tools; $(MAKE) clean
clean/lib:
	cd lib; $(MAKE) clean

ci-test-golden-no-color:
	mkdir -p out 
	./bin/golden --no-color

test: test/local test/multinode
test/local:
	mkdir -p out
	cd compiler && $(MAKE) test
test/multinode:
	./scripts/run-multinode-tests.sh
test/libp2p-migration:
	./scripts/run-libp2p-migration-tests.sh
test/libp2p-migration-verbose:
	./scripts/run-libp2p-migration-tests.sh -v
test/ci-network: rt p2p-tools
	@echo "Running CI network test..."
	./tests/ci-network-test.sh
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
