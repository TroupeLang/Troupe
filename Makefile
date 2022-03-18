.PHONY: rt



COMPILER=./bin/troupec
# run the make of the compiler itself
stack:
	$(MAKE) -C compiler 

yarn:
	yarn install
rt:
	cd rt; tsc 
service:
	$(COMPILER) ./trp-rt/service.trp -l	
libs:
	$(COMPILER) ./lib/nsuref.trp -l
	$(COMPILER) ./lib/string.trp -l
	$(COMPILER) ./lib/printService.trp -l
	$(COMPILER) ./lib/lists.trp -l
	$(COMPILER) ./lib/declassifyutil.trp -l 
	$(COMPILER) ./lib/stdio.trp -l 
	$(COMPILER) ./lib/timeout.trp -l
	$(COMPILER) ./lib/raft.trp -l
	$(COMPILER) ./lib/raft_debug.trp -l
	$(COMPILER) ./lib/bst.trp -l	
	$(COMPILER) ./lib/localregistry.trp -l	

test:
	mkdir -p out
	cd compiler && $(MAKE) test

dist: stack yarn rt libs
	rm -rf ./build/
	mkdir -p ./build/Troupe/rt/built
	mkdir -p ./build/Troupe/bin
	cp -RP bin  ./build/Troupe
	cp -RL lib ./build/Troupe/
	cp -RL trustmap.json ./build/Troupe/trustmap.json
	cp -RL node_modules ./build/Troupe/node_modules
	# yarn run rollup --config
	cp -RL rt/built ./build/Troupe/rt/
	cp rt/troupe ./build/Troupe/rt/troupe
	cp local.sh ./build/Troupe/bin/local.sh
	cp network.sh ./build/Troupe/bin/network.sh
	cp -RL tests ./build/Troupe/
all:
	make stack 
	yarn
	make rt 
	make libs 
	make service
clear-built-rt:
	rm -rf rt/built

build-and-push-docker:
	docker build -t jbay/troupe . && docker push jbay/troupe

build-and-push-repo:
	docker build -t jbay/troupe git@github.com:aslanix/Troupe.git\#devraft && docker push jbay/troupe

