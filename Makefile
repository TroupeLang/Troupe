.PHONY: rt



COMPILER=./bin/troupec
# run the make of the compiler itself
stack:
	$(MAKE) -C compiler 

yarn:
	yarn install
rt:
	cd rt; tsc 
libs:
	$(COMPILER) ./lib/lists.trp -l
	$(COMPILER) ./lib/declassifyutil.trp -l 
	$(COMPILER) ./lib/stdio.trp -l 
	$(COMPILER) ./lib/timeout.trp -l 

test:
	cd compiler && $(MAKE) test

dist: slack yarn libs
	rm -rf ./build/
	mkdir -p ./build/Troupe/rt/built
	mkdir -p ./build/Troupe/bin
	cp -RP ./bin  ./build/Troupe
	cp -RL lib ./build/Troupe/lib
	cp -RL trustmaps ./build/Troupe/trustmaps
	cp -RL trustmap.json ./build/Troupe/trustmap.json
	cp -RL node_modules ./build/Troupe/node_modules
	yarn run rollup --config
	cp local.sh ./build/Troupe/
	cp -RL ./tests ./build/Troupe/tests
all:
	make stack 
	yarn
	make rt 
	make libs 
