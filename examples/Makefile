COMPILER=../bin/troupec
TERMINAL=open -a Terminal.app
TROUPERT=node --stack-trace-limit=100 ../rt/built/troupe.js 
OUT=../out
.PHONY: bin


compile.zero:
	$(COMPILER) zero.trp --output=$(OUT)/zero.js

run.zero.listener:
	$(TROUPERT) -f=$(OUT)/zero.js --id=../rt/src/p2p/id-listener.json --rspawn=true # --debug --debugp2p

run.dating.decentralized:
	$(TERMINAL) scripts/startserver.sh
	sleep 5
	$(TERMINAL) scripts/startalice.sh
	$(TERMINAL) scripts/startbob.sh
	$(TERMINAL) scripts/startcharlie.sh
	$(TERMINAL) scripts/startdorothy.sh
	@echo "run 'make kill' to terminate all the created process (assuming there are no other useful node processes running locally)"

kill:
	killall node

test.trust.compile.listener:
	$(COMPILER)	./tests/trust2.picox --output=$(OUT)/trust2.js
	$(COMPILER)	./tests/trust3.picox --output=$(OUT)/trust3.js


test.trust.run.listener:
	$(TROUPERT) -f=$(OUT)/trust2.js --id=../rt/src/p2p/id-listener.json
test.trust.run.dialer:
	$(TROUPERT) -f=$(OUT)/trust3.js
compile.pingpong:
	$(COMPILER) p2ppingpong.trp --output=$(OUT)/p2ppingpong.js -v
run.pingpong.dialer:
	$(TROUPERT) -f=$(OUT)/p2ppingpong.js --id=../rt/src/p2p/id-dialer.json --debug --debugp2p


compile.spawnremote:
	$(COMPILER) spawnremote.trp --output=$(OUT)/spawnremote.js

run.spawnremote.dialer:
	$(TROUPERT) -f=$(OUT)/spawnremote.js --id=../rt/src/p2p/id-dialer.json --debug --debugp2p
