MKID=node $(TROUPE)/rt/built/p2p/mkid.js
MKALIASES=node $(TROUPE)/rt/built/p2p/mkaliases.js
START=$(TROUPE)/network.sh

echo-server:

	$(START) echo-server.trp --id=ids/echo-server.json --debug --debugp2p

echo-client:
	$(START) echo-client.trp --id=ids/echo-client.json --aliases=aliases.json --debug --debugp2p

create-network-identifiers:
	mkdir -p ids 
	$(MKID) --outfile=ids/echo-server.json
	$(MKID) --outfile=ids/echo-client.json
	$(MKALIASES) --include ids/echo-server.json --include ids/echo-client.json --outfile aliases.json
