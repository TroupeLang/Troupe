TROUPE_ROOT=../../..
MKID=node $(TROUPE_ROOT)/p2p-tools/built/mkid.mjs
MKALIASES=node $(TROUPE_ROOT)/p2p-tools/built/mkaliases.js
START=$(TROUPE_ROOT)/network.sh

# This channel level is corrupt -- more secret than it is trusted -- and has to
# be. What the example prints is confidential and untrusted at once: the endorsed
# reply is alice-confidential, and its integrity carries the peer's quarantine
# tag. Admitting both means confidentiality at least alice and integrity at most
# that tag, which is corruptness by definition. Measured on the running example:
# {alice} and <#null-confidentiality;#null-integrity> are both non-corrupt and
# both refuse it, from opposite directions. The tag is generated per run and per
# peer, so #null-integrity is the only integrity weak enough to write down.
# (the # in a label is escaped: make would read it as a comment)
STDIOLEV=--stdiolev='<\#root-confidentiality;\#null-integrity>'

# Server configuration
SERVER_PORT=6789
SERVER_CMD=$(START) qecho-server.trp $(STDIOLEV) --debugquarantine --disable-relay --id=ids/server.json --port=$(SERVER_PORT)

# Client configuration
CLIENT_CMD=$(START) qecho-client.trp $(STDIOLEV) --debugquarantine --disable-relay --id=ids/client.json --aliases=aliases.json --trustmap=client-trustmap.json

# Run both server and client together
# Usage: make run (or: make setup && make run)
run: check-setup
	@echo "Starting server in background..." && \
	$(SERVER_CMD) & SERVER_PID=$$!; \
	sleep 2; \
	echo "Starting client..."; \
	$(CLIENT_CMD); CLIENT_EXIT=$$?; \
	echo "Client finished, waiting for server..."; \
	wait $$SERVER_PID; SERVER_EXIT=$$?; \
	echo "Done. Server exit: $$SERVER_EXIT, Client exit: $$CLIENT_EXIT"; \
	exit $$CLIENT_EXIT

# Server: does NOT trust the client (no trustmap entry for client)
# This means information from the client will be downgraded to BOT level
server: check-setup
	$(SERVER_CMD)

# Client: trusts the server at level {alice}
# This allows the client to send {alice}-level data to the server
client: check-setup
	$(CLIENT_CMD)

# Full setup: create identifiers and trustmap
setup: create-network-identifiers create-trustmap
	@echo "Setup complete. Run 'make run' to start both nodes."

check-setup:
	@test -f ids/server.json || (echo "Run 'make setup' first" && exit 1)
	@test -f client-trustmap.json || (echo "Run 'make setup' first" && exit 1)

create-network-identifiers:
	mkdir -p ids
	$(MKID) --outfile=ids/server.json
	$(MKID) --outfile=ids/client.json
	$(MKALIASES) --include ids/server.json --include ids/client.json --outfile aliases.json

# Create trustmap: client trusts server at {alice} (DC label format: <alice;alice>)
create-trustmap: ids/server.json
	@SERVER_ID=$$(node -e "console.log(require('./ids/server.json').id)"); \
	echo "[{\"id\": \"$$SERVER_ID\", \"level\": {\"confidentiality\": [[\"alice\"]], \"integrity\": [[\"alice\"]]}}]" > client-trustmap.json
	@echo "Created client-trustmap.json"

clean:
	rm -rf ids/ aliases.json client-trustmap.json out/
	pkill -f "qecho-server.trp" 2>/dev/null || true

.PHONY: run server client setup check-setup create-network-identifiers create-trustmap clean
