# synvar-exchange

Two nodes declare an identical datatype group by copy-paste (no import) and
exchange variant values; node B matches them with its independently compiled
constructor patterns, so the test passes only if both compilations produce
identical constructor tags (content-addressed hashing). Node A also sends a
value of a datatype node B does not declare; it falls to a catch-all handler,
showing distinct declarations never collide.

Run: scripts/run-multinode-tests.sh -p synvar
