# troupe-ir-sexp conformance corpus

Recorded artifacts in the IR interchange format: the contract a second implementation is
developed and judged against, and a drift detector for this one. `data/` holds, per program:

| File            | What it is                                                              |
|-----------------|--------------------------------------------------------------------------|
| `<name>.sexp`   | the whole program as a `troupe-ir-sexp` document, positions included      |
| `<name>.blob`   | base64 of a `TRPI` blob carrying the program's largest function, gzipped by Haskell |
| `<name>.node.blob` | the same document, re-compressed by Node's `zlib` — a different compressor's bytes |
| `<name>.troupe.sexp` | the whole program as the Troupe implementation re-prints it |
| `<name>.troupe.blob` | written by the Troupe implementation, from IR it decoded itself — no part of it is this compiler's output |

The references are cut from programs in `tests/rt/pos` chosen to span the grammar: arithmetic and
recursion, both float spellings, variant tuples, a structured DC-label literal, lists.

## What they check

`ir-sexp-conformance-test` runs the interchange laws (`compiler/docs/spec-troupe-ir-sexp.md`) over
them: that the compiler still parses documents written down earlier (L2), that it still decodes
blobs written down earlier (L3), that its own framing round-trips (L4), and — the reason
`.node.blob` exists — that a blob compressed by a *different implementation* decodes to the same
IR. Blob bytes are never compared; only decoded values are.

`.troupe.sexp` and `.troupe.blob` close the other direction: both are produced by the second
implementation, from IR that implementation decoded itself, so a passing case means each side
accepts what the other writes — for whole programs as text and for a function as a blob.
`scripts/ir-sexp-troupe-conformance.sh` runs the same laws inside Troupe over the `.sexp` and both
recorded blobs, and holds it to the same corpus of malformed blobs the Haskell suite uses.

## Regenerating

```sh
IR_SEXP_REGENERATE=1 stack test Troupe-compiler:ir-sexp-conformance-test  # .sexp and .blob
node scripts/ir-blob-interchange.mjs                                    # .node.blob
./scripts/ir-sexp-troupe-conformance.sh --write-troupe-references        # .troupe.sexp and .troupe.blob
```

Run the second and third whenever the first changes anything: both are derived from the Haskell
blobs. `node scripts/ir-blob-interchange.mjs --check` verifies the Node ones are current without
rewriting, and is what `make test/local` runs.

**Reading a regenerated diff.** Text that merely moved — different line breaking, a different float
spelling, a different compressed length — is an implementation detail and carries no weight: layout
and compression are not part of the format. A reference that no longer *parses to the same value* is
a format change, and needs a version bump in `IRSexp.formatVersion` plus the changelog entry in the
spec.

Note the documents embed absolute source paths, because that is what the compiler puts in a
position. Regenerating in a different checkout will therefore rewrite every `.sexp` and `.blob`;
that is expected and is not a format change.
