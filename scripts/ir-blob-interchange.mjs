// The Node half of the IR blob interchange check.
//
// A blob is "TRPI", a version byte, and a gzip stream of a troupe-ir-sexp
// document. The claim the format rests on is that the compression layer is
// interchangeable without being identical: Haskell's zlib, Node's zlib and a
// browser's CompressionStream all produce gzip streams that any of them can
// read. Nothing about that is worth assuming, so it is checked here in both
// directions.
//
//   Haskell -> Node : decode each checked-in .blob, verify its framing, and
//                     gunzip it to a document this side can read.
//   Node -> Haskell : re-compress that document with Node's zlib and write
//                     <name>.node.blob, which ir-sexp-conformance-test decodes.
//
// Run from the repository root:  node scripts/ir-blob-interchange.mjs
// Add --check to verify without rewriting the .node.blob references.

import { readFileSync, writeFileSync, readdirSync } from 'node:fs';
import { gunzipSync, gzipSync } from 'node:zlib';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = dirname(dirname(fileURLToPath(import.meta.url)));
const dir = join(root, 'compiler', 'test', 'ir-sexp-conformance', 'data');
const checkOnly = process.argv.includes('--check');

const FORMAT_ID = 'TRPI';
const VERSION = 2;

function unpack(b64) {
  const raw = Buffer.from(b64.trim(), 'base64');
  const id = raw.subarray(0, 4).toString('latin1');
  if (id !== FORMAT_ID) throw new Error(`bad format identifier ${JSON.stringify(id)}`);
  if (raw[4] !== VERSION) throw new Error(`unexpected blob version ${raw[4]}`);
  return { header: raw.subarray(0, 5), payload: raw.subarray(5) };
}

let checked = 0;
let mismatched = 0;

for (const entry of readdirSync(dir).sort()) {
  // Only the Haskell-written references are inputs here: the .node.blob files
  // are this script's own output, and the .troupe.blob files are the second
  // implementation's.
  if (!entry.endsWith('.blob')) continue;
  if (entry.endsWith('.node.blob') || entry.endsWith('.troupe.blob')) continue;
  const name = entry.slice(0, -'.blob'.length);
  const { header, payload } = unpack(readFileSync(join(dir, entry), 'utf8'));

  // Haskell -> Node: their gzip stream must be readable here, and what comes
  // out must be a document of the version the framing claims.
  const text = gunzipSync(payload).toString('utf8');
  // Whitespace-tolerant on purpose: layout is not part of the format, and the
  // Haskell printer happens to break the line after the head symbol.
  if (!new RegExp(String.raw`^\(\s*troupe-ir-sexp\s+${VERSION}\b`).test(text)) {
    throw new Error(`${entry}: payload is not a version-${VERSION} document: `
                    + JSON.stringify(text.slice(0, 40)));
  }
  if (!/\(\s*@\s/.test(text)) {
    throw new Error(`${entry}: payload carries no source positions`);
  }

  // Node -> Haskell: the same document, compressed by a different
  // implementation. The bytes are expected to differ; the content must not.
  const nodePayload = gzipSync(Buffer.from(text, 'utf8'), { level: 9 });
  const roundTripped = gunzipSync(nodePayload).toString('utf8');
  if (roundTripped !== text) throw new Error(`${entry}: node gzip did not round-trip`);
  if (nodePayload.equals(payload)) {
    // Not a failure — just means nothing is being proved about cross-producer
    // decoding for this input, so say so rather than let it pass silently.
    console.log(`  ${name}: node and haskell gzip bytes are identical (nothing proved here)`);
  } else {
    mismatched += 1;
  }

  const outPath = join(dir, `${name}.node.blob`);
  const outText = Buffer.concat([header, nodePayload]).toString('base64') + '\n';
  if (checkOnly) {
    const existing = readFileSync(outPath, 'utf8');
    if (existing.trim() !== outText.trim()) {
      throw new Error(`${outPath} is stale; re-run without --check`);
    }
  } else {
    writeFileSync(outPath, outText);
  }
  checked += 1;
  console.log(`  ${name}: haskell gzip ${payload.length} B -> node gzip ${nodePayload.length} B, `
              + `${text.length} B of document`);
}

console.log(`${checkOnly ? 'checked' : 'wrote'} ${checked} blob(s); `
            + `${mismatched} had compressor-specific bytes`);
if (checked === 0) throw new Error(`no .blob references in ${dir}`);
