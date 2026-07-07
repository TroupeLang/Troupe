/**
 * Corpus generator for the differential lattice harness.
 *
 * Emits JSON lines to stdout, each a case for the two judges. Uses fast-check
 * with fixed committed seeds so the corpus is deterministic and replayable.
 *
 * Two kinds of case are emitted, in this order:
 *
 *   1. CNF cases (Step 3): `{"x": [[..]], "y": [[..]]}` — a pair of CNFs
 *      encoded as `string[][]`. Shape: principals from the 5-element universe;
 *      clause count 0-4, clause size 0-4; empty clauses (FALSE) and the empty
 *      CNF (TRUE) included on purpose (the TRUE/FALSE boundary is where the two
 *      implementations tend to diverge). These lines carry no `kind` field;
 *      both judges treat a missing `kind` as `cnf`, so the Step 3 corpus is
 *      byte-for-byte unchanged.
 *
 *   2. V1 cases (Step 4c): `{"kind":"v1","raw":"{ BOB, alice }","canon":"alice,bob"}`
 *      — two V1 label surface strings. `raw` is a tag list rendered with random
 *      whitespace, casing, duplication, and optional (balanced) braces; `canon`
 *      is the same-or-different tag set rendered plainly (sorted, unique,
 *      lowercase). Both judges answer the SAME boolean: "does `raw` denote the
 *      same label as `canon`?" — Haskell via `v1LabelEq`, TS via
 *      `fromV1String(raw).equals(fromV1String(canon))`. The empty V1 label
 *      (`{}` / `` = IFC_BOT) is included on purpose.
 *
 * Run: node rt/built/proptests/tools/gen-corpus.mjs > corpus.jsonl
 */
import fc from 'fast-check'

const PRINCIPALS = ['alice', 'bob', 'charlie', 'dorothy', 'eve'] as const

const arbPrincipal = fc.constantFrom(...PRINCIPALS)

// ---------------------------------------------------------------------------
// CNF cases (Step 3) — unchanged: same seed, same shape, same byte output.
// ---------------------------------------------------------------------------

const CNF_SEED = 20260707
const CNF_N = 10000

// clause size 0-4: minLength 0 permits the empty (FALSE) clause.
const arbClause = fc.array(arbPrincipal, { minLength: 0, maxLength: 4 })
// clause count 0-4: minLength 0 permits the empty (TRUE) CNF.
const arbCNFClauses = fc.array(arbClause, { minLength: 0, maxLength: 4 })
const arbCNFCase = fc.record({ x: arbCNFClauses, y: arbCNFClauses })

const cnfCases = fc.sample(arbCNFCase, { numRuns: CNF_N, seed: CNF_SEED })
const cnfLines = cnfCases.map(c => JSON.stringify({ x: c.x, y: c.y }))

// ---------------------------------------------------------------------------
// V1 cases (Step 4c).
// ---------------------------------------------------------------------------

const V1_SEED = 20260708
const V1_N = 10000
const MAXTAGS = 5

// A V1 case is described by primitives sampled from fast-check, then rendered
// deterministically. `rawTags` is the multiset rendered messily into `raw`;
// `canonTags` (equal to `rawTags` when `sameCanon`, else `otherTags`) is
// rendered plainly into `canon`. Independent draws over a 5-principal universe
// make both the equal and unequal answers common.
const arbV1Case = fc.record({
    rawTags: fc.array(arbPrincipal, { minLength: 0, maxLength: MAXTAGS }),
    sameCanon: fc.boolean(),
    otherTags: fc.array(arbPrincipal, { minLength: 0, maxLength: MAXTAGS }),
    brace: fc.boolean(),
    innerSpace: fc.nat(3),
    // Per-occurrence decoration, sized to the max tag count.
    padL: fc.array(fc.nat(3), { minLength: MAXTAGS, maxLength: MAXTAGS }),
    padR: fc.array(fc.nat(3), { minLength: MAXTAGS, maxLength: MAXTAGS }),
    caseMask: fc.array(fc.nat(0x7fffffff), { minLength: MAXTAGS, maxLength: MAXTAGS }),
})

const sp = (n: number) => ' '.repeat(n)

// Random-case each character of `tag` according to the bits of `mask`.
function recase(tag: string, mask: number): string {
    let out = ''
    for (let j = 0; j < tag.length; j++) {
        const up = (mask >> (j % 31)) & 1
        out += up ? tag[j].toUpperCase() : tag[j].toLowerCase()
    }
    return out
}

// Render `raw`: messy comma-joined tags, with random spacing/casing and an
// optional pair of *balanced* braces. Two rendering invariants keep `raw`
// inside the surface syntax both parsers agree on (the property under test is
// V1-normalization equivalence, not parser robustness on malformed input):
//
//   - When braced, `{` is the first character and `}` is the last, with no
//     whitespace outside the braces. The Haskell parser strips a leading `{` /
//     trailing `}` via dropWhile without a prior whole-string trim, so
//     "  {alice}  " and "{alice}" are NOT equivalent to it; the balanced,
//     tight-brace form avoids that.
//   - The empty tag set renders as exactly `{}` (braced) or `` (unbraced) --
//     never `{  }`. Haskell reads whitespace-only brace contents as a single
//     empty-string tag, whereas the runtime reads it as IFC_BOT; the canonical
//     empty forms named in the spec (`{}` / ``) agree on both sides.
function renderRaw(c: {
    rawTags: readonly string[]; brace: boolean; innerSpace: number
    padL: readonly number[]; padR: readonly number[]; caseMask: readonly number[]
}): string {
    const parts = c.rawTags.map((tag, i) =>
        sp(c.padL[i]) + recase(tag, c.caseMask[i]) + sp(c.padR[i]))
    const inner = parts.join(',')
    if (c.brace) {
        return inner === '' ? '{}' : '{' + sp(c.innerSpace) + inner + sp(c.innerSpace) + '}'
    }
    return inner
}

// Render `canon`: the plain, canonical form — sorted, de-duplicated, lowercase,
// comma-joined, no braces, no spaces. The empty set renders to "".
function renderCanon(tags: readonly string[]): string {
    return [...new Set(tags.map(t => t.toLowerCase()))].sort().join(',')
}

const v1Cases = fc.sample(arbV1Case, { numRuns: V1_N, seed: V1_SEED })
const v1Lines = v1Cases.map(c => {
    const raw = renderRaw(c)
    const canonTags = c.sameCanon ? c.rawTags : c.otherTags
    const canon = renderCanon(canonTags)
    return JSON.stringify({ kind: 'v1', raw, canon })
})

process.stdout.write(cnfLines.concat(v1Lines).join('\n') + '\n')
