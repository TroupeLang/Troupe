/**
 * Corpus generator for the differential lattice harness (Step 3).
 *
 * Emits N JSON lines `{"x": [[..]], "y": [[..]]}` to stdout, each a pair of
 * CNFs encoded as `string[][]`. Uses fast-check with a fixed committed seed so
 * the corpus is deterministic and replayable.
 *
 * Shape (matches the Step 3 spec):
 *   - principals drawn from the 5-element universe used in steps 1-2
 *   - clause count 0-4, clause size 0-4
 *   - empty clauses (FALSE) and the empty CNF (TRUE) are included on purpose:
 *     the TRUE/FALSE boundary is exactly where the two implementations tend to
 *     diverge.
 *
 * Run: node rt/built/proptests/tools/gen-corpus.mjs > corpus.jsonl
 */
import fc from 'fast-check'

const PRINCIPALS = ['alice', 'bob', 'charlie', 'dorothy', 'eve'] as const

/** Fixed, committed seed. Changing it reshuffles the whole corpus. */
const SEED = 20260707
const N = 10000

const arbPrincipal = fc.constantFrom(...PRINCIPALS)
// clause size 0-4: minLength 0 permits the empty (FALSE) clause.
const arbClause = fc.array(arbPrincipal, { minLength: 0, maxLength: 4 })
// clause count 0-4: minLength 0 permits the empty (TRUE) CNF.
const arbCNFClauses = fc.array(arbClause, { minLength: 0, maxLength: 4 })
const arbCase = fc.record({ x: arbCNFClauses, y: arbCNFClauses })

const cases = fc.sample(arbCase, { numRuns: N, seed: SEED })

const chunks: string[] = cases.map(c => JSON.stringify({ x: c.x, y: c.y }))
process.stdout.write(chunks.join('\n') + '\n')
