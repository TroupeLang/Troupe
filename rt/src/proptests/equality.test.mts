/**
 * Step 4d: EqualityChecker laws.
 *
 * `runtimeEquals` (rt/src/EqualityChecker.mts) must be an equivalence relation
 * over the 4b value space, and must be consistent with serialization:
 *
 *   - reflexive, symmetric, transitive;
 *   - self-equality survives a serialization round-trip;
 *   - equality is preserved under round-trip in BOTH directions (equal copies
 *     stay equal; distinct values stay distinct — inequality preservation).
 *
 * Piggybacks entirely on the 4b arbitraries (value-arbitraries.mts). The pure
 * laws (reflexive/symmetric/transitive) need no runtime; the consistency laws
 * use the round-trip harness (serde-harness.mts).
 */
import { test } from 'node:test'
import assert from 'node:assert'
import fc from 'fast-check'
import { roundTrip } from './serde-harness.mjs'
import {
    arbLVal,
    arbTriple,
    arbMaybeEqualPair,
    rtEqual,
    cloneLVal,
} from './value-arbitraries.mjs'

const PURE_RUNS = 1000
const RT_RUNS = 500

// --- equivalence relation (pure) -------------------------------------------

test('4d: reflexive — rtEqual(v, v)', () => {
    fc.assert(fc.property(arbLVal, (v) => {
        assert.ok(rtEqual(v, v), 'value not equal to itself')
    }), { numRuns: PURE_RUNS })
})

test('4d: reflexive on a distinct-but-equal clone — rtEqual(v, clone(v))', () => {
    fc.assert(fc.property(arbLVal, (v) => {
        assert.ok(rtEqual(v, cloneLVal(v)), 'value not equal to its structural clone')
    }), { numRuns: PURE_RUNS })
})

test('4d: symmetric — rtEqual(a, b) === rtEqual(b, a)', () => {
    fc.assert(fc.property(arbLVal, arbLVal, (a, b) => {
        assert.strictEqual(rtEqual(a, b), rtEqual(b, a), 'equality is not symmetric')
    }), { numRuns: PURE_RUNS })
})

test('4d: transitive — rtEqual(a,b) && rtEqual(b,c) ⟹ rtEqual(a,c)', () => {
    fc.assert(fc.property(arbTriple, ([a, b, c]) => {
        if (rtEqual(a, b) && rtEqual(b, c)) {
            assert.ok(rtEqual(a, c), 'equality is not transitive')
        }
    }), { numRuns: PURE_RUNS })
})

// --- consistency with serialization (round-trip) ---------------------------

test('4d: round-trip preserves self-equality — rtEqual(v, roundTrip(v))', async () => {
    await fc.assert(fc.asyncProperty(arbLVal, async (v) => {
        const w = await roundTrip(v)
        assert.ok(rtEqual(v, w), 'value not equal to its round-tripped copy')
    }), { numRuns: RT_RUNS })
})

test('4d: round-trip preserves equality AND inequality — rtEqual(rt(a),rt(b)) === rtEqual(a,b)', async () => {
    await fc.assert(fc.asyncProperty(arbMaybeEqualPair, async ([a, b]) => {
        const before = rtEqual(a, b)
        const ra = await roundTrip(a)
        const rb = await roundTrip(b)
        assert.strictEqual(rtEqual(ra, rb), before, 'round-trip changed the equality verdict')
    }), { numRuns: RT_RUNS })
})
