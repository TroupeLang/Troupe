/**
 * Step 4b: value serialization round-trip.
 *
 * For random labelled-value trees (numbers, strings, booleans, unit,
 * lists, tuples, records; no closures or process handles), `deserialize` of the
 * JSON-encoded `serialize` output must reproduce the original value:
 *
 *   (i)  structurally, via the runtime EqualityChecker, and
 *   (ii) label-for-label, comparing every position's value and type labels with
 *        DCLabel.equals.
 *
 * A failure here is a wire-format defect: silent value corruption or — for the
 * label check — a label corruption in transit, which is an IFC hole.
 *
 * See value-arbitraries.mts for the generator and the (documented) restriction
 * to non-corrupt labels, and serde-harness.mts for the round-trip mechanics and
 * runtime teardown.
 */
import { test } from 'node:test'
import assert from 'node:assert'
import fc from 'fast-check'
import { roundTrip } from './serde-harness.mjs'
import { arbLVal, rtEqual, labelsEqualEverywhere } from './value-arbitraries.mjs'

const NUM_RUNS = 500

test('4b: deserialize(serialize(v)) is structurally equal to v (EqualityChecker)', async () => {
    await fc.assert(
        fc.asyncProperty(arbLVal, async (v) => {
            const w = await roundTrip(v)
            assert.ok(rtEqual(v, w), 'round-trip changed the value structurally')
        }),
        { numRuns: NUM_RUNS },
    )
})

test('4b: round-trip preserves every label (DCLabel.equals at each position)', async () => {
    await fc.assert(
        fc.asyncProperty(arbLVal, async (v) => {
            const w = await roundTrip(v)
            assert.ok(labelsEqualEverywhere(v, w), 'round-trip changed a label somewhere in the tree')
        }),
        { numRuns: NUM_RUNS },
    )
})
