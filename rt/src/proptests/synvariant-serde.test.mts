/**
 * Shape validation of flagged (syntactic-variant) tuples at the deserialization
 * boundary (deserialize.mts).
 *
 * A flagged tuple is well-formed iff it has arity 1 (tag only) or 2 (tag,
 * payload) with slot 0 a string (isWellFormedSynVariant in RawTuple.mts). The
 * deserializer reconstructs a flagged tuple of any arity from the wire, so a
 * malformed inbound shape must be rejected there — as corrupt data (DROP) —
 * rather than crashing the printer later on a missing slot 0 or silently
 * dropping payload slots.
 *
 * Well-formed flagged tuples must still round-trip unchanged; malformed ones
 * (empty, over-long, non-string tag slot) must be dropped.
 */
import { test } from 'node:test'
import assert from 'node:assert'
// serde-harness first: it evaluates runtimeMonitored before deserialize (see
// its header) and registers the runtime teardown hook for this test file.
import { roundTrip } from './serde-harness.mjs'
import * as DS from '../deserialize.mjs'
import { IngressResult } from '../deserialize.mjs'
import { serialize } from '../serialize.mjs'
import * as levels from '../Level.mjs'
import { LVal } from '../Lval.mjs'
import { RawTuple } from '../RawTuple.mjs'

const tag = () => new LVal('hhu9#color#RED', levels.BOT)
const payload = () => new LVal(42, levels.BOT)

// Push a flagged tuple of the given slots through the JSON wire encoding and
// deserialize it, returning the ingress result.
async function deserializeFlagged(slots: LVal[]) {
    const v = new LVal(new RawTuple(slots, true), levels.BOT)
    const { data } = serialize(v, levels.BOT)
    const wire = JSON.parse(JSON.stringify(data))
    return DS.deserialize(levels.ROOT, wire)
}

test('well-formed nullary flagged tuple round-trips and renders as the bare name', async () => {
    const v = new LVal(new RawTuple([tag()], true), levels.BOT)
    const w = await roundTrip(v)
    assert.strictEqual((w.val as any)._isSynVariant, true)
    assert.strictEqual(w.val.stringRep(true), 'RED')
})

test('well-formed applied flagged tuple round-trips and renders as (name payload)', async () => {
    const v = new LVal(new RawTuple([tag(), payload()], true), levels.BOT)
    const w = await roundTrip(v)
    assert.strictEqual((w.val as any)._isSynVariant, true)
    assert.strictEqual(w.val.stringRep(true), '(RED 42)')
})

test('malformed flagged tuples are dropped at the deserialization boundary', async () => {
    const shapes: { name: string; slots: LVal[] }[] = [
        { name: 'empty', slots: [] },
        { name: 'over-long (3 slots)', slots: [tag(), payload(), payload()] },
        { name: 'slot 0 not a string', slots: [payload()] },
    ]
    for (const { name, slots } of shapes) {
        const result = await deserializeFlagged(slots)
        assert.strictEqual(result.result, IngressResult.DROP, `expected DROP for ${name}`)
    }
})
