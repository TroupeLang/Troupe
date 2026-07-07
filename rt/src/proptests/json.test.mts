/**
 * Wire-format (JSON) round-trip property tests. This is the format exchanged
 * between nodes, so round-tripping must preserve label semantics.
 */
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fc from 'fast-check'
import { Category, CNF } from '../levels/DCLabels/cnf.mjs'
import { RegularLabel } from '../levels/DCLabels/label.mjs'
import { DCLabel } from '../levels/DCLabels/dclabel.mjs'
import { arbCNFAny, arbDCLabelQuar } from './arbitraries.mjs'

const RUNS = { numRuns: 1000 }

test('CNF JSON round-trip preserves semantics', () => {
    fc.assert(fc.property(arbCNFAny, x => {
        return CNF.fromJSON(x.toJSON()).equals(x)
    }), RUNS)
})

test('DCLabel JSON round-trip preserves semantics (incl. quarantined labels)', () => {
    fc.assert(fc.property(arbDCLabelQuar, l => {
        // fromJSON's declared param type is narrower than toJSON's output; the
        // runtime accepts the general shape.
        return DCLabel.fromJSON(l.toJSON() as any).equals(l)
    }), RUNS)
})

test('legacy string[][] JSON format equals the directly-built CNF', () => {
    const legacy = CNF.fromJSON([['alice', 'bob']])
    const direct = new CNF(new Set([
        new Category([new RegularLabel('alice'), new RegularLabel('bob')]),
    ]))
    assert.ok(legacy.equals(direct))
})
