/**
 * DCLabel lattice-law property tests.
 *
 * All equalities are checked via DCLabel.equals (mutual flowsTo, i.e. semantic
 * equality), never structural equality.
 */
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fc from 'fast-check'
import { levels, IFC_BOT, IFC_TOP } from '../levels/DCLabels/dclabel.mjs'
import { arbDCLabel, PRINCIPALS } from './arbitraries.mjs'

const RUNS = { numRuns: 1000 }

const lub = (a: any, b: any) => levels.lub(a, b)
const glb = (a: any, b: any) => levels.glb(a, b)

test('flowsTo is reflexive', () => {
    fc.assert(fc.property(arbDCLabel, a => a.flowsTo(a)), RUNS)
})

test('flowsTo is transitive', () => {
    fc.assert(fc.property(arbDCLabel, arbDCLabel, arbDCLabel, (a, b, c) => {
        fc.pre(a.flowsTo(b) && b.flowsTo(c))
        return a.flowsTo(c)
    }), RUNS)
})

test('IFC_BOT is bottom and IFC_TOP is top', () => {
    fc.assert(fc.property(arbDCLabel, a => {
        assert.ok(IFC_BOT.flowsTo(a), 'BOT flowsTo a')
        assert.ok(a.flowsTo(IFC_TOP), 'a flowsTo TOP')
        return true
    }), RUNS)
})

test('lub: idempotent, commutative, associative', () => {
    fc.assert(fc.property(arbDCLabel, arbDCLabel, arbDCLabel, (a, b, c) => {
        assert.ok(lub(a, a).equals(a), 'idempotent')
        assert.ok(lub(a, b).equals(lub(b, a)), 'commutative')
        assert.ok(lub(lub(a, b), c).equals(lub(a, lub(b, c))), 'associative')
        return true
    }), RUNS)
})

test('lub: identity IFC_BOT, absorbing IFC_TOP', () => {
    fc.assert(fc.property(arbDCLabel, a => {
        assert.ok(lub(a, IFC_BOT).equals(a), 'identity BOT')
        assert.ok(lub(a, IFC_TOP).equals(IFC_TOP), 'absorbing TOP')
        return true
    }), RUNS)
})

test('lub is an upper bound', () => {
    fc.assert(fc.property(arbDCLabel, arbDCLabel, (a, b) => {
        const j = lub(a, b)
        return a.flowsTo(j) && b.flowsTo(j)
    }), RUNS)
})

test('lub is the least upper bound (non-vacuous, c = lub(a,b,extra))', () => {
    fc.assert(fc.property(arbDCLabel, arbDCLabel, arbDCLabel, (a, b, extra) => {
        const c = levels.lub(a, b, extra)
        // a and b both flow to c by construction; the LUB must too.
        return lub(a, b).flowsTo(c)
    }), RUNS)
})

test('lub is the least upper bound (conditioned form)', () => {
    fc.assert(fc.property(arbDCLabel, arbDCLabel, arbDCLabel, (a, b, c) => {
        fc.pre(a.flowsTo(c) && b.flowsTo(c))
        return lub(a, b).flowsTo(c)
    }), RUNS)
})

test('glb: idempotent, commutative, associative', () => {
    fc.assert(fc.property(arbDCLabel, arbDCLabel, arbDCLabel, (a, b, c) => {
        assert.ok(glb(a, a).equals(a), 'idempotent')
        assert.ok(glb(a, b).equals(glb(b, a)), 'commutative')
        assert.ok(glb(glb(a, b), c).equals(glb(a, glb(b, c))), 'associative')
        return true
    }), RUNS)
})

test('glb: identity IFC_TOP, absorbing IFC_BOT', () => {
    fc.assert(fc.property(arbDCLabel, a => {
        assert.ok(glb(a, IFC_TOP).equals(a), 'identity TOP')
        assert.ok(glb(a, IFC_BOT).equals(IFC_BOT), 'absorbing BOT')
        return true
    }), RUNS)
})

test('glb is a lower bound', () => {
    fc.assert(fc.property(arbDCLabel, arbDCLabel, (a, b) => {
        const m = glb(a, b)
        return m.flowsTo(a) && m.flowsTo(b)
    }), RUNS)
})

test('glb is the greatest lower bound (conditioned form)', () => {
    fc.assert(fc.property(arbDCLabel, arbDCLabel, arbDCLabel, (a, b, c) => {
        fc.pre(c.flowsTo(a) && c.flowsTo(b))
        return c.flowsTo(glb(a, b))
    }), RUNS)
})

test('absorption laws', () => {
    fc.assert(fc.property(arbDCLabel, arbDCLabel, (a, b) => {
        assert.ok(lub(a, glb(a, b)).equals(a), 'a lub (a glb b) = a')
        assert.ok(glb(a, lub(a, b)).equals(a), 'a glb (a lub b) = a')
        return true
    }), RUNS)
})

test('variadic lub folds as left-associated binary lub', () => {
    fc.assert(fc.property(arbDCLabel, arbDCLabel, arbDCLabel, (a, b, c) => {
        return levels.lub(a, b, c).equals(lub(lub(a, b), c))
    }), RUNS)
})

// ---------------------------------------------------------------------------
// fromV1String
// ---------------------------------------------------------------------------

const arbTagSet = fc.uniqueArray(fc.constantFrom(...PRINCIPALS), { minLength: 1, maxLength: 5 })

test('fromV1String is insensitive to order, case, duplication, and whitespace', () => {
    fc.assert(fc.property(arbTagSet, fc.array(fc.boolean(), { minLength: 1 }), (tags, flags) => {
        const canonical = tags.join(',')
        const decorated = [...tags, ...tags]
            .map((t, i) => flags[i % flags.length] ? t.toUpperCase() : t)
            .map(t => `  ${t} `)
        decorated.reverse()
        const variant = decorated.join(',')
        return levels.fromV1String(canonical).equals(levels.fromV1String(variant))
    }), RUNS)
})

test('fromV1String empty forms are IFC_BOT (V1 {} = <True; False>)', () => {
    assert.ok(levels.fromV1String('').equals(IFC_BOT))
    assert.ok(levels.fromV1String('{}').equals(IFC_BOT))
    assert.ok(levels.fromV1String('  {  }  ').equals(IFC_BOT))
})
