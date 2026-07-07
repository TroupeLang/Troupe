/**
 * Model-based property tests for RawTuple (rt/src/RawTuple.mts).
 *
 * RawTuple extends Array<LVal> and is immutable: its entire contract is
 * "construct from an array of LVals, then read". It has no domain mutators, so
 * every observable is a pure function of the construction array — a stateful
 * `fc.commands` sequence would add no state transitions over simply quantifying
 * over random construction arrays and checking each reader against that array
 * (the reference model). Hence the direct property style here rather than the
 * `fc.commands` model used for RawList.
 *
 * Readers checked against the JS-array model (by reference identity, since
 * RawTuple stores the LVal references it is given): indexed access, length,
 * iteration (spread / Array.from / for-of), the `dataLevel` lub invariant, and
 * `stringRep`.
 *
 * NOTE (documented, not asserted here): inherited Array transformers that go
 * through the species constructor — `map`, `filter`, `slice`, etc. — throw,
 * because `Symbol.species` builds `new RawTuple(len)` and the constructor does
 * `super(...len)`, spreading a number. Callers work around this with
 * `Array.from(tuple)` (see rt/src/builtins/debugValue.mts). These are not part
 * of RawTuple's intended contract and are excluded from the model.
 */
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fc from 'fast-check'
import { RawTuple } from '../RawTuple.mjs'
import { RawList } from '../RawList.mjs'
import { LVal, listStringRep } from '../Lval.mjs'
import { levels, IFC_BOT } from '../levels/DCLabels/dclabel.mjs'
import { arbDCLabel } from './arbitraries.mjs'

const RUNS = { numRuns: 1000 }

// ---------------------------------------------------------------------------
// Element generators (mirrors rawlist.test.mts: nesting exercises the lub)
// ---------------------------------------------------------------------------

const { arbElem } = fc.letrec(tie => ({
    arbElem: fc.oneof(
        { maxDepth: 2 },
        fc.tuple(fc.integer(), arbDCLabel).map(([n, l]) => new LVal(n, l)),
        fc.tuple(fc.array(tie('arbElem') as fc.Arbitrary<LVal>, { maxLength: 3 }), arbDCLabel)
            .map(([xs, l]) => new LVal(new RawTuple(xs), l)),
        fc.tuple(fc.array(tie('arbElem') as fc.Arbitrary<LVal>, { maxLength: 3 }), arbDCLabel)
            .map(([xs, l]) => new LVal(RawList.fromArray(xs), l)),
    ) as fc.Arbitrary<LVal>,
}))

const modelDataLevel = (arr: LVal[]) =>
    arr.reduce((acc, e) => levels.lub(acc, e.dataLevel), IFC_BOT)

// ---------------------------------------------------------------------------
// Properties
// ---------------------------------------------------------------------------

test('length matches the construction array', () => {
    fc.assert(fc.property(fc.array(arbElem, { maxLength: 12 }), arr => {
        return new RawTuple(arr).length === arr.length
    }), RUNS)
})

test('indexed access returns the model element (by identity) at every position', () => {
    fc.assert(fc.property(fc.array(arbElem, { maxLength: 12 }), arr => {
        const t = new RawTuple(arr)
        for (let i = 0; i < arr.length; i++) assert.strictEqual(t[i], arr[i])
        return true
    }), RUNS)
})

test('iteration (spread / Array.from / for-of) yields the model elements in order', () => {
    fc.assert(fc.property(fc.array(arbElem, { maxLength: 12 }), arr => {
        const t = new RawTuple(arr)

        const spread = [...t]
        const from = Array.from(t)
        const loop: LVal[] = []
        for (const e of t) loop.push(e)

        assert.strictEqual(spread.length, arr.length)
        assert.strictEqual(from.length, arr.length)
        assert.strictEqual(loop.length, arr.length)
        for (let i = 0; i < arr.length; i++) {
            assert.strictEqual(spread[i], arr[i], `spread ${i}`)
            assert.strictEqual(from[i], arr[i], `Array.from ${i}`)
            assert.strictEqual(loop[i], arr[i], `for-of ${i}`)
        }
        return true
    }), RUNS)
})

test('dataLevel equals the lub of element dataLevels', () => {
    fc.assert(fc.property(fc.array(arbElem, { maxLength: 12 }), arr => {
        return new RawTuple(arr).dataLevel.equals(modelDataLevel(arr))
    }), RUNS)
})

test('stringRep(omitLevels) matches the parenthesized join of element stringReps', () => {
    fc.assert(fc.property(fc.array(arbElem, { maxLength: 8 }), arr => {
        const expected = '(' + listStringRep(arr, true) + ')'
        return new RawTuple(arr).stringRep(true) === expected
    }), RUNS)
})

test('empty tuple: length 0, no elements, dataLevel BOT', () => {
    const t = new RawTuple([])
    assert.strictEqual(t.length, 0)
    assert.deepStrictEqual([...t], [])
    assert.deepStrictEqual(Array.from(t), [])
    assert.ok(t.dataLevel.equals(IFC_BOT))
    assert.strictEqual(t.stringRep(true), '()')
})

test('single-element tuple: length 1, dataLevel equals the element dataLevel', () => {
    fc.assert(fc.property(arbElem, e => {
        const t = new RawTuple([e])
        assert.strictEqual(t.length, 1)
        assert.strictEqual(t[0], e)
        assert.ok(t.dataLevel.equals(e.dataLevel))
        return true
    }), RUNS)
})
