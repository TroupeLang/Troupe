/**
 * Model-based property tests for RawList (rt/src/RawList.mts).
 *
 * RawList is an immutable, persistent singly-linked list of LVals (Nil / Cons).
 * Its structural operations — `new Cons` (prepend) and `.tail` (behead) — compose
 * into sequences, so a stateful fast-check `fc.commands` model is the natural fit:
 * the reference model is a plain JS array of the same LVal references, and every
 * generated command applies the same transition to both, then the observer
 * commands assert the RawList agrees with the array element-wise (by reference
 * identity, since RawList only ever rearranges the element references it is given),
 * on length, on indexing, and on the `dataLevel` lub invariant.
 *
 * A handful of one-shot properties (fromArray round-trip, out-of-range / empty
 * errors) cover the parts that are not naturally a state transition.
 */
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fc from 'fast-check'
import { RawList, Nil, Cons } from '../RawList.mjs'
import { LVal } from '../Lval.mjs'
import { RawTuple } from '../RawTuple.mjs'
import { levels, IFC_BOT } from '../levels/DCLabels/dclabel.mjs'
import { arbDCLabel } from './arbitraries.mjs'

const RUNS = { numRuns: 1000 }

// ---------------------------------------------------------------------------
// Element generators
// ---------------------------------------------------------------------------

/**
 * An LVal element, possibly wrapping a nested RawTuple/RawList so the
 * `dataLevel` lub is exercised over nested aggregate labels. Elements are
 * compared by reference identity, so their payloads need not be distinct.
 */
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

/** Expected dataLevel of a list of elements: the lub of the element dataLevels. */
const modelDataLevel = (arr: LVal[]) =>
    arr.reduce((acc, e) => levels.lub(acc, e.dataLevel), IFC_BOT)

// ---------------------------------------------------------------------------
// Stateful model (fc.commands)
// ---------------------------------------------------------------------------

type Model = { arr: LVal[] }
type Real = { list: RawList }

class ConsCommand implements fc.Command<Model, Real> {
    constructor(readonly v: LVal) {}
    check(_m: Readonly<Model>) { return true }
    run(m: Model, r: Real) {
        r.list = new Cons(this.v, r.list)
        m.arr.unshift(this.v)
    }
    toString() { return `cons(${this.v.val})` }
}

class TailCommand implements fc.Command<Model, Real> {
    check(m: Readonly<Model>) { return m.arr.length > 0 }
    run(m: Model, r: Real) {
        r.list = r.list.tail
        m.arr.shift()
    }
    toString() { return 'tail' }
}

class HeadCheck implements fc.Command<Model, Real> {
    check(m: Readonly<Model>) { return m.arr.length > 0 }
    run(m: Model, r: Real) {
        assert.strictEqual(r.list.head, m.arr[0])
    }
    toString() { return 'head?' }
}

class LengthCheck implements fc.Command<Model, Real> {
    check(_m: Readonly<Model>) { return true }
    run(m: Model, r: Real) {
        assert.strictEqual(r.list.length, m.arr.length)
    }
    toString() { return 'length?' }
}

class IsNilCheck implements fc.Command<Model, Real> {
    check(_m: Readonly<Model>) { return true }
    run(m: Model, r: Real) {
        assert.strictEqual(r.list.isNil, m.arr.length === 0)
    }
    toString() { return 'isNil?' }
}

class IndexCheck implements fc.Command<Model, Real> {
    constructor(readonly frac: number) {}
    check(m: Readonly<Model>) { return m.arr.length > 0 }
    run(m: Model, r: Real) {
        const j = Math.min(m.arr.length - 1, Math.floor(this.frac * m.arr.length))
        assert.strictEqual(r.list.index(j), m.arr[j])
    }
    toString() { return `index(${this.frac.toFixed(3)})` }
}

class ToArrayCheck implements fc.Command<Model, Real> {
    check(_m: Readonly<Model>) { return true }
    run(m: Model, r: Real) {
        const got = r.list.toArray()
        assert.strictEqual(got.length, m.arr.length)
        for (let i = 0; i < m.arr.length; i++) {
            assert.strictEqual(got[i], m.arr[i], `element ${i}`)
        }
    }
    toString() { return 'toArray?' }
}

class DataLevelCheck implements fc.Command<Model, Real> {
    check(_m: Readonly<Model>) { return true }
    run(m: Model, r: Real) {
        assert.ok(r.list.dataLevel.equals(modelDataLevel(m.arr)), 'dataLevel = lub of elements')
    }
    toString() { return 'dataLevel?' }
}

/** Rebuilding the current model array via fromArray must reproduce it. */
class FromArrayCheck implements fc.Command<Model, Real> {
    check(_m: Readonly<Model>) { return true }
    run(m: Model, _r: Real) {
        const rebuilt = RawList.fromArray(m.arr).toArray()
        assert.strictEqual(rebuilt.length, m.arr.length)
        for (let i = 0; i < m.arr.length; i++) {
            assert.strictEqual(rebuilt[i], m.arr[i], `element ${i}`)
        }
    }
    toString() { return 'fromArray?' }
}

const allCommands = [
    arbElem.map(v => new ConsCommand(v)),
    fc.constant(new TailCommand()),
    fc.constant(new HeadCheck()),
    fc.constant(new LengthCheck()),
    fc.constant(new IsNilCheck()),
    fc.double({ min: 0, max: 1, noNaN: true, noDefaultInfinity: true }).map(f => new IndexCheck(f)),
    fc.constant(new ToArrayCheck()),
    fc.constant(new DataLevelCheck()),
    fc.constant(new FromArrayCheck()),
]

test('RawList agrees with a JS-array model over random cons/tail sequences', () => {
    fc.assert(
        fc.property(fc.commands(allCommands, { maxCommands: 50 }), cmds => {
            const setup = () => ({ model: { arr: [] as LVal[] }, real: { list: new Nil() as RawList } })
            fc.modelRun(setup, cmds)
        }),
        RUNS,
    )
})

// ---------------------------------------------------------------------------
// One-shot properties
// ---------------------------------------------------------------------------

test('fromArray preserves order, length, and identity of every element', () => {
    fc.assert(fc.property(fc.array(arbElem, { maxLength: 12 }), arr => {
        const got = RawList.fromArray(arr).toArray()
        assert.strictEqual(got.length, arr.length)
        for (let i = 0; i < arr.length; i++) assert.strictEqual(got[i], arr[i])
        return true
    }), RUNS)
})

test('index(j) equals the model element for every in-range j', () => {
    fc.assert(fc.property(fc.array(arbElem, { minLength: 1, maxLength: 12 }), arr => {
        const l = RawList.fromArray(arr)
        for (let j = 0; j < arr.length; j++) assert.strictEqual(l.index(j), arr[j])
        return true
    }), RUNS)
})

test('dataLevel equals the lub of element dataLevels', () => {
    fc.assert(fc.property(fc.array(arbElem, { maxLength: 12 }), arr => {
        return RawList.fromArray(arr).dataLevel.equals(modelDataLevel(arr))
    }), RUNS)
})

test('empty list: Nil is nil, length 0, dataLevel BOT, and head/tail/index throw', () => {
    const nil = new Nil()
    assert.strictEqual(nil.isNil, true)
    assert.strictEqual(nil.length, 0)
    assert.deepStrictEqual(nil.toArray(), [])
    assert.ok(nil.dataLevel.equals(IFC_BOT))
    assert.throws(() => nil.head, /head: empty list/)
    assert.throws(() => nil.tail, /tail: empty list/)
    assert.throws(() => nil.index(0), /index: empty list/)
})

test('index out of range (>= length and negative) throws', () => {
    fc.assert(fc.property(
        fc.array(arbElem, { maxLength: 8 }),
        fc.nat({ max: 5 }),
        (arr, over) => {
            const l = RawList.fromArray(arr)
            assert.throws(() => l.index(arr.length + over), /index: empty list/)
            assert.throws(() => l.index(-1 - over), /index: empty list/)
            return true
        }), RUNS)
})

test('cons prepends: head is the new element and tail is the old list', () => {
    fc.assert(fc.property(arbElem, fc.array(arbElem, { maxLength: 8 }), (v, arr) => {
        const base = RawList.fromArray(arr)
        const consed = new Cons(v, base)
        assert.strictEqual(consed.head, v)
        assert.strictEqual(consed.tail, base)
        assert.strictEqual(consed.length, arr.length + 1)
        assert.strictEqual(consed.isNil, false)
        return true
    }), RUNS)
})
