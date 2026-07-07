/**
 * CNF-level property tests (regular labels only).
 *
 * The central check is that the syntactic `implies` matches the semantic oracle
 * (satisfaction under truth assignments) exhaustively over the 5-principal
 * universe (32 assignments).
 */
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fc from 'fast-check'
import {
    CNF,
    CNF_TRUE,
    CNF_FALSE,
    implies,
    conjunction,
    disjunction,
} from '../levels/DCLabels/cnf.mjs'
import { arbCNF, ALL_ASSIGNMENTS, evalCNF } from './arbitraries.mjs'

const RUNS = { numRuns: 1000 }

/** True iff every assignment satisfying x also satisfies y. */
function semanticImplies(x: CNF, y: CNF): boolean {
    return ALL_ASSIGNMENTS.every(ts => !evalCNF(ts, x) || evalCNF(ts, y))
}

test('implies matches the semantic oracle (exhaustive over 32 assignments)', () => {
    fc.assert(fc.property(arbCNF, arbCNF, (x, y) => {
        return implies(x, y) === semanticImplies(x, y)
    }), RUNS)
})

test('implies is reflexive', () => {
    fc.assert(fc.property(arbCNF, x => implies(x, x)), RUNS)
})

test('implies is transitive', () => {
    fc.assert(fc.property(arbCNF, arbCNF, arbCNF, (x, y, z) => {
        fc.pre(implies(x, y) && implies(y, z))
        return implies(x, z)
    }), RUNS)
})

test('equals is reflexive and symmetric, and matches two-sided implies', () => {
    fc.assert(fc.property(arbCNF, arbCNF, (x, y) => {
        assert.ok(x.equals(x))
        assert.equal(x.equals(y), y.equals(x))
        assert.equal(x.equals(y), implies(x, y) && implies(y, x))
        return true
    }), RUNS)
})

test('conjunction is semantic AND', () => {
    fc.assert(fc.property(arbCNF, arbCNF, (x, y) => {
        const c = conjunction(x, y)
        return ALL_ASSIGNMENTS.every(ts =>
            evalCNF(ts, c) === (evalCNF(ts, x) && evalCNF(ts, y)))
    }), RUNS)
})

test('disjunction is semantic OR', () => {
    fc.assert(fc.property(arbCNF, arbCNF, (x, y) => {
        const d = disjunction(x, y)
        return ALL_ASSIGNMENTS.every(ts =>
            evalCNF(ts, d) === (evalCNF(ts, x) || evalCNF(ts, y)))
    }), RUNS)
})

test('conjunction/disjunction identities', () => {
    fc.assert(fc.property(arbCNF, x => {
        assert.ok(conjunction(x, CNF_TRUE).equals(x), 'x & TRUE = x')
        assert.ok(disjunction(x, CNF_FALSE).equals(x), 'x | FALSE = x')
        return true
    }), RUNS)
})

test('CNF_FALSE implies everything; everything implies CNF_TRUE', () => {
    fc.assert(fc.property(arbCNF, x => {
        assert.ok(implies(CNF_FALSE, x), 'FALSE => x')
        assert.ok(implies(x, CNF_TRUE), 'x => TRUE')
        return true
    }), RUNS)
})
