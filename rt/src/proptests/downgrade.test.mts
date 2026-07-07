/**
 * Downgrade decision-function and result-label property tests (step 4a).
 *
 * Two units under test, both pure:
 *   - `levels.okToDowngradeGeneric(VALUE, dim)` — the policy decision function
 *     `(l_from, l_to, l_auth, bl, isNMIFC, pc) => DowngradeResult`
 *     (rt/src/levels/DCLabels/dclabel.mts).
 *   - `downgradeResultLabels(...)` — the occurrence-floor result-label
 *     computation factored out of `downgrader` (rt/src/downgrading.mts).
 *
 * Properties are stated against the ACTUAL implementation, not the model:
 *   - the `bl` (blocking-level) check is commented out, so the decision is
 *     independent of `bl` — no bl condition is asserted;
 *   - "authority monotonicity" is stated against `actsFor` (a' actsFor a means
 *     a' is at least as authoritative), which is the direction that makes the
 *     `implies(conjunction(auth, X), Y)` and robustness checks monotone.
 */
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fc from 'fast-check'
import { levels } from '../levels/DCLabels/dclabel.mjs'
import { DowngradeKind, DowngradeDimension, DowngradeResult } from '../DowngradeEnums.mjs'
import { downgradeResultLabels } from '../downgrading.mjs'
import { arbDCLabel } from './arbitraries.mjs'

const RUNS = { numRuns: 1000 }

// Decision function per dimension (VALUE kind throughout — the runtime's only
// downgrade kind exercised here).
const DIMENSIONS: ReadonlyArray<[string, DowngradeDimension]> = [
    ['CONFIDENTIALITY', DowngradeDimension.CONFIDENTIALITY],
    ['INTEGRITY', DowngradeDimension.INTEGRITY],
    ['BOTH', DowngradeDimension.BOTH],
]

const okTo = (dim: DowngradeDimension) =>
    levels.okToDowngradeGeneric(DowngradeKind.VALUE, dim)

const succeeds = (r: DowngradeResult) => r.kind === 'SUCCESS'

// ---------------------------------------------------------------------------
// Reflexive downgrade: l_to == l_from == l always SUCCEEDS, for any auth, any
// bl, any pc, both NMIFC modes.  Reasoned in the task spec: dimension-equality
// checks hold reflexively; enough_confidentiality/enough_integrity reduce to
// implies(conjunction(auth, l.X), l.X) = true; NMIFC robustness reduces to
// implies(conjunction(_, l.conf), l.conf) = true; transparency reduces to
// implies(l.int, l.int ∨ _) = true.
// ---------------------------------------------------------------------------
for (const [name, dim] of DIMENSIONS) {
    test(`reflexive downgrade always SUCCEEDS [${name}]`, () => {
        fc.assert(fc.property(
            arbDCLabel, arbDCLabel, arbDCLabel, arbDCLabel, fc.boolean(),
            (l, auth, bl, pc, isNMIFC) => {
                const r = okTo(dim)(l, l, auth, bl, isNMIFC, pc)
                assert.equal(r.kind, 'SUCCESS',
                    `reflexive downgrade failed with reason ${(r as any).reason}`)
                return true
            }), RUNS)
    })
}

// ---------------------------------------------------------------------------
// Authority monotonicity (against actsFor).  If a downgrade succeeds with
// authority `a`, then it also succeeds with any `a'` that actsFor `a`.
//   - constructive form: a' = a.coalesce(x) = <a.conf ∧ x.conf, a.int ∧ x.int>,
//     which actsFor a by construction;
//   - conditioned form: independent a', precondition on levels.actsFor(a', a).
// The decision's auth dependence: enough_confidentiality/integrity via
// implies(conjunction(auth, X), Y) and NMIFC robustness via
// disjunction(auth.conf, ...) — both monotone in strengthening auth.
// ---------------------------------------------------------------------------
for (const [name, dim] of DIMENSIONS) {
    test(`authority monotonicity — coalesce-strengthened a' [${name}]`, () => {
        fc.assert(fc.property(
            arbDCLabel, arbDCLabel, arbDCLabel, arbDCLabel, arbDCLabel, fc.boolean(),
            (from, to, a, x, pc, isNMIFC) => {
                fc.pre(succeeds(okTo(dim)(from, to, a, pc, isNMIFC, pc)))
                const aStrong = a.coalesce(x)
                assert.ok(levels.actsFor(aStrong, a), 'coalesce actsFor sanity')
                return succeeds(okTo(dim)(from, to, aStrong, pc, isNMIFC, pc))
            }), RUNS)
    })

    test(`authority monotonicity — conditioned actsFor(a',a) [${name}]`, () => {
        fc.assert(fc.property(
            arbDCLabel, arbDCLabel, arbDCLabel, arbDCLabel, arbDCLabel, fc.boolean(),
            (from, to, a, aPrime, pc, isNMIFC) => {
                fc.pre(levels.actsFor(aPrime, a))
                fc.pre(succeeds(okTo(dim)(from, to, a, pc, isNMIFC, pc)))
                return succeeds(okTo(dim)(from, to, aPrime, pc, isNMIFC, pc))
            }), RUNS)
    })
}

// ---------------------------------------------------------------------------
// Result-label floor.  On the success path, both result labels sit above the
// occurrence floor lub(pc, authLev, toLev).  Tested for both granularities.
// (The extracted floor is actually lub(pc, argLev, authLev, toLev) ⊒ this, so
// the property is a sound lower bound on that floor.)
// ---------------------------------------------------------------------------
for (const typeOnly of [false, true]) {
    const gran = typeOnly ? 'TYPE_ONLY' : 'BOTH_VALUE_AND_TYPE'
    test(`result-label floor: lub(pc,auth,toLev) flowsTo both labels [${gran}]`, () => {
        fc.assert(fc.property(
            arbDCLabel, arbDCLabel, arbDCLabel, arbDCLabel, arbDCLabel, arbDCLabel,
            (dataLev, dataTlev, levTo, pc, argLev, toLev) => {
                // authLev is independent of toLev in the runtime; use argLev as a
                // second free label for authLev to exercise both operands.
                const authLev = argLev
                const { lev, tlev } = downgradeResultLabels(
                    typeOnly, dataLev, dataTlev, levTo, pc, argLev, authLev, toLev)
                const occ = levels.lub(pc, authLev, toLev)
                assert.ok(occ.flowsTo(lev), 'occ flowsTo result.lev')
                assert.ok(occ.flowsTo(tlev), 'occ flowsTo result.tlev')
                return true
            }), RUNS)
    })
}

// Independent auth/arg operands (the runtime passes distinct labels for
// arg.lev and auth.lev); verify the floor still holds with all six free.
for (const typeOnly of [false, true]) {
    const gran = typeOnly ? 'TYPE_ONLY' : 'BOTH_VALUE_AND_TYPE'
    test(`result-label floor: all operands free [${gran}]`, () => {
        fc.assert(fc.property(
            fc.record({
                dataLev: arbDCLabel, dataTlev: arbDCLabel, levTo: arbDCLabel,
                pc: arbDCLabel, argLev: arbDCLabel, authLev: arbDCLabel, toLev: arbDCLabel,
            }),
            ({ dataLev, dataTlev, levTo, pc, argLev, authLev, toLev }) => {
                const { lev, tlev } = downgradeResultLabels(
                    typeOnly, dataLev, dataTlev, levTo, pc, argLev, authLev, toLev)
                const occ = levels.lub(pc, authLev, toLev)
                return occ.flowsTo(lev) && occ.flowsTo(tlev)
            }), RUNS)
    })
}

// ---------------------------------------------------------------------------
// Decision stability (referential transparency): the same six inputs yield the
// same kind and reason on two calls — guards against hidden global state.
// ---------------------------------------------------------------------------
for (const [name, dim] of DIMENSIONS) {
    test(`decision stability — equal inputs give equal result [${name}]`, () => {
        fc.assert(fc.property(
            arbDCLabel, arbDCLabel, arbDCLabel, arbDCLabel, arbDCLabel, fc.boolean(),
            (from, to, auth, bl, pc, isNMIFC) => {
                const r1 = okTo(dim)(from, to, auth, bl, isNMIFC, pc)
                const r2 = okTo(dim)(from, to, auth, bl, isNMIFC, pc)
                assert.deepEqual(r1, r2)
                return true
            }), RUNS)
    })
}

// ---------------------------------------------------------------------------
// bl-independence: with the blocking-level check commented out, the decision
// does not depend on `bl`.  Two calls differing only in bl agree.
// ---------------------------------------------------------------------------
for (const [name, dim] of DIMENSIONS) {
    test(`decision is independent of bl [${name}]`, () => {
        fc.assert(fc.property(
            arbDCLabel, arbDCLabel, arbDCLabel, arbDCLabel, arbDCLabel, arbDCLabel, fc.boolean(),
            (from, to, auth, bl1, bl2, pc, isNMIFC) => {
                const r1 = okTo(dim)(from, to, auth, bl1, isNMIFC, pc)
                const r2 = okTo(dim)(from, to, auth, bl2, isNMIFC, pc)
                assert.deepEqual(r1, r2)
                return true
            }), RUNS)
    })
}

// ---------------------------------------------------------------------------
// NMIFC is a refinement of non-NMIFC.  The isNMIFC block runs strictly after
// (and in addition to) the base dimension/authority checks, so if a downgrade
// succeeds with isNMIFC=true it also succeeds with isNMIFC=false.
// ---------------------------------------------------------------------------
for (const [name, dim] of DIMENSIONS) {
    test(`NMIFC refines non-NMIFC (success@true ⟹ success@false) [${name}]`, () => {
        fc.assert(fc.property(
            arbDCLabel, arbDCLabel, arbDCLabel, arbDCLabel, arbDCLabel,
            (from, to, auth, bl, pc) => {
                fc.pre(succeeds(okTo(dim)(from, to, auth, bl, true, pc)))
                return succeeds(okTo(dim)(from, to, auth, bl, false, pc))
            }), RUNS)
    })
}
