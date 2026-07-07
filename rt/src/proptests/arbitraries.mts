/**
 * Shared fast-check generators and helpers for the runtime DC-label property tests.
 *
 * The DC-label lattice (rt/src/levels/DCLabels/) is self-contained, so these tests
 * import it directly with no scheduler/runtime state.
 */
import fc from 'fast-check'
import { Category, CNF } from '../levels/DCLabels/cnf.mjs'
import {
    Label,
    RegularLabel,
    QuarantinedLabel,
    QFalseLabel,
    WildcardQFalseLabel,
    QuarantineTag,
} from '../levels/DCLabels/label.mjs'
import { DCLabel } from '../levels/DCLabels/dclabel.mjs'

/** Fixed 5-principal universe used by the exhaustive semantic oracle. */
export const PRINCIPALS = ['alice', 'bob', 'charlie', 'dorothy', 'eve'] as const

/**
 * All 32 truth assignments over PRINCIPALS, each represented as the set of
 * principals assigned "true".
 */
export const ALL_ASSIGNMENTS: ReadonlyArray<Set<string>> = (() => {
    const out: Set<string>[] = []
    const n = PRINCIPALS.length
    for (let mask = 0; mask < (1 << n); mask++) {
        const s = new Set<string>()
        for (let i = 0; i < n; i++) {
            if (mask & (1 << i)) s.add(PRINCIPALS[i])
        }
        out.push(s)
    }
    return out
})()

/**
 * Semantic oracle: evaluate a regular-label CNF under a truth assignment.
 * A CNF (conjunction of clauses) is satisfied iff every clause (a disjunction
 * of principals) has at least one principal set true.
 */
export function evalCNF(trueTags: Set<string>, x: CNF): boolean {
    return [...x.categories].every(cat =>
        cat.getLabels().some(l => trueTags.has((l as RegularLabel).principal)))
}

// ---------------------------------------------------------------------------
// Regular-label generators
// ---------------------------------------------------------------------------

export const arbRegular: fc.Arbitrary<RegularLabel> =
    fc.constantFrom(...PRINCIPALS).map(p => new RegularLabel(p))

/** Non-empty clause (disjunction of 1-3 principals). */
export const arbCategory: fc.Arbitrary<Category> =
    fc.array(arbRegular, { minLength: 1, maxLength: 3 }).map(ls => new Category(ls))

/** Clause that may be empty (empty = FALSE clause). */
export const arbCategoryAny: fc.Arbitrary<Category> =
    fc.array(arbRegular, { minLength: 0, maxLength: 3 }).map(ls => new Category(ls))

/**
 * CNF with only non-empty clauses (the set of clauses may itself be empty,
 * i.e. CNF_TRUE). Used for the semantic-oracle tests.
 */
export const arbCNF: fc.Arbitrary<CNF> =
    fc.array(arbCategory, { minLength: 0, maxLength: 3 }).map(cs => new CNF(new Set(cs)))

/** CNF that may contain empty (FALSE) clauses. Used for the lattice-law tests. */
export const arbCNFAny: fc.Arbitrary<CNF> =
    fc.array(arbCategoryAny, { minLength: 0, maxLength: 3 }).map(cs => new CNF(new Set(cs)))

/** DCLabel over regular labels, possibly containing FALSE clauses. */
export const arbDCLabel: fc.Arbitrary<DCLabel> =
    fc.tuple(arbCNFAny, arbCNFAny).map(([c, i]) => new DCLabel(c, i))

// ---------------------------------------------------------------------------
// Quarantine generators
// ---------------------------------------------------------------------------

export const NODES = ['nodeA', 'nodeB'] as const
export const QIDS = ['q1', 'q2'] as const

export const arbTag: fc.Arbitrary<QuarantineTag> = fc.record({
    nodeId: fc.constantFrom(...NODES),
    quarantineId: fc.constantFrom(...QIDS),
})

export const arbQuarantinedLabel: fc.Arbitrary<QuarantinedLabel> =
    fc.tuple(arbRegular, arbTag).map(([r, t]) => QuarantinedLabel.fromRegular(r, t))

export const arbQFalseLabel: fc.Arbitrary<QFalseLabel> =
    arbTag.map(t => new QFalseLabel(t))

export const arbWildcardLabel: fc.Arbitrary<WildcardQFalseLabel> =
    fc.constantFrom(...NODES).map(n => new WildcardQFalseLabel(n))

/** Any label kind, including the non-serializable wildcard. */
export const arbAnyLabel: fc.Arbitrary<Label> = fc.oneof(
    arbRegular,
    arbQuarantinedLabel,
    arbQFalseLabel,
    arbWildcardLabel,
)

/** Labels that round-trip through JSON (excludes WildcardQFalseLabel). */
export const arbSerializableLabel: fc.Arbitrary<Label> = fc.oneof(
    arbRegular,
    arbQuarantinedLabel,
    arbQFalseLabel,
)

const arbSerializableCategory: fc.Arbitrary<Category> =
    fc.array(arbSerializableLabel, { minLength: 0, maxLength: 3 }).map(ls => new Category(ls))

const arbSerializableCNF: fc.Arbitrary<CNF> =
    fc.array(arbSerializableCategory, { minLength: 0, maxLength: 3 }).map(cs => new CNF(new Set(cs)))

/** DCLabel that may contain (serializable) quarantined labels. */
export const arbDCLabelQuar: fc.Arbitrary<DCLabel> =
    fc.tuple(arbSerializableCNF, arbSerializableCNF).map(([c, i]) => new DCLabel(c, i))
