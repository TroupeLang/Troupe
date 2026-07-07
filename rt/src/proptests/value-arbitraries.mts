/**
 * Value-tree generators and helpers shared by the serialization round-trip
 * property test (4b, serialize.test.mts) and the EqualityChecker-law property
 * test (4d, equality.test.mts).
 *
 * `arbLVal` produces random labelled-value (LVal) trees over the serializable,
 * non-closure Troupe value space: atoms, numbers, strings, booleans, unit, and
 * the aggregates list / tuple / record. Every node carries two random labels
 * (a value label `lev` and a type label `tlev`), each drawn from the step-2
 * `arbDCLabel` generator (reused from arbitraries.mts).
 *
 * Labels are restricted to *non-corrupt* ones (integrity implies confidentiality).
 * This is a deliberate scoping, not a weakening: ingress deserialization DROPs a
 * corrupt label by design (a corrupt label is rejected on receipt, never
 * round-tripped), so quantifying the round-trip over corrupt labels would test
 * the corruption policy rather than the wire format. Under ROOT trust every
 * non-corrupt label classifies as TRUSTED, so labels survive ingress unchanged
 * and the round-trip is expected to be the identity up to DCLabel.equals.
 *
 * Closures and process handles are excluded (per the 4b/4d spec).
 *
 * This module imports only the self-contained value/level machinery — no
 * scheduler, deserializer, or compiler subprocess — so it stays cheap to load.
 */
import fc from 'fast-check'
import { LVal } from '../Lval.mjs'
import { mkList, mkTuple } from '../ValuesUtil.mjs'
import { Record } from '../Record.mjs'
import { Atom } from '../Atom.mjs'
import { __unitbase } from '../UnitBase.mjs'
import { DCLabel } from '../levels/DCLabels/dclabel.mjs'
import { TroupeType } from '../TroupeTypes.mjs'
import { runtimeEquals } from '../EqualityChecker.mjs'
import { arbDCLabel } from './arbitraries.mjs'

// ---------------------------------------------------------------------------
// Label generator (reuses the step-2 arbDCLabel; keeps only non-corrupt labels)
// ---------------------------------------------------------------------------

/** Non-corrupt DC label. ~52% of arbDCLabel passes this filter. */
export const arbLabel: fc.Arbitrary<DCLabel> = arbDCLabel.filter(l => !l.isCorrupt())

// ---------------------------------------------------------------------------
// Leaf raw-value generators
// ---------------------------------------------------------------------------

const arbAtom: fc.Arbitrary<Atom> =
    fc.constantFrom('Foo', 'Bar', 'Baz', 'nil', 'cons', 'ok', 'error', 'none', 'some', '_a', 'x1')
        .map(n => new Atom(n))

/**
 * Numbers: small ints, large (near-2^53) ints, arbitrary finite doubles, and a
 * fixed set of edge values including -0. NaN and +/-Infinity are excluded: they
 * are not JSON-representable (JSON.stringify maps them to null) and NaN also
 * breaks the reference `==` equality — both are documented JS/JSON facts, not
 * wire-format defects. -0 is retained: JSON coerces it to 0, but the value stays
 * `==`-equal (and so EqualityChecker-equal) either way, which the test confirms.
 */
const arbNumber: fc.Arbitrary<number> = fc.oneof(
    fc.integer(),
    fc.maxSafeInteger(),
    fc.double({ noNaN: true, noDefaultInfinity: true }),
    fc.constantFrom(
        -0, 0, 1, -1,
        Number.MAX_SAFE_INTEGER, Number.MIN_SAFE_INTEGER,
        3.14159, -2.5e-10, 1e308, 42,
    ),
)

/** Strings: printable ASCII, full-unicode graphemes, and quote/backtick/control edges. */
const arbString: fc.Arbitrary<string> = fc.oneof(
    fc.string(),
    fc.string({ unit: 'grapheme' }),
    fc.constantFrom(
        '', '"', '`', 'a"b', 'x`y`z', 'quote:"inside"', 'back`tick`s',
        'newline\nhere', 'tab\there', 'emoji😀mix', 'ünïcödé', 'null\u0000char',
        '{}[]()', '\\backslash\\',
    ),
)

const arbBool: fc.Arbitrary<boolean> = fc.boolean()

const arbUnit: fc.Arbitrary<typeof __unitbase> = fc.constant(__unitbase)

const arbFieldName: fc.Arbitrary<string> =
    fc.constantFrom('a', 'b', 'c', 'foo', 'bar', 'x', 'y', 'field1', '_priv', 'name')

// ---------------------------------------------------------------------------
// Recursive labelled-value generator
// ---------------------------------------------------------------------------

const { lval } = fc.letrec<{ lval: LVal; raw: any }>(tie => ({
    // Each node draws an independent value label and type label.
    lval: fc.tuple(tie('raw'), arbLabel, arbLabel)
        .map(([raw, lev, tlev]) => new LVal(raw, lev, tlev)),

    raw: fc.oneof(
        { maxDepth: 4, depthSize: 'small', withCrossShrink: true },
        // leaves
        arbAtom, arbNumber, arbString, arbBool, arbUnit,
        // aggregates
        fc.array(tie('lval'), { maxLength: 4 }).map(a => mkList(a)),
        fc.array(tie('lval'), { minLength: 2, maxLength: 4 }).map(a => mkTuple(a)),
        fc.uniqueArray(fc.tuple(arbFieldName, tie('lval')), { selector: e => e[0], maxLength: 4 })
            .map(entries => Record.mkRecord(entries as [string, LVal][])),
    ),
}))

/** Random labelled-value tree over the serializable, non-closure value space. */
export const arbLVal: fc.Arbitrary<LVal> = lval

// ---------------------------------------------------------------------------
// Equality / label helpers
// ---------------------------------------------------------------------------

/** Structural equality via the runtime EqualityChecker, unwrapping the LVal-wrapped boolean. */
export function rtEqual(a: LVal, b: LVal): boolean {
    return runtimeEquals(a.val, b.val).val === true
}

/**
 * True iff `orig` and `copy` carry DCLabel-equal value and type labels at every
 * position of the tree (labels compared with DCLabel.equals, i.e. semantic
 * mutual-flowsTo equality, not structural identity).
 */
export function labelsEqualEverywhere(orig: LVal, copy: LVal): boolean {
    if (!(orig.lev as DCLabel).equals(copy.lev as DCLabel)) return false
    if (!(orig.tlev as DCLabel).equals(copy.tlev as DCLabel)) return false
    if (orig.troupeType !== copy.troupeType) return false

    switch (orig.troupeType) {
        case TroupeType.LIST: {
            const a: LVal[] = orig.val.toArray()
            const b: LVal[] = copy.val.toArray()
            if (a.length !== b.length) return false
            for (let i = 0; i < a.length; i++) {
                if (!labelsEqualEverywhere(a[i], b[i])) return false
            }
            return true
        }
        case TroupeType.TUPLE: {
            const a: LVal[] = orig.val
            const b: LVal[] = copy.val
            if (a.length !== b.length) return false
            for (let i = 0; i < a.length; i++) {
                if (!labelsEqualEverywhere(a[i], b[i])) return false
            }
            return true
        }
        case TroupeType.RECORD: {
            const a: Map<string, LVal> = orig.val.__obj
            const b: Map<string, LVal> = copy.val.__obj
            if (a.size !== b.size) return false
            for (const [k, v] of a.entries()) {
                if (!b.has(k)) return false
                if (!labelsEqualEverywhere(v, b.get(k)!)) return false
            }
            return true
        }
        default:
            return true
    }
}

/**
 * Deep structural clone producing a distinct object graph with identical content
 * and identical labels (same DCLabel instances). Used by 4d to construct
 * distinct-but-equal values that exercise the true-antecedent branch of the
 * equivalence-relation and serialization-consistency properties.
 */
export function cloneLVal(l: LVal): LVal {
    let raw: any
    switch (l.troupeType) {
        case TroupeType.LIST:
            raw = mkList((l.val.toArray() as LVal[]).map(cloneLVal))
            break
        case TroupeType.TUPLE:
            raw = mkTuple([...(l.val as LVal[])].map(cloneLVal))
            break
        case TroupeType.RECORD:
            raw = Record.mkRecord(
                [...(l.val.__obj as Map<string, LVal>).entries()]
                    .map(([k, v]) => [k, cloneLVal(v)] as [string, LVal]),
            )
            break
        case TroupeType.ATOM:
            raw = new Atom(l.val.atom, l.val.creation_uuid)
            break
        case TroupeType.UNIT:
            raw = __unitbase
            break
        default:
            raw = l.val
    }
    return new LVal(raw, l.lev, l.tlev)
}

/**
 * A pair that is sometimes two independent values (almost always unequal) and
 * sometimes a value paired with a distinct clone of itself (equal). Lets a single
 * property check equality preservation in both directions.
 */
export const arbMaybeEqualPair: fc.Arbitrary<[LVal, LVal]> = fc.oneof(
    fc.tuple(arbLVal, arbLVal),
    arbLVal.map(v => [v, cloneLVal(v)] as [LVal, LVal]),
)

/**
 * A triple in which each component is independently either a fresh random value
 * or a clone of a shared base value. Collisions on the shared base make the
 * transitivity antecedent (a=b and b=c) genuinely reachable rather than
 * vacuously false.
 */
export const arbTriple: fc.Arbitrary<[LVal, LVal, LVal]> = fc
    .tuple(
        arbLVal,
        fc.option(arbLVal, { nil: undefined }),
        fc.option(arbLVal, { nil: undefined }),
        fc.option(arbLVal, { nil: undefined }),
    )
    .map(([base, x, y, z]) => {
        const pick = (o?: LVal) => (o === undefined ? cloneLVal(base) : o)
        return [pick(x), pick(y), pick(z)] as [LVal, LVal, LVal]
    })
