/**
 * Quarantine-label property tests.
 *
 * The `labelImplies` rules under test are taken verbatim from the documented
 * behaviour in rt/src/levels/DCLabels/label.mts:
 *
 *   - Every label implies itself.
 *   - QFalse implies any QuarantinedLabel with the same quarantine tag.
 *   - WildcardQFalse implies any QuarantinedLabel with the same nodeId (any quarantineId).
 *   - WildcardQFalse implies any QFalseLabel with the same nodeId (any quarantineId).
 *   - No other cross-type implications hold.
 *
 * Also checks that flowsTo/equals over DCLabels containing quarantined labels
 * remain a preorder / equivalence.
 */
import { test } from 'node:test'
import assert from 'node:assert/strict'
import fc from 'fast-check'
import {
    Label,
    LabelKind,
    RegularLabel,
    QuarantinedLabel,
    QFalseLabel,
    WildcardQFalseLabel,
    labelImplies,
    QuarantineTag,
} from '../levels/DCLabels/label.mjs'
import {
    arbAnyLabel,
    arbDCLabelQuar,
    arbTag,
    arbRegular,
    NODES,
} from './arbitraries.mjs'

const RUNS = { numRuns: 1000 }

function sameTag(a: QuarantineTag, b: QuarantineTag): boolean {
    return a.nodeId === b.nodeId && a.quarantineId === b.quarantineId
}

/**
 * Independent reference for the documented labelImplies rules, written from the
 * doc comment using raw field comparisons (not the implementation's helpers).
 */
function specImplies(x: Label, y: Label): boolean {
    // Reflexivity: same kind and same identifying fields.
    if (x.kind === y.kind) {
        switch (x.kind) {
            case LabelKind.REGULAR:
                if ((x as RegularLabel).principal === (y as RegularLabel).principal) return true
                break
            case LabelKind.QUARANTINED: {
                const xq = x as QuarantinedLabel, yq = y as QuarantinedLabel
                if (xq.principal === yq.principal && sameTag(xq.quarantineTag, yq.quarantineTag)) return true
                break
            }
            case LabelKind.QFALSE:
                if (sameTag((x as QFalseLabel).quarantineTag, (y as QFalseLabel).quarantineTag)) return true
                break
            case LabelKind.WILDCARD_QFALSE:
                if ((x as WildcardQFalseLabel).nodeId === (y as WildcardQFalseLabel).nodeId) return true
                break
        }
    }
    // QFalse implies QuarantinedLabel with the same tag.
    if (x.kind === LabelKind.QFALSE && y.kind === LabelKind.QUARANTINED) {
        return sameTag((x as QFalseLabel).quarantineTag, (y as QuarantinedLabel).quarantineTag)
    }
    // WildcardQFalse implies QuarantinedLabel with the same nodeId.
    if (x.kind === LabelKind.WILDCARD_QFALSE && y.kind === LabelKind.QUARANTINED) {
        return (x as WildcardQFalseLabel).nodeId === (y as QuarantinedLabel).quarantineTag.nodeId
    }
    // WildcardQFalse implies QFalseLabel with the same nodeId.
    if (x.kind === LabelKind.WILDCARD_QFALSE && y.kind === LabelKind.QFALSE) {
        return (x as WildcardQFalseLabel).nodeId === (y as QFalseLabel).quarantineTag.nodeId
    }
    return false
}

test('labelImplies matches the documented rules (all label kinds)', () => {
    fc.assert(fc.property(arbAnyLabel, arbAnyLabel, (x, y) => {
        return labelImplies(x, y) === specImplies(x, y)
    }), RUNS)
})

test('labelImplies is reflexive', () => {
    fc.assert(fc.property(arbAnyLabel, x => labelImplies(x, x)), RUNS)
})

test('QFalse implies a same-tag QuarantinedLabel, and only then', () => {
    fc.assert(fc.property(arbTag, arbTag, arbRegular, (t1, t2, r) => {
        const qfalse = new QFalseLabel(t1)
        const quar = QuarantinedLabel.fromRegular(r, t2)
        return labelImplies(qfalse, quar) === sameTag(t1, t2)
    }), RUNS)
})

test('WildcardQFalse implies a same-node QuarantinedLabel/QFalse, and only then', () => {
    fc.assert(fc.property(fc.constantFrom(...NODES), arbTag, arbRegular, (node, tag, r) => {
        const wild = new WildcardQFalseLabel(node)
        const quar = QuarantinedLabel.fromRegular(r, tag)
        const qfalse = new QFalseLabel(tag)
        const same = node === tag.nodeId
        assert.equal(labelImplies(wild, quar), same, 'wildcard => quarantined')
        assert.equal(labelImplies(wild, qfalse), same, 'wildcard => qfalse')
        return true
    }), RUNS)
})

test('no undocumented cross-type implications hold', () => {
    fc.assert(fc.property(arbAnyLabel, arbAnyLabel, (x, y) => {
        if (labelImplies(x, y) && x.kind !== y.kind) {
            // Only three documented cross-type directions are allowed.
            const allowed =
                (x.kind === LabelKind.QFALSE && y.kind === LabelKind.QUARANTINED) ||
                (x.kind === LabelKind.WILDCARD_QFALSE && y.kind === LabelKind.QUARANTINED) ||
                (x.kind === LabelKind.WILDCARD_QFALSE && y.kind === LabelKind.QFALSE)
            return allowed
        }
        return true
    }), RUNS)
})

// ---------------------------------------------------------------------------
// DCLabels containing quarantined labels: preorder / equivalence
// ---------------------------------------------------------------------------

test('flowsTo is reflexive over quarantined DCLabels', () => {
    fc.assert(fc.property(arbDCLabelQuar, a => a.flowsTo(a)), RUNS)
})

test('flowsTo is transitive over quarantined DCLabels', () => {
    fc.assert(fc.property(arbDCLabelQuar, arbDCLabelQuar, arbDCLabelQuar, (a, b, c) => {
        fc.pre(a.flowsTo(b) && b.flowsTo(c))
        return a.flowsTo(c)
    }), RUNS)
})

test('equals is reflexive and symmetric over quarantined DCLabels', () => {
    fc.assert(fc.property(arbDCLabelQuar, arbDCLabelQuar, (a, b) => {
        assert.ok(a.equals(a), 'reflexive')
        assert.equal(a.equals(b), b.equals(a), 'symmetric')
        return true
    }), RUNS)
})
