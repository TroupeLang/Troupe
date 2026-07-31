import { AbstractLevel, AbstractLevelSystem } from '../../AbstractLevel.mjs';
import { DowngradeKind, DowngradeDimension, DowngradeResult, DowngradeErrorReason, DowngradeResultSuccess, DowngradeError } from '../../DowngradeEnums.mjs';
import { tagsetStringRep } from '../tagsets.mjs';
import { Category
       , CNF
       , CNF_FALSE
       , CNF_TRUE
       , implies
       , conjunction
       , disjunction
    } from './cnf.mjs'
import { DC_CONF_LITERALS, DC_DELIM_LEFT, DC_DELIM_RIGHT, DC_DELIM_SEP, DC_IFC_TOP, DC_INTG_LITERALS, DC_TRUST_ROOT, DC_TRUST_NULL, getDelimiters, LabelStringFormat, getDefaultLabelStringFormat } from './dcl_pp_config.mjs';
import { Label, LabelKind, RegularLabel, QuarantinedLabel, QFalseLabel, WildcardQFalseLabel, QuarantineTag } from './label.mjs';

/**
 * Options for quarantine-aware implication checks.
 *
 * When performing actsFor checks for serialization to a specific node,
 * we coalesce the authority with a wildcard quarantine authority for that node.
 * This allows the check to succeed for any quarantined labels from the target node,
 * while naturally failing for quarantined labels from other nodes.
 */
export interface QuarantineOptions {
    /** The target node ID for which we're checking the implication */
    node: string;
}

// import { getCliArgs, TroupeCliArg } from '../../TroupeCliArgs.mjs';
// import { mkLogger } from '../../logger.mjs';

// const argv = getCliArgs();
// const logLevel = argv[TroupeCliArg.Debug] ? 'debug' : 'info';
// const logger = mkLogger('DCLabel', logLevel);
// const debug = x => logger.debug(x);

export class DCLabel extends AbstractLevel<DCLabel> {
    integrity: CNF
    confidentiality: CNF

    get dataLevel () {
        return IFC_BOT
    }

    _cachedStringRep: { [key: number]: string } = {};

    constructor(c: CNF, i: CNF) {
        super ()
        this.confidentiality = c;
        this.integrity = i;
        
    }

    flowsTo(other: DCLabel): boolean {
        /* 
        S_2 ==> S1         I1 ==> I_2
        -------------------------------
        <S_1, I_1> flowsto <S_2, I_2>

        assuming this = <S_1, I_1>

        */
        // Reflexivity (a ⊑ a) and the lattice bounds (⊥ ⊑ x, x ⊑ ⊤), decided by
        // pointer identity: labels are immutable and IFC_BOT/IFC_TOP are canonical
        // singletons, so these checks are sound; the CNF entailment below remains
        // the general case.
        if (this === other || this === IFC_BOT || other === IFC_TOP) {
            return true;
        }
        return implies(other.confidentiality, this.confidentiality)
            && implies(this.integrity, other.integrity);

    }

    actsFor(other: DCLabel, options?: QuarantineOptions): boolean {
        /*
        returns true if this label actsfor another label.

        S_1 ==> S2         I_1 ==> I_2
        -------------------------------
        <S_1, I_1> actsfor <S_2, I_2>

        assuming this = <S_1, I_1>
        */

        if (!options) {
            // Original behavior
            return implies(this.confidentiality, other.confidentiality)
                && implies(this.integrity, other.integrity);
        }

        // Coalescing approach: combine this label with wildcard authority for the target node.
        // The wildcard authority can imply any quarantined label from the same node,
        // so the implication naturally succeeds for same-node quarantined labels
        // and fails for labels quarantined from different nodes.
        const wildcardAuth = createWildcardQuarantineAuthority(options.node);
        const coalescedThis = this.coalesce(wildcardAuth);

        return implies(coalescedThis.confidentiality, other.confidentiality)
            && implies(coalescedThis.integrity, other.integrity);
    }

    equals(other: DCLabel): boolean {
        return this.flowsTo(other) && other.flowsTo(this);
    }

    isTagsetCompatible(): boolean | Set<string> {
        if (this.confidentiality.categories.size == 0) {
            return false;
        }

        if (this.integrity.categories.size != 1) {
            return false;
        }
        const _the_integrity: Category =
            this.integrity.categories.values().next().value;

        const integrityLabels = _the_integrity.getLabels();

        // All labels must be RegularLabels for tagset compatibility
        for (const label of integrityLabels) {
            if (label.kind !== LabelKind.REGULAR) {
                return false;
            }
        }

        // Build a set of principal names from integrity labels
        const s: Set<string> = new Set(
            integrityLabels.map(l => (l as RegularLabel).principal)
        );

        for (let cat of this.confidentiality.categories) {
            const catLabels = cat.getLabels();
            if (catLabels.length == 1) {
                const label = catLabels[0];
                // Must be a RegularLabel
                if (label.kind !== LabelKind.REGULAR) {
                    return false;
                }
                const principal = (label as RegularLabel).principal;
                if (!s.has(principal)) {
                    return false;
                }
            } else {
                return false;
            }
        }

        return s;
    }


    stringRep(format?: LabelStringFormat): string {
        // Guard: LVal.stringRep() duck-types v.stringRep(omitLevels, taintRef),
        // so format may receive a boolean or object — only accept valid enum values.
        const fmt = (format === LabelStringFormat.V1
                  || format === LabelStringFormat.V2
                  || format === LabelStringFormat.V2Full)
            ? format
            : getDefaultLabelStringFormat();

        if (this._cachedStringRep[fmt] !== undefined) {
            return this._cachedStringRep[fmt];
        }

        const delims = getDelimiters(fmt);
        let result: string;

        if (fmt === LabelStringFormat.V1) {
            // V1: use shorthands for special labels
            if (this.flowsTo(IFC_BOT)) {
                result = delims.left + delims.right;
            } else if (IFC_TOP.flowsTo(this)) {
                result = delims.left + DC_IFC_TOP + delims.right;
            } else if (TRUST_ROOT.flowsTo(this) && this.flowsTo(TRUST_ROOT)) {
                result = delims.left + DC_TRUST_ROOT + delims.right;
            } else if (TRUST_NULL.flowsTo(this) && this.flowsTo(TRUST_NULL)) {
                result = delims.left + DC_TRUST_NULL + delims.right;
            } else {
                let s = this.isTagsetCompatible();
                if (s) {
                    result = tagsetStringRep(s as Set<string>, delims.left, delims.right);
                } else {
                    result = DC_DELIM_LEFT +
                        this.confidentiality.stringRep(DC_CONF_LITERALS) +
                        DC_DELIM_SEP +
                        this.integrity.stringRep(DC_INTG_LITERALS) +
                        DC_DELIM_RIGHT;
                }
            }
        } else if (fmt === LabelStringFormat.V2Full) {
            // V2Full: always use full <conf;intg> form with explicit null names
            result = DC_DELIM_LEFT +
                this.confidentiality.stringRep(DC_CONF_LITERALS) +
                DC_DELIM_SEP +
                this.integrity.stringRep(DC_INTG_LITERALS) +
                DC_DELIM_RIGHT;
        } else {
            // V2: DC format with null-component elision
            const confStr = this.confidentiality.categories.size === 0
                ? ""
                : this.confidentiality.stringRep(DC_CONF_LITERALS);
            const intgStr = this.integrity.categories.size === 0
                ? ""
                : this.integrity.stringRep(DC_INTG_LITERALS);
            result = DC_DELIM_LEFT + confStr + DC_DELIM_SEP + intgStr + DC_DELIM_RIGHT;
        }

        this._cachedStringRep[fmt] = result;
        return result;
    }

    
    /* 

    L1 ⊔ L2 = <S1 /\ S2, I1 \/ I2) 
    L1 ⊓ L2 = <S1 \/ S2, I1 /\ I2) 

    */

    join (other:DCLabel): DCLabel {
        // Unit (⊥ ⊔ x = x) and idempotence (x ⊔ x = x): the operand object is
        // the result. Returning it — not an equal fresh label — preserves
        // pointer identity through join chains, which is what keeps the
        // identity fast paths of flowsTo effective downstream.
        if (this === other || this === IFC_BOT) { return other; }
        if (other === IFC_BOT) { return this; }
        const conf = conjunction (this.confidentiality, other.confidentiality);
        const intg = disjunction (this.integrity, other.integrity);
        // Absorption: conjunction/disjunction return a dominating operand's
        // component unchanged; when both of an operand's components are
        // preserved, that operand is the join. Labels are immutable, so
        // sharing the object is sound.
        if (conf === this.confidentiality && intg === this.integrity) { return this; }
        if (conf === other.confidentiality && intg === other.integrity) { return other; }
        return new DCLabel (conf, intg)
    }

    meet (other:DCLabel): DCLabel {
        return new DCLabel (
             disjunction (this.confidentiality, other.confidentiality)
           , conjunction (this.integrity, other.integrity)
        );
    }

    /**
     * Coalesce operator (⊛): combines two labels using conjunction on both components
     * ⟨S₁, I₁⟩ ⊛ ⟨S₂, I₂⟩ = ⟨S₁ ∧ S₂, I₁ ∧ I₂⟩
     * Creates a combined authority that acts for both input authorities.
     */
    coalesce(other: DCLabel): DCLabel {
        return new DCLabel(
            conjunction(this.confidentiality, other.confidentiality),
            conjunction(this.integrity, other.integrity)
        );
    }

    toJSON () {
        return { confidentiality: this.confidentiality.toJSON() 
               , integrity: this.integrity.toJSON()  
        }
    }

    static fromJSON (o: { confidentiality: [[string]]
                        ; integrity: [[string]]; }) {
        return new DCLabel(CNF.fromJSON(o.confidentiality)
                         , CNF.fromJSON(o.integrity))
    }

    // TODO: Performance optimization - if this path becomes a bottleneck, consider
    // adding a fromSingleTagNormalized() variant that skips normalization for cases
    // where the frontend (e.g., IR processor/lexer) has already normalized the tag.
    static fromSingleTag(s: string): DCLabel {
        // Create a RegularLabel (which handles normalization and validation)
        const label = new RegularLabel(s);
        const cat = new Category([label]);
        const cnf = new CNF(new Set([cat]));
        return new DCLabel(cnf, cnf);
    }

    /**
     * Check if this label contains any quarantined components (QuarantinedLabel or QFalseLabel).
     */
    hasQuarantinedLabels(): boolean {
        const checkCNF = (cnf: CNF): boolean => {
            for (const cat of cnf.categories) {
                for (const label of cat.getLabels()) {
                    if (label.isQuarantined()) return true;
                }
            }
            return false;
        };
        return checkCNF(this.confidentiality) || checkCNF(this.integrity);
    }

    /**
     * Quarantine this label by converting all RegularLabels to QuarantinedLabels.
     * CNF_FALSE components become QFalseLabel.
     *
     * @param tag The quarantine tag identifying this quarantine session
     * @returns A new DCLabel with quarantined components
     */
    quarantine(tag: QuarantineTag): DCLabel {
        return new DCLabel(
            this.quarantineCNF(this.confidentiality, tag),
            this.quarantineCNF(this.integrity, tag)
        );
    }

    /**
     * Result type for restoreForNode operation.
     * On success: { success: true, label: DCLabel }
     * On failure: { success: false, mismatchedNodes: Set<string> }
     */
    static RestoreResult: {
        success(label: DCLabel): { success: true; label: DCLabel };
        failure(mismatchedNodes: Set<string>): { success: false; mismatchedNodes: Set<string> };
    } = {
        success: (label: DCLabel) => ({ success: true as const, label }),
        failure: (mismatchedNodes: Set<string>) => ({ success: false as const, mismatchedNodes })
    };

    /**
     * Restore quarantined labels for serialization to a specific target node.
     *
     * If this label contains quarantined labels whose quarantine node matches the target,
     * those labels are restored to their original form. If there are quarantined labels
     * for a different node, returns failure with the mismatched node IDs.
     *
     * @param targetNodeId The ID of the node we're serializing to
     * @returns RestoreResult indicating success with restored label, or failure with mismatched nodes
     */
    restoreForNode(targetNodeId: string): { success: true; label: DCLabel } | { success: false; mismatchedNodes: Set<string> } {
        const mismatchedNodes = new Set<string>();

        const restoredConf = this.restoreCNFForNode(this.confidentiality, targetNodeId, mismatchedNodes);
        const restoredIntg = this.restoreCNFForNode(this.integrity, targetNodeId, mismatchedNodes);

        if (mismatchedNodes.size > 0) {
            return DCLabel.RestoreResult.failure(mismatchedNodes);
        }

        return DCLabel.RestoreResult.success(new DCLabel(restoredConf, restoredIntg));
    }

    /**
     * Collect all quarantine source node IDs from this label.
     * Useful for error reporting when quarantine forward checks fail.
     */
    getQuarantineSourceNodes(): Set<string> {
        const nodes = new Set<string>();
        const collectFromCNF = (cnf: CNF) => {
            for (const cat of cnf.categories) {
                for (const label of cat.getLabels()) {
                    const tag = label.getQuarantineTag();
                    if (tag) {
                        nodes.add(tag.nodeId);
                    }
                }
            }
        };
        collectFromCNF(this.confidentiality);
        collectFromCNF(this.integrity);
        return nodes;
    }

    /**
     * Helper to restore quarantined labels in a CNF for a specific target node.
     * Collects mismatched node IDs into the provided set.
     * Always returns a valid CNF; caller checks mismatchedNodes to determine success.
     */
    private restoreCNFForNode(cnf: CNF, targetNodeId: string, mismatchedNodes: Set<string>): CNF {
        const newCategories: Category[] = [];

        for (const cat of cnf.categories) {
            const newLabels: Label[] = [];
            for (const label of cat.getLabels()) {
                if (label.kind === LabelKind.QUARANTINED) {
                    const qlabel = label as QuarantinedLabel;
                    if (qlabel.quarantineTag.nodeId !== targetNodeId) {
                        mismatchedNodes.add(qlabel.quarantineTag.nodeId);
                        newLabels.push(label);  // keep as-is
                    } else {
                        newLabels.push(qlabel.restore());
                    }
                } else if (label.kind === LabelKind.QFALSE) {
                    const qfalse = label as QFalseLabel;
                    if (qfalse.quarantineTag.nodeId !== targetNodeId) {
                        mismatchedNodes.add(qfalse.quarantineTag.nodeId);
                        newLabels.push(label);  // keep as-is
                    }
                    // else: QFalse restores to false in a disjunction (false ∨ X = X),
                    // so we don't add anything to newLabels.
                } else {
                    newLabels.push(label);
                }
            }
            newCategories.push(new Category(newLabels));
        }

        return new CNF(new Set(newCategories));
    }

    /**
     * Helper to quarantine a single CNF component.
     */
    private quarantineCNF(cnf: CNF, tag: QuarantineTag): CNF {
        // Quarantine all labels in all categories
        const newCategories: Category[] = [];
        for (const cat of cnf.categories) {
            if (cat.isEmpty()) {
                // Empty category (false clause) becomes qfalse
                const qfalse = new QFalseLabel(tag);
                newCategories.push(new Category([qfalse]));
            } else {
                const newLabels: Label[] = [];
                for (const label of cat.getLabels()) {
                    if (label.kind === LabelKind.REGULAR) {
                        // Convert RegularLabel to QuarantinedLabel
                        newLabels.push(QuarantinedLabel.fromRegular(label as RegularLabel, tag));
                    } else {
                        // Already quarantined - keep as is
                        newLabels.push(label);
                    }
                }
                newCategories.push(new Category(newLabels));
            }
        }
        return new CNF(new Set(newCategories));
    }

    /**
     * Returns the reflection of this label: <i, c> for a label <c, i>
     */
    reflection(): DCLabel {
        return new DCLabel(this.integrity, this.confidentiality);
    }

    /**
     * A label <S, I> is corrupt iff it does not flow to its reflection <I, S>.
     * This simplifies to: NOT (I ⟹ S), i.e., integrity does not imply confidentiality.
     */
    isCorrupt(): boolean {
        return !implies(this.integrity, this.confidentiality);
    }

}



/*  
                   ⊤ = <False, True>  (most secret, least trusted)

<True, True>                      <False, False>  (TOP TRUST  = most secret, most trusted)

                   ⊥ = <True, False>  (most public, least garbage)
*/

/// see fabric paper https://www.cs.cornell.edu/andru/papers/jfabric/jfabric.pdf 
/// for the intuition about trust


export const IFC_BOT = new DCLabel(CNF_TRUE, CNF_FALSE)
export const IFC_TOP = new DCLabel(CNF_FALSE, CNF_TRUE)
export const TRUST_NULL = new DCLabel(CNF_TRUE, CNF_TRUE)
export const TRUST_ROOT = new DCLabel(CNF_FALSE, CNF_FALSE)


export class DCLevelSystem extends AbstractLevelSystem<DCLabel> {
    BOT = IFC_BOT
    TOP = IFC_TOP
    NULL = TRUST_NULL
    ROOT = TRUST_ROOT
    flowsTo(a: DCLabel, b: DCLabel): boolean {
        return a.flowsTo (b);   
    }

    actsFor(a: DCLabel, b: DCLabel, options?: QuarantineOptions): boolean {
        return a.actsFor(b, options);
    }

    /**
     * DC privilege relation "x may flow to y given authority auth":
     *   secrecy:   (S_y ∧ S_auth) ⟹ S_x
     *   integrity: (I_x ∧ I_auth) ⟹ I_y
     * i.e. auth, treated as authority, covers the flow of x down to y on both
     * axes. With auth = TRUST_NULL this degenerates to plain `x flowsTo y`.
     */
    privFlowsTo(auth: DCLabel, x: DCLabel, y: DCLabel): boolean {
        const enough_confidentiality =
            implies(conjunction(y.confidentiality, auth.confidentiality), x.confidentiality)
        const enough_integrity =
            implies(conjunction(x.integrity, auth.integrity), y.integrity)
        return enough_confidentiality && enough_integrity
    }



    glb(a: DCLabel, b: DCLabel): DCLabel {
         return a.meet(b)
    }

    // 2025-05-24: TODO
    // - make a better version of this
    lub(...ls: DCLabel[]): DCLabel {
        if (ls.length == 0) {
            return IFC_BOT
        }
        let r = ls[0]
        for (let i = 1; i < ls.length; i ++) {
            r = r.join (ls[i])
        }
        return r;
    }

    fromV1String (str2:string):DCLabel {
        const str1 = str2.trim();
        let str = str1.startsWith ("{") && str1.endsWith ("}") ?
                str1.substring(1, str1.length - 1) :
                str1;
        
        str = str.trim();
        if (str == "") {
            return IFC_BOT
        }

        if (str == "#TOP") {
            return IFC_TOP;
        }

        // Drop empty/whitespace-only segments (e.g. "{alice,,bob}", "{ , }") so a
        // stray or trailing comma does not produce an empty principal. This mirrors
        // the compiler's normalizeV1Label (DCLabels.hs), keeping the two parsers in
        // agreement; an all-empty body reduces to IFC_BOT via lub of no categories.
        const tags = str.split(',').map(t => t.trim()).filter(t => t.length > 0)
        const dcs = tags.map (t => DCLabel.fromSingleTag(t))
        return this.lub (...dcs)
    }

    
    okToDowngradeGeneric (kind: DowngradeKind, dimension: DowngradeDimension) {
        return (( l_from : DCLabel
                , l_to   : DCLabel
                , l_auth : DCLabel
                , bl     : DCLabel
                , isNMIFC: boolean = false
                , pc     : DCLabel = null ) : DowngradeResult => {

            // 2026-01-16: AA: I don't think we actually need this check
            // 
            // if (kind === DowngradeKind.VALUE && !this.flowsTo(bl, l_to)) {
            //     return DowngradeError(DowngradeErrorReason.BLOCKING_LEVEL_MISMATCH);
            // }

            /*

            S_auth /\ S_to ==> S_from        I_auth /\ I_from ==> I_to
            -----------------------------------------------------------
                <S_from, I_from> flowsto_{l_auth} <S_to, I_to>

            */

            switch (dimension) {
                case DowngradeDimension.INTEGRITY:
                    if (!l_from.confidentiality.equals(l_to.confidentiality)) {
                        return DowngradeError(DowngradeErrorReason.CONFIDENTIALITY_MISMATCH);
                    }
                    break;
                case DowngradeDimension.CONFIDENTIALITY:
                    if (!l_from.integrity.equals(l_to.integrity)) {
                        return DowngradeError(DowngradeErrorReason.INTEGRITY_MISMATCH);
                    }
                    break;
                case DowngradeDimension.BOTH:
                    // Cross-dimensional downgrade: allow both dimensions to change
                    // No mismatch checks needed
                    break;
            }

            let enough_confidentiality =
                implies( conjunction ( l_auth.confidentiality
                                ,   l_to.confidentiality)
                    , l_from.confidentiality)
            let enough_integrity =
                implies( conjunction ( l_auth.integrity
                                    , l_from.integrity)
                    , l_to.integrity)

            if (!(enough_confidentiality && enough_integrity)) {
                return DowngradeError(DowngradeErrorReason.INSUFFICIENT_AUTHORITY);
            }

            /*
               NMIFC checks (see Troupe security model document, Section 2.3)

               Robust declassification (confidentiality dimension):
                 (S_auth ∨ I_from ∨ I_pc) ∧ S_to ⟹ S_from

               Transparent endorsement (integrity dimension):
                 I_from ⟹ I_to ∨ (S_from ∧ S_pc)
            */
            if (isNMIFC) {
                // Helper to check robustness (for CONFIDENTIALITY and BOTH)
                const checkRobustness = () => {
                    // Robust declassification: (S_auth ∨ I_from ∨ I_pc) ∧ S_to ⟹ S_from
                    const s_auth_or_i_from_or_i_pc = disjunction(
                        disjunction(l_auth.confidentiality, l_from.integrity),
                        pc.integrity
                    );
                    return implies(
                        conjunction(s_auth_or_i_from_or_i_pc, l_to.confidentiality),
                        l_from.confidentiality
                    );
                };

                // Helper to check transparency (for INTEGRITY and BOTH)
                const checkTransparency = () => {
                    // Transparent endorsement: I_from ⟹ I_to ∨ (S_from ∧ S_pc)
                    const s_from_and_s_pc = conjunction(l_from.confidentiality, pc.confidentiality);
                    const i_to_or_secret = disjunction(l_to.integrity, s_from_and_s_pc);
                    return implies(l_from.integrity, i_to_or_secret);
                };

                switch (dimension) {
                    case DowngradeDimension.CONFIDENTIALITY: {
                        if (!checkRobustness()) {
                            return DowngradeError(DowngradeErrorReason.ROBUSTNESS_VIOLATION);
                        }
                        break;
                    }
                    case DowngradeDimension.INTEGRITY: {
                        if (!checkTransparency()) {
                            return DowngradeError(DowngradeErrorReason.TRANSPARENCY_VIOLATION);
                        }
                        break;
                    }
                    case DowngradeDimension.BOTH: {
                        // Cross-dimensional downgrade: check BOTH robustness AND transparency
                        if (!checkRobustness()) {
                            return DowngradeError(DowngradeErrorReason.ROBUSTNESS_VIOLATION);
                        }
                        if (!checkTransparency()) {
                            return DowngradeError(DowngradeErrorReason.TRANSPARENCY_VIOLATION);
                        }
                        break;
                    }
                }
            }

            return DowngradeResultSuccess;
        }
     )}

    okToEndorse = this.okToDowngradeGeneric (DowngradeKind.VALUE, DowngradeDimension.INTEGRITY)
    okToDeclassify = this.okToDowngradeGeneric (DowngradeKind.VALUE, DowngradeDimension.CONFIDENTIALITY)
    okToCrossDimensionalDowngrade = this.okToDowngradeGeneric (DowngradeKind.VALUE, DowngradeDimension.BOTH)
    okToDowngrade (kind: DowngradeKind, dimension: DowngradeDimension) {
        return this.okToDowngradeGeneric(kind, dimension);
    }


}

export const mkLevel = DCLabel.fromJSON
export type Level = DCLabel
export const levels = new DCLevelSystem ()

/**
 * Creates a quarantine authority label for the given quarantine tag.
 *
 * The quarantine authority contains qfalse, which can authorize downgrading
 * of any quarantined labels with the same quarantine tag.
 *
 * @param tag The quarantine tag identifying this quarantine session
 * @returns A DCLabel suitable for use as quarantine authority
 */
export function createQuarantineAuthority(tag: QuarantineTag): DCLabel {
    const qfalse = new QFalseLabel(tag);
    const cat = new Category([qfalse]);
    const cnf = new CNF(new Set([cat]));
    return new DCLabel(cnf, cnf);
}

/**
 * Creates a wildcard quarantine authority for a given node.
 *
 * The wildcard authority contains WildcardQFalseLabel, which can imply
 * any quarantined label (QuarantinedLabel or QFalseLabel) from the same node,
 * regardless of the specific quarantineId.
 *
 * This is used internally for actsFor checks when serializing to a specific node.
 * Unlike createQuarantineAuthority, this creates an authority that covers ALL
 * quarantine sessions from the given node.
 *
 * @param nodeId The target node ID
 * @returns A DCLabel suitable for use as wildcard quarantine authority
 */
export function createWildcardQuarantineAuthority(nodeId: string): DCLabel {
    const wildcard = new WildcardQFalseLabel(nodeId);
    const cat = new Category([wildcard]);
    const cnf = new CNF(new Set([cat]));
    return new DCLabel(cnf, cnf);
}

// Re-export QuarantineTag for use by other modules
export { QuarantineTag } from './label.mjs';