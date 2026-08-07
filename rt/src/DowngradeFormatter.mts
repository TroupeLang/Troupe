import { Level } from './Level.mjs';
import { DC_INTG_LITERALS, DC_CONF_LITERALS } from './levels/DCLabels/dcl_pp_config.mjs';
import { DowngradeKind, DowngradeDimension, ValidateDowngradeParams, DowngradeErrorReason } from './DowngradeEnums.mjs';
import { ImplementationError } from './TroupeError.mjs';

// Individual formatter functions (formatIntegrityMismatchMsg, etc.) remain here and are exported.
export function formatIntegrityMismatchMsg(operationDescription: string, dataLevel: Level, targetLevel: Level): string {
    return `Integrity level mismatch for ${operationDescription}\n` +
           ` | integrity level of the data: ${dataLevel.integrity.stringRep(DC_INTG_LITERALS)}\n` +
           ` | integrity level of the target: ${targetLevel.integrity.stringRep(DC_INTG_LITERALS)}`;
}

export function formatConfidentialityMismatchMsg(operationDescription: string, dataLevel: Level, targetLevel: Level): string {
    return `Confidentiality level mismatch for ${operationDescription}\n` +
           ` | confidentiality level of the data: ${dataLevel.confidentiality.stringRep(DC_CONF_LITERALS)}\n` +
           ` | confidentiality level of the target: ${targetLevel.confidentiality.stringRep(DC_CONF_LITERALS)}`;
}

export function formatPiniBlockingLevelMismatchMsg(operationDescription: string, currentBlockingLevel: Level, targetBlockingLevel: Level): string {
    return `Current blocking level does not flow to the target level of the ${operationDescription}\n` +
           ` | current blocking level: ${currentBlockingLevel.stringRep()}\n` +
           ` | target blocking level: ${targetBlockingLevel.stringRep()}`;
}

export function formatPiniInsufficientAuthorityMsg(operationDescription: string, fromBlockingLevel: Level, authorityValLevel: Level, toBlockingLevel: Level): string {
    return `Not enough authority for ${operationDescription}\n` +
           ` | from level of the blocking level: ${fromBlockingLevel.stringRep()}\n` +
           ` | level of the authority: ${authorityValLevel.stringRep()}\n`  +
           ` | to level of the blocking level: ${toBlockingLevel.stringRep()}`;
}

// The mailbox arm's operands are the two ends of the move the operation makes:
// where the mailbox stands now, and where the operation puts it. Both are named
// the same way in every mailbox message so one term keeps one meaning.
export function formatMboxBlockingLevelMismatchMsg(operationDescription: string, currentBlockingLevel: Level, targetMailboxLevel: Level): string {
    return `Blocking level too sensitive for ${operationDescription}\n` +
           ` | blocking level now           : ${currentBlockingLevel.stringRep()}\n` +
           ` | earlier level of the mailbox : ${targetMailboxLevel.stringRep()}`;
}

export function formatMboxInsufficientAuthorityMsg(operationDescription: string, authorityProvidedLevel: Level, currentMailboxLevel: Level, targetMailboxLevel: Level): string {
    return `${operationDescription} cannot put the mailbox back to its earlier level with this authority.\n` +
           ` | level of the mailbox now     : ${currentMailboxLevel.stringRep()}\n` +
           ` | earlier level of the mailbox : ${targetMailboxLevel.stringRep()}\n` +
           ` | level of the authority       : ${authorityProvidedLevel.stringRep()}`;
}

export function formatMboxConfidentialityMismatchMsg(operationDescription: string, currentMailboxLevel: Level, targetMailboxLevel: Level): string {
    return `Confidentiality level mismatch for ${operationDescription}\n` +
           ` | confidentiality of the mailbox now     : ${currentMailboxLevel.confidentiality.stringRep(DC_CONF_LITERALS)}\n` +
           ` | earlier confidentiality of the mailbox : ${targetMailboxLevel.confidentiality.stringRep(DC_CONF_LITERALS)}`;
}

export function formatMboxIntegrityMismatchMsg(operationDescription: string, currentMailboxLevel: Level, targetMailboxLevel: Level): string {
    return `Integrity level mismatch for ${operationDescription}\n` +
           ` | integrity of the mailbox now     : ${currentMailboxLevel.integrity.stringRep(DC_INTG_LITERALS)}\n` +
           ` | earlier integrity of the mailbox : ${targetMailboxLevel.integrity.stringRep(DC_INTG_LITERALS)}`;
}

export function formatValueInsufficientAuthorityMsg(operationDescription: string, dataLevel: Level, authorityLevel: Level, targetLevel: Level): string {
    return `Not enough authority for ${operationDescription}\n` +
           ` | level of the data: ${dataLevel.stringRep()}\n` +
           ` | level of the authority: ${authorityLevel.stringRep()}\n` +
           ` | target level of the ${operationDescription}: ${targetLevel.stringRep()}`;
}

function fromLevelLabel(kind: DowngradeKind): string {
    switch (kind) {
        case DowngradeKind.BLOCKING: return "current blocking level";
        case DowngradeKind.MAILBOX:  return "level of the mailbox now";
        default:                     return "level of the data";
    }
}

export function formatRobustnessViolationMsg(operationDescription: string, fromLevel: Level, targetLevel: Level, pcLevel: Level, kind: DowngradeKind): string {
    return `NMIFC robustness violation for ${operationDescription}\n` +
           ` | The integrity of the data and PC do not permit this ${operationDescription}.\n` +
           ` | ${fromLevelLabel(kind)}: ${fromLevel.stringRep()} (corrupt: ${fromLevel.isCorrupt()})\n` +
           ` | target level: ${targetLevel.stringRep()}\n` +
           ` | PC level: ${pcLevel.stringRep()} (corrupt: ${pcLevel.isCorrupt()})`;
}

export function formatTransparencyViolationMsg(operationDescription: string, fromLevel: Level, targetLevel: Level, pcLevel: Level, kind: DowngradeKind): string {
    return `NMIFC transparency violation for ${operationDescription}\n` +
           ` | The confidentiality of the data and PC do not permit this endorsement.\n` +
           ` | ${fromLevelLabel(kind)}: ${fromLevel.stringRep()} (corrupt: ${fromLevel.isCorrupt()})\n` +
           ` | target level: ${targetLevel.stringRep()}\n` +
           ` | PC level: ${pcLevel.stringRep()} (corrupt: ${pcLevel.isCorrupt()})`;
}

// Non-exported helper for BLOCKING kind
function getBlockDowngradeErrorMessageForReason(
    reason: DowngradeErrorReason,
    operationDescription: string,
    levFrom: Level,
    levTo: Level,
    authorityLevel: Level,
    pcLevel?: Level
): string {
    switch (reason) {
        case DowngradeErrorReason.INTEGRITY_MISMATCH: return formatIntegrityMismatchMsg(operationDescription, levFrom, levTo);
        case DowngradeErrorReason.CONFIDENTIALITY_MISMATCH: return formatConfidentialityMismatchMsg(operationDescription, levFrom, levTo);
        case DowngradeErrorReason.BLOCKING_LEVEL_MISMATCH: return formatPiniBlockingLevelMismatchMsg(operationDescription, levFrom, levTo);
        case DowngradeErrorReason.INSUFFICIENT_AUTHORITY: return formatPiniInsufficientAuthorityMsg(operationDescription, levFrom, authorityLevel, levTo);
        case DowngradeErrorReason.ROBUSTNESS_VIOLATION:
            if (!pcLevel) throw new ImplementationError("pcLevel required for ROBUSTNESS_VIOLATION");
            return formatRobustnessViolationMsg(operationDescription, levFrom, levTo, pcLevel, DowngradeKind.BLOCKING);
        case DowngradeErrorReason.TRANSPARENCY_VIOLATION:
            if (!pcLevel) throw new ImplementationError("pcLevel required for TRANSPARENCY_VIOLATION");
            return formatTransparencyViolationMsg(operationDescription, levFrom, levTo, pcLevel, DowngradeKind.BLOCKING);
        default:
            const _exhaustiveBlockReason: never = reason;
            throw new ImplementationError(`Unexpected reason for BLOCKING: ${_exhaustiveBlockReason}`);
    }
}

// Non-exported helper for MAILBOX kind
function getMailboxDowngradeErrorMessageForReason(
    reason: DowngradeErrorReason,
    operationDescription: string,
    levFrom: Level,
    levTo: Level,
    authorityLevel: Level,
    currentBlockingLevelForCheck: Level,
    pcLevel?: Level
): string {
    switch (reason) {
        case DowngradeErrorReason.INTEGRITY_MISMATCH: return formatMboxIntegrityMismatchMsg(operationDescription, levFrom, levTo);
        case DowngradeErrorReason.CONFIDENTIALITY_MISMATCH: return formatMboxConfidentialityMismatchMsg(operationDescription, levFrom, levTo);
        case DowngradeErrorReason.BLOCKING_LEVEL_MISMATCH: return formatMboxBlockingLevelMismatchMsg(operationDescription, currentBlockingLevelForCheck, levTo);
        case DowngradeErrorReason.INSUFFICIENT_AUTHORITY: return formatMboxInsufficientAuthorityMsg(operationDescription, authorityLevel, levFrom, levTo);
        // enableRangedReceive decides its downgrade under NMIFC like every other
        // downgrade, so both NMIFC reasons reach this arm.
        case DowngradeErrorReason.ROBUSTNESS_VIOLATION:
            if (!pcLevel) throw new ImplementationError("pcLevel required for ROBUSTNESS_VIOLATION");
            return formatRobustnessViolationMsg(operationDescription, levFrom, levTo, pcLevel, DowngradeKind.MAILBOX);
        case DowngradeErrorReason.TRANSPARENCY_VIOLATION:
            if (!pcLevel) throw new ImplementationError("pcLevel required for TRANSPARENCY_VIOLATION");
            return formatTransparencyViolationMsg(operationDescription, levFrom, levTo, pcLevel, DowngradeKind.MAILBOX);
        default:
            const _exhaustiveMboxReason: never = reason;
            throw new ImplementationError(`Unexpected reason for MAILBOX: ${_exhaustiveMboxReason}`);
    }
}

// Non-exported helper for VALUE kind
function getValueDowngradeErrorMessageForReason(
    reason: DowngradeErrorReason,
    operationDescription: string,
    levFrom: Level,
    levTo: Level,
    authorityLevel: Level,
    currentBlockingLevelForCheck: Level,
    pcLevel?: Level
): string {
    if (reason === DowngradeErrorReason.BLOCKING_LEVEL_MISMATCH && currentBlockingLevelForCheck === null) {
        throw new ImplementationError("Internal inconsistency: currentBlockingLevelForCheck is null for VALUE kind with BLOCKING_LEVEL_MISMATCH reason.");
    }
    switch (reason) {
        case DowngradeErrorReason.INTEGRITY_MISMATCH: return formatIntegrityMismatchMsg(operationDescription, levFrom, levTo);
        case DowngradeErrorReason.CONFIDENTIALITY_MISMATCH: return formatConfidentialityMismatchMsg(operationDescription, levFrom, levTo);
        case DowngradeErrorReason.BLOCKING_LEVEL_MISMATCH:
            return formatPiniBlockingLevelMismatchMsg(operationDescription, currentBlockingLevelForCheck!, levTo);
        case DowngradeErrorReason.INSUFFICIENT_AUTHORITY: return formatValueInsufficientAuthorityMsg(operationDescription, levFrom, authorityLevel, levTo);
        case DowngradeErrorReason.ROBUSTNESS_VIOLATION:
            if (!pcLevel) throw new ImplementationError("pcLevel required for ROBUSTNESS_VIOLATION");
            return formatRobustnessViolationMsg(operationDescription, levFrom, levTo, pcLevel, DowngradeKind.VALUE);
        case DowngradeErrorReason.TRANSPARENCY_VIOLATION:
            if (!pcLevel) throw new ImplementationError("pcLevel required for TRANSPARENCY_VIOLATION");
            return formatTransparencyViolationMsg(operationDescription, levFrom, levTo, pcLevel, DowngradeKind.VALUE);
        default:
            const _exhaustiveValueReason: never = reason;
            throw new ImplementationError(`Unexpected reason for VALUE: ${_exhaustiveValueReason}`);
    }
}

export function getDowngradeErrorMessage(params: ValidateDowngradeParams, reason: DowngradeErrorReason): string {
    const { levFrom, levTo, authorityLevel, downgradeKind, blockLevel: currentBlockingLevelForCheck, pcLevel } = params;
    let opDesc = params.operationDescription; // Allow opDesc to be potentially modified

    switch (downgradeKind) {
        case DowngradeKind.BLOCKING:
            if (typeof opDesc !== 'string') {
                throw new ImplementationError("operationDescription is required for BLOCKING downgradeKind.");
            }
            return getBlockDowngradeErrorMessageForReason(reason, opDesc, levFrom, levTo, authorityLevel, pcLevel);
        case DowngradeKind.MAILBOX:
            // The caller names the primitive it is refusing; there is no standard
            // description to fall back on now that the mailbox is moved by more
            // than one operation.
            if (typeof opDesc !== 'string') {
                throw new ImplementationError("operationDescription is required for MAILBOX downgradeKind.");
            }
            if (currentBlockingLevelForCheck === null) {
                throw new ImplementationError("currentBlockingLevelForCheck is required for MAILBOX downgradeKind.");
            }
            return getMailboxDowngradeErrorMessageForReason(reason, opDesc, levFrom, levTo, authorityLevel, currentBlockingLevelForCheck, pcLevel);
        case DowngradeKind.VALUE:
            opDesc = opDesc || "value downgrade"; // Default opDesc for value
            return getValueDowngradeErrorMessageForReason(reason, opDesc, levFrom, levTo, authorityLevel, currentBlockingLevelForCheck, pcLevel);
        default:
            const _exhaustiveKind: never = downgradeKind;
            throw new ImplementationError(`Unhandled DowngradeKind: ${_exhaustiveKind}`);
    }
} 