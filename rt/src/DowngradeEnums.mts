import { Level } from './Level.mjs';


export enum DowngradeKind {
	VALUE = 1,
	BLOCKING = 2,
	MAILBOX = 3
}
    
export enum DowngradeDimension {
	CONFIDENTIALITY = 1,
	INTEGRITY = 2,
	BOTH = 3,  // Cross-dimensional downgrade (changes both confidentiality and integrity)
}

export enum DowngradeErrorReason {
	INTEGRITY_MISMATCH = 1,
	CONFIDENTIALITY_MISMATCH = 2,
	INSUFFICIENT_AUTHORITY = 3,
	BLOCKING_LEVEL_MISMATCH = 4,
	ROBUSTNESS_VIOLATION = 5,
	TRANSPARENCY_VIOLATION = 6
}

export enum ValueDowngradeGranularity {
	TYPE_ONLY = 1,
	BOTH_VALUE_AND_TYPE = 2,
}

export type SuccessfulDowngradeResult = {
	kind: "SUCCESS";
};

export type FailedDowngradeResult = {
	kind: "FAILURE";
	reason: DowngradeErrorReason;
};

export type DowngradeResult = SuccessfulDowngradeResult | FailedDowngradeResult;

export const DowngradeResultSuccess: SuccessfulDowngradeResult = { kind: "SUCCESS" };

export function DowngradeError(reason: DowngradeErrorReason): FailedDowngradeResult {
	return { kind: "FAILURE", reason };
}

export type ValidateDowngradeParams = {
	downgradeKind: DowngradeKind;
	levFrom: Level;
	levTo: Level;
	authorityLevel: Level;
	downgradeDimension: DowngradeDimension;
	blockLevel?: Level ;
	operationDescription?: string;
	pcLevel?: Level;  // PC level for NMIFC error messages
	operandLevel?: Level;  // label of the authority operand, joined with the pc in the NMIFC check
};
