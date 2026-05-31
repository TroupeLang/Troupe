import { LCopyVal, LVal } from './Lval.mjs';
import { assertIsNTuple, assertIsAuthority, assertIsLevel } from './Asserts.mjs'
import { __unit } from './UnitVal.mjs';
import { lub, glb, okToDeclassify, okToEndorse, okToCrossDimensionalDowngrade}  from './Level.mjs'
import { DowngradeResult, DowngradeDimension, DowngradeErrorReason, DowngradeKind, ValueDowngradeGranularity } from './DowngradeEnums.mjs';
import {
    formatIntegrityMismatchMsg,
    formatConfidentialityMismatchMsg,
    formatPiniBlockingLevelMismatchMsg,
    formatValueInsufficientAuthorityMsg,
    formatRobustnessViolationMsg,
    formatTransparencyViolationMsg
} from './DowngradeFormatter.mjs';
import { ErrorKind } from './TroupeError.mjs';
import { RuntimeInterface } from './RuntimeInterface.mjs';


function stringOfDowngrader (d: DowngradeDimension): string {
    switch (d) {
        case DowngradeDimension.CONFIDENTIALITY: {
            return "declassification"
        }
        case DowngradeDimension.INTEGRITY: {
            return "endorsement"
        }
        case DowngradeDimension.BOTH: {
            return "downgrade"
        }
    }
}

export function downgrader (runtime: RuntimeInterface,
                            dimension: DowngradeDimension,
                            granularity: ValueDowngradeGranularity = ValueDowngradeGranularity.BOTH_VALUE_AND_TYPE) {
    return (arg => {
            const typeOnly = granularity === ValueDowngradeGranularity.TYPE_ONLY;

            assertIsNTuple(arg, 3);
            let argv: LVal = arg.val;
            let data: LVal = argv[0];
            let auth: LVal = argv[1];
            assertIsAuthority(auth);
            let toLevV = argv[2];
            assertIsLevel(toLevV);

            // 2026-03-09; AA & SS
            // Downgrading may fail, depending on the data in `data`, `auth`, and `toLevV`. Hence,
            // we need to raise the blocking label accordingly.
            //
            // - Depending on the given authority. For reference, see the following example of a
            //   leak to the adversary via the termination channel:
            //   `tests/rt/neg/ifc/declassify_blocking.authority.trp`
            //
            //   TODO (2026-03-09; SS): Should we instead fail on a tainted authority, similar to
            //                          the `blockdecl` etc.? In this case, we  don't need to raise
            //                          the blocking label by `auth.lev`.
            //
            // - Depending on the given target level. For reference, see the following example of a
            //   leak to the adversary via the termination channel:
            //   `tests/rt/neg/ifc/declassify_blocking.to.trp`
            //
            // - Depending on the data level of the value being downgraded. Without this,
            //   the success/failure of the downgrade leaks information about the value's
            //   level. For reference, see:
            //   `tests/rt/neg/ifc/declassify_blocking.value_level.trp`
            let pc = runtime.$t.pc;

            const levFrom = typeOnly ? data.tlev : data.lev;
            runtime.$t.raiseBlockingThreadLev(lub (auth.lev, toLevV.lev, levFrom));

            let bl = runtime.$t.bl;
            let isNMIFC = runtime.$t.isNmifcMode;
            let lev_to = toLevV.val
            const downgradeKindString = stringOfDowngrader (dimension)

            const dg_f =
                dimension == DowngradeDimension.CONFIDENTIALITY ? okToDeclassify :
                dimension == DowngradeDimension.INTEGRITY ? okToEndorse :
                okToCrossDimensionalDowngrade;
            const ok_to_downgrade_result: DowngradeResult =
                dg_f(levFrom, lev_to, auth.val.authorityLevel, bl, isNMIFC, pc)

            if (ok_to_downgrade_result.kind === "SUCCESS") {
                const taintedLevTo = lub(lev_to, pc, arg.lev, auth.lev);
                const r = typeOnly
                    ? new LCopyVal(data, lub(data.lev, taintedLevTo), taintedLevTo)
                    : new LCopyVal(data, taintedLevTo, glb(data.tlev, taintedLevTo) );
                return runtime.ret(r)
            } else {
                let errorMessage = "";
                switch (ok_to_downgrade_result.reason) {
                    case DowngradeErrorReason.INTEGRITY_MISMATCH:
                        errorMessage = formatIntegrityMismatchMsg(downgradeKindString, levFrom, lev_to);
                        break;
                    case DowngradeErrorReason.CONFIDENTIALITY_MISMATCH:
                        errorMessage = formatConfidentialityMismatchMsg(downgradeKindString, levFrom, lev_to);
                        break;
                    case DowngradeErrorReason.BLOCKING_LEVEL_MISMATCH:
                        errorMessage = formatPiniBlockingLevelMismatchMsg(downgradeKindString, bl, lev_to);
                        break;
                    case DowngradeErrorReason.INSUFFICIENT_AUTHORITY:
                        errorMessage = formatValueInsufficientAuthorityMsg(downgradeKindString, levFrom, auth.val.authorityLevel, lev_to);
                        break;
                    case DowngradeErrorReason.ROBUSTNESS_VIOLATION:
                        errorMessage = formatRobustnessViolationMsg(downgradeKindString, levFrom, lev_to, pc, DowngradeKind.VALUE);
                        break;
                    case DowngradeErrorReason.TRANSPARENCY_VIOLATION:
                        errorMessage = formatTransparencyViolationMsg(downgradeKindString, levFrom, lev_to, pc, DowngradeKind.VALUE);
                        break;
                    default:
                        const _exhaustiveCheck: never = ok_to_downgrade_result.reason;
                        errorMessage = `Unhandled downgrade error reason: ${_exhaustiveCheck} for ${downgradeKindString}`;
                }
                runtime.$t.threadError(errorMessage, false, null, ErrorKind.IFCCheck);
            }
        })
}
