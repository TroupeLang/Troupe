import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import * as levels from '../Level.mjs'
import {deserialize} from '../deserialize.mjs'
import { shouldDrop } from '../QuarantineUtils.mjs'
import * as fs from 'node:fs';
import { assertIsNTuple, assertIsRootAuthority, assertIsString } from '../Asserts.mjs'
import { __unit } from '../UnitVal.mjs';
import { StopThreadError, ErrorKind } from '../TroupeError.mjs';
import { getRuntimeObject } from '../SysState.mjs';
import * as Ty from '../TroupeTypes.mjs';

/**
 * Error thrown when attempting to persist data containing quarantined labels.
 * Quarantined data is ephemeral and tied to its source session; persisting it
 * would allow the quarantine to escape its intended scope.
 */
class QuarantinedDataPersistenceError extends StopThreadError {
    explainstr: string = "Quarantined data is tagged with its source node and session. " +
                         "It cannot be persisted because: (1) the quarantine session is ephemeral, " +
                         "(2) restored data would have stale/invalid quarantine tags, and " +
                         "(3) this could allow quarantined information to escape its intended scope.";
    errorKind: ErrorKind = ErrorKind.IFCCheck;

    get errorMessage() {
        return "Cannot persist data containing quarantined labels. " +
               "Quarantined data must be processed or downgraded before persistence.";
    }

    constructor() {
        super(getRuntimeObject().$t);
    }
}

/**
 * Check if an LVal contains any quarantined labels (in its value, level, or thread level).
 * This is a recursive check that examines nested structures.
 */
function containsQuarantinedLabels(lval: LVal, seen: Set<any> = new Set()): boolean {
    // Check for circular references
    if (seen.has(lval)) return false;
    seen.add(lval);

    // Check the levels of this LVal
    if (lval.lev.hasQuarantinedLabels && lval.lev.hasQuarantinedLabels()) return true;
    if (lval.tlev.hasQuarantinedLabels && lval.tlev.hasQuarantinedLabels()) return true;

    // Check the value based on its type
    const x = lval.val;
    const _tt = lval.troupeType;

    switch (_tt) {
        case Ty.TroupeType.RECORD:
            for (const [, v] of x.__obj.entries()) {
                if (containsQuarantinedLabels(v, seen)) return true;
            }
            break;
        case Ty.TroupeType.LIST:
            for (const item of x.toArray()) {
                if (containsQuarantinedLabels(item, seen)) return true;
            }
            break;
        case Ty.TroupeType.TUPLE:
            for (let i = 0; i < x.length; i++) {
                if (containsQuarantinedLabels(x[i], seen)) return true;
            }
            break;
        case Ty.TroupeType.CLOSURE:
            if (x.env) {
                for (const field in x.env) {
                    if (field !== "ret" && field !== "_is_rt_env" && field !== "__dataLevel") {
                        const y = x.env[field];
                        if (Ty.isLVal(y) && containsQuarantinedLabels(y, seen)) return true;
                    }
                }
            }
            break;
        case Ty.TroupeType.LEVEL:
            if (x.hasQuarantinedLabels && x.hasQuarantinedLabels()) return true;
            break;
        case Ty.TroupeType.LVAL:
            if (containsQuarantinedLabels(x, seen)) return true;
            break;
        case Ty.TroupeType.AUTHORITY:
            if (x.authorityLevel?.hasQuarantinedLabels && x.authorityLevel.hasQuarantinedLabels()) return true;
            break;
    }

    return false;
}

export function BuiltinPersist<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        save = mkBase((larg) => {
            assertIsNTuple(larg, 3);
            this.runtime.$t.raiseCurrentThreadPC(larg.lev);
            let arg = larg.val;
            let auth = arg[0];
            let file = arg[1].val;
            let data = arg[2];
            assertIsRootAuthority(auth);

            // Check for quarantined labels before persisting
            if (containsQuarantinedLabels(data)) {
                throw new QuarantinedDataPersistenceError();
            }

            this.runtime.persist(data, "./out/saved." + file + ".persist.json")
            return this.runtime.ret(__unit);
        }, "save")


        restore = mkBase((arg) => {
            assertIsString(arg)
            let theThread = this.runtime.$t;
            let file = arg;

            (async () => {
                try {
                    let jsonStr = await fs.promises.readFile("./out/saved." + file.val + ".persist.json", 'utf8');
                    // Use ROOT (most trusted) for local deserialization - we trust our own persisted data
                    let result = await deserialize(levels.ROOT, JSON.parse(jsonStr));

                    // For restore, DROP means the persisted data was corrupted
                    if (shouldDrop(result)) {
                        theThread.throwInSuspended("Corrupt data in persisted file");
                        this.runtime.__sched.scheduleThread(theThread);
                        this.runtime.__sched.resumeLoopAsync();
                        return;
                    }

                    // For local restore, we trust TOP so QUARANTINE should not happen
                    // but if it does, we still use the value
                    let data = result.value!;
                    theThread.returnSuspended(data);
                    this.runtime.__sched.scheduleThread(theThread);
                    this.runtime.__sched.resumeLoopAsync();
                } catch (err) {
                    // Failure to read or deserialize the file becomes a
                    // thread-level error handled by the scheduler, not a
                    // node crash.
                    const m = err instanceof Error ? err.message : String(err);
                    theThread.throwInSuspended("Error restoring value: " + m);
                    this.runtime.__sched.scheduleThread(theThread);
                    this.runtime.__sched.resumeLoopAsync();
                }
            })()
        }, "restore")

    }

}