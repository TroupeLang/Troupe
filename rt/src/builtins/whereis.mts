'use strict'
import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import * as levels from '../Level.mjs'
import { LVal } from '../Lval.mjs'
import { ProcessID } from '../process.mjs';
const { lub, flowsTo } = levels
import {deserialize, DeserializeResult} from '../deserialize.mjs'
import { shouldDrop } from '../QuarantineUtils.mjs'
import { __nodeManager } from '../NodeManager.mjs';
import { assertNormalState, assertIsNTuple, assertIsString, assertIsProcessId, assertIsAuthority, assertIsRootAuthority, assertIsNode } from '../Asserts.mjs';
import { __unit } from '../UnitVal.mjs';
import { nodeTrustLevel } from '../TrustManager.mjs';
import { ErrorKind } from '../TroupeError.mjs';
export let __theRegister = {}
import { getNetworkProvider } from '../NetworkProvider.mjs'

// import runId from '../runId.mjs';

import { getCliArgs, TroupeCliArg } from '../TroupeCliArgs.mjs';
const argv = getCliArgs();

let logLevel = argv[TroupeCliArg.Debug] ? 'debug': 'info'
import { mkLogger } from '../logger.mjs'
const logger = mkLogger('RTM', logLevel);
const debug = x => logger.debug(x)


// Type for result transformation functions
type LocalResultTransformer = (pidLVal: LVal) => LVal;
type RemoteResultTransformer = (pid: ProcessID, bodyLev: levels.Level, result: DeserializeResult) => LVal;

export function BuiltinRegistry<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        register = mkBase((arg) => {
            let $r = this.runtime
            assertNormalState("register")
            assertIsNTuple(arg, 3);
            assertIsString(arg.val[0])
            assertIsProcessId(arg.val[1]);
            assertIsAuthority(arg.val[2]);
            assertIsRootAuthority(arg.val[2]);


            let ok_to_raise =
                flowsTo($r.$t.bl, levels.BOT);
            if (!ok_to_raise) {
                $r.$t.threadError("Cannot raise trust level when the process is tainted\s" +
                    ` | blocking label: ${$r.$t.bl.stringRep()}`, false, null, ErrorKind.IFCCheck)
            }


            // TODO: 2018-07-29: info flow checks
            // this is needed, because registration
            // is stateful

            let k = arg.val[0].val;
            let v = arg.val[1];

            __theRegister[k] = v;
            return $r.ret(__unit);
        }, "register")


        // Common implementation for whereis and qwhereis
        private whereisImpl(
            arg: any,
            fnName: string,
            localTransform: LocalResultTransformer,
            remoteTransform: RemoteResultTransformer
        ) {
            let $r = this.runtime
            assertNormalState(fnName)
            assertIsNTuple(arg, 2);
            assertIsNode(arg.val[0]);
            assertIsString(arg.val[1]);
            $r.$t.raiseBlockingThreadLev(arg.val[0].lev);
            $r.$t.raiseBlockingThreadLev(arg.val[1].lev);

            let __sched = $r.__sched

            let n = __nodeManager.getNode(arg.val[0].val).nodeId;

            let k = arg.val[1].val;
            let nodeLev = nodeTrustLevel(n);
            let theThread = $r.$t;


            let okToLookup = flowsTo(lub($r.$t.pc, arg.val[0].lev, arg.val[1].lev), nodeLev);
            if (!okToLookup) {
                $r.$t.threadError(`Information flow violation in ${fnName}`, false, null, ErrorKind.IFCCheck);
                return;
            }

            if (__nodeManager.isLocalNode(n)) {
                if (__theRegister[k]) {
                    let pidLVal = __theRegister[k];
                    return $r.ret(localTransform(pidLVal));
                }
            } else {
                (async () => {
                    try {
                        let body1 = await getNetworkProvider().whereisp2p(n, k);
                        let result = await deserialize(nodeTrustLevel(n), body1, n);

                        // DROP means we can't trust the pid we got back
                        if (shouldDrop(result)) {
                            debug(`Dropping corrupt ${fnName} response from ${n}`);
                            theThread.throwInSuspended("Corrupt whereis response from remote node");
                            __sched.scheduleThread(theThread);
                            __sched.resumeLoopAsync();
                            return;
                        }

                        let body = result.value!;
                        let pid = new ProcessID(body.val.uuid, body.val.pid, body.val.node);
                        let resultLVal = remoteTransform(pid, body.lev, result);

                        theThread.returnSuspended(resultLVal);
                        __sched.scheduleThread(theThread);
                        __sched.resumeLoopAsync();

                    } catch (err) {
                        $r.debug(`${fnName} error: ` + err.toString())
                        throw err;
                    }

                })()
            }
        }


        whereis = mkBase((arg) => {
            return this.whereisImpl(
                arg,
                "whereis",
                // Local: return the pid LVal directly
                (pidLVal) => pidLVal,
                // Remote: return just the pid with its level
                (pid, bodyLev, _result) => this.runtime.$t.mkValWithLev(pid, bodyLev)
            );
        }, "whereis")

    }
}
