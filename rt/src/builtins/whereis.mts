'use strict'
import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import * as levels from '../options.mjs'
import { ProcessID } from '../process.mjs';
const { lubs, flowsTo } = levels
import {deserialize} from '../deserialize.mjs'
import { __nodeManager } from '../NodeManager.mjs';
import { assertNormalState, assertIsNTuple, assertIsString, assertIsProcessId, assertIsAuthority, assertIsTopAuthority, assertIsNode } from '../Asserts.mjs';
import { __unit } from '../UnitVal.mjs';
import { nodeTrustLevel } from '../TrustManager.mjs';
export let __theRegister = {}
import {p2p} from '../p2p/p2p.mjs'

// import runId from '../runId.mjs';

import yargs from 'yargs';
let logLevel = yargs.argv.debug ? 'debug': 'info'
import { mkLogger } from '../logger.mjs'
const logger = mkLogger('RTM', logLevel);
const debug = x => logger.debug(x)


export function BuiltinRegistry<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        register = mkBase((arg) => {            
            let $r = this.runtime
            assertNormalState("register")
            assertIsNTuple(arg, 3);
            assertIsString(arg.val[0])
            assertIsProcessId(arg.val[1]);
            assertIsAuthority(arg.val[2]);
            assertIsTopAuthority(arg.val[2]);
            

            let ok_to_raise =
                flowsTo($r.$t.bl, levels.BOT);
            if (!ok_to_raise) {
                $r.$t.threadError("Cannot raise trust level when the process is tainted\s" +
                    ` | blocking label: ${$r.$t.bl.stringRep()}`)
            }


            // TODO: 2018-07-29: info flow checks
            // this is needed, because registration
            // is stateful

            let k = arg.val[0].val;
            let v = arg.val[1];

            __theRegister[k] = v;
            return $r.ret(__unit);
        }, "register")



        whereis = mkBase((arg) => {            
            let $r = this.runtime
            assertNormalState("whereis")
            assertIsNTuple(arg, 2);
            assertIsNode(arg.val[0]);
            assertIsString(arg.val[1]);
            $r.$t.raiseBlockingThreadLev(arg.val[0].lev);
            $r.$t.raiseBlockingThreadLev(arg.val[1].lev);

            let __sched = $r.__sched

            // let n = dealias(arg.val[0].val);    
            let n = __nodeManager.getNode(arg.val[0].val).nodeId;
            
            let k = arg.val[1].val;
            let nodeLev = nodeTrustLevel(n);
            let theThread = $r.$t;

            
            let okToLookup = flowsTo(lubs([$r.$t.pc, arg.val[0].lev, arg.val[1].lev]), nodeLev);
            if (!okToLookup) {
                $r.$t.threadError("Information flow violation in whereis");
                return;
            }

            if (__nodeManager.isLocalNode(n)) {
                if (__theRegister[k]) {            
                    return $r.ret(__theRegister[k]) 
                }
            } else {
                (async () => {
                    try {
                        let body1 = await p2p.whereisp2p(n, k);
                        let body = await deserialize(nodeTrustLevel(n), body1);
                        let pid = new ProcessID(body.val.uuid, body.val.pid, body.val.node);

                        theThread.returnSuspended(theThread.mkValWithLev(pid, body.lev));
                        __sched.scheduleThread(theThread);
                        __sched.resumeLoopAsync();

                    } catch (err) {
                        $r.debug("whereis error: " + err.toString())
                        throw err;
                    }

                })()
            }
        }, "whereis")

    }
}