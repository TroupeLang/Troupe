import {UserRuntimeZero, Constructor, mkBase} from './UserRuntimeZero.mjs'
import * as levels from '../Level.mjs'
import { assertNormalState, assertIsAuthority, assertIsCapability, assertIsNTuple, assertIsLevel } from '../Asserts.mjs'
import { ErrorKind } from '../TroupeError.mjs';
const flowsTo = levels.flowsTo;


export function BuiltinPini <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    
    
    return class extends Base {

        // pcpush = mkBase((arg) => {
        //     assertNormalState("pcpush")
        //     assertIsAuthority(arg);
        //     return this.runtime.$t.pcpinipush(arg, "pcpush");
        // })


        // pcpop = mkBase((arg) => {
        //     assertNormalState("pcpop");
        //     assertIsCapability(arg);
        //     return this.runtime.$t.pcpop(arg)
        // })


        pinipush = mkBase((arg) => {
            // assertNormalState("pinipush");
            assertIsAuthority(arg);
            return this.runtime.$t.pcpinipush(arg, "pinipush");
        })

        pinipushto = mkBase((arg) => {
            assertIsNTuple(arg, 2)
            assertIsAuthority(arg.val[0])
            assertIsLevel(arg.val[1])
            this.runtime.$t.raiseBlockingThreadLev(arg.val[1].lev);
            let is_lev_ok = flowsTo(this.runtime.$t.bl, arg.val[1].val);
            if (!is_lev_ok) {
                this.runtime.$t.threadError(`level argument of pinipushto operation must flow to the current blocking level\n` +
                    `| the blocking level: ${this.runtime.$t.bl.stringRep()}\n` +
                    `| the level argument: ${arg.val[1].stringRep()}`, false, null, ErrorKind.IFCCheck)
            } else {
                return this.runtime.$t.pcpinipush(arg.val[0], "pinipush", arg.val[1].val);
            }
        })

        pinipop = mkBase((arg) => {
            // assertNormalState("pinipop");
            assertIsCapability(arg)
            return this.runtime.$t.pinipop(arg);
        })

        blockdecl = mkBase((arg) => {
            assertIsAuthority(arg);
            return this.runtime.$t.blockDeclassifyTo(arg)
        })

        blockdeclto = mkBase((arg) => {
            assertIsNTuple(arg, 2);
            assertIsAuthority(arg.val[0])
            assertIsLevel(arg.val[1]);
            this.runtime.$t.raiseBlockingThreadLev (arg.val[1].lev);
            return this.runtime.$t.blockDeclassifyTo(arg.val[0], arg.val[1].val, arg.val[1].lev)
        })

        blockendorse = mkBase ((arg) => {
            assertIsAuthority(arg);
            return this.runtime.$t.blockEndorseTo(arg);
        })

        blockendorseto = mkBase((arg) => {
            assertIsNTuple(arg, 2);
            assertIsAuthority(arg.val[0])
            assertIsLevel(arg.val[1]);
            this.runtime.$t.raiseBlockingThreadLev (arg.val[1].lev);
            return this.runtime.$t.blockEndorseTo(arg.val[0], arg.val[1].val)
        })

        // Cross-dimensional blocking level downgrade to current PC
        blockdown = mkBase((arg) => {
            assertIsAuthority(arg);
            return this.runtime.$t.blockDowngradeTo(arg)
        })

        // Cross-dimensional blocking level downgrade to specified level
        blockdownto = mkBase((arg) => {
            assertIsNTuple(arg, 2);
            assertIsAuthority(arg.val[0])
            assertIsLevel(arg.val[1]);
            this.runtime.$t.raiseBlockingThreadLev (arg.val[1].lev);
            return this.runtime.$t.blockDowngradeTo(arg.val[0], arg.val[1].val)
        })

    }
}