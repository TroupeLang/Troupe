import {UserRuntimeZero, Constructor, mkBase} from './UserRuntimeZero.mjs'
import { assertIsAuthority, assertIsNTuple, assertIsLevel } from '../Asserts.mjs'


export function BuiltinPini <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    
    
    return class extends Base {

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