import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import { assertIsProcessId } from '../Asserts.mjs'

/**
 * Returns a string corresponding to the node identify
 * from a process
 */

export function BuiltinNodeUtils<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        node = mkBase((arg) => {
            assertIsProcessId(arg);
            let data = arg.val;
            let nodeId = data.node.nodeId;
            if (nodeId == null) {
                nodeId = "<null>"
            }
            let v = new LVal(nodeId, arg.lev);
            return this.runtime.ret(v);
        }, "node")
    }
}