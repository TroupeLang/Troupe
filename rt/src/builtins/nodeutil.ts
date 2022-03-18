import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero'
import { LVal } from '../Lval';
import options from '../options'
import { assertIsProcessId } from '../Asserts';
const levels = options;

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