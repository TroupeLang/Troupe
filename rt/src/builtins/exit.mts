import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { assertNormalState, assertIsNTuple, assertIsAuthority, assertIsNumber, assertIsRootAuthority } from '../Asserts.mjs'
import { __unit } from '../UnitVal.mjs';
import { setExitInitiated } from '../runtimeMonitored.mjs';
import { getResultChannel } from '../ResultChannel.mjs';
import { getLifecycle } from '../Lifecycle.mjs';


export function BuiltinExit <TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        exit = mkBase((arg) => {
            let $r = this.runtime
            assertNormalState("exit");
            assertIsNTuple(arg, 2);
            assertIsAuthority(arg.val[0]);
            assertIsNumber(arg.val[1]);
            assertIsRootAuthority(arg.val[0]);
            setExitInitiated();  // Prevent compiler exit handler from interfering
            (async () => {
                await getResultChannel().sendAndClose({ type: 'process-exit', exitCode: arg.val[1].val });
                await $r.cleanup()
                getLifecycle().exit(arg.val[1].val);
            }) ()

        }, "exit")

        _resetScheduler = mkBase((arg) => {
            assertNormalState("exit");
            assertIsAuthority(arg);
            assertIsRootAuthority(arg);
            this.runtime.__sched.resetScheduler ()
            return this.runtime.ret(__unit)
            
        })
    }
}