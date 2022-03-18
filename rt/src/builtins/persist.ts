import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero'
import { LVal } from '../Lval';
import options from '../options'
const levels = options;
import {deserialize} from '../deserialize'
import fs from 'fs';
import { assertIsNTuple, assertIsString } from '../Asserts';
import { __unit } from '../UnitVal';

export function BuiltinPersist<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        save = mkBase((larg) => {
            assertIsNTuple(larg, 2);
            this.runtime.$t.raiseCurrentThreadPC(larg.lev);
            let arg = larg.val;
            let file = arg[0].val;
            let data = arg[1];
            this.runtime.persist(data, "./out/saved." + file + ".json")
            return this.runtime.ret(__unit);
        }, "save")


        restore = mkBase((arg) => {
            assertIsString(arg)
            let theThread = this.runtime.$t;
            let file = arg;

            (async () => {
                let jsonStr = await fs.promises.readFile("./out/saved." + file.val + ".json", 'utf8');
                let data = await deserialize(levels.TOP, JSON.parse(jsonStr));                
                theThread.returnSuspended(data);
                this.runtime.__sched.scheduleThread(theThread);
                this.runtime.__sched.resumeLoopAsync();

            })()
        }, "restore")

    }

}