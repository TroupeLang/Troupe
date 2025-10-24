import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { assertIsUnit } from '../Asserts.mjs'
import { BOT } from '../Level.mjs';
import { LVal } from '../Lval.mjs';

export function BuiltinThread<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
  return class extends Base {
    _blockThread = mkBase ((arg) => {
      assertIsUnit(arg)
      this.runtime.__sched.blockThread(this.runtime.__sched.__currentThread);
      return null;
    })

    _pc = mkBase ((arg) => {
      assertIsUnit (arg)
      return this.runtime.ret (
        new LVal (this.runtime.$t.pc, this.runtime.$t.pc, BOT))
    });

    _bl = mkBase ((arg) => {
      assertIsUnit (arg)
      return this.runtime.ret (
        new LVal (this.runtime.$t.bl, this.runtime.$t.bl, BOT))
    });
  };
};
