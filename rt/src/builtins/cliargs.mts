'use strict'
import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import { assertIsAuthority, assertIsRootAuthority, assertNormalState } from '../Asserts.mjs'
import { mkList } from '../ValuesUtil.mjs'
import * as levels from '../Level.mjs'
import { getCliArgs } from '../TroupeCliArgs.mjs'

export function BuiltinCliArgs<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        getCliArgs = mkBase((arg) => {
            assertNormalState("getCliArgs")

            // Require authority argument and validate it's root authority
            assertIsAuthority(arg)
            assertIsRootAuthority(arg)

            const programArgs: string[] = getCliArgs().programArgs ?? [];

            // Convert to Troupe list of LVals
            // Label each argument at ROOT level (sensitive data)
            const lvalArgs = programArgs.map(s => new LVal(s, levels.ROOT));
            const result = mkList(lvalArgs);

            // Return the list labeled at ROOT level
            return this.runtime.ret(new LVal(result, levels.ROOT));
        }, "getCliArgs")
    }
}
