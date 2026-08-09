'use strict'
import { mkBase } from '../../builtins/UserRuntimeZero.mjs'
import { LVal } from '../../Lval.mjs'
import { assertIsString, assertIsNTuple, assertIsNumber } from '../../Asserts.mjs'
import { lub } from '../../Level.mjs'
import { getRuntimeObject } from '../../SysState.mjs'

// FFIDemo: the demonstration native module (manifest ffi/FFIDemo.exports).
// Deliberately platform-neutral — no node APIs — so every host can register
// it and it never becomes an availability hazard. Label discipline follows
// the pure string builtins (builtins/string.mts): the result carries the
// join of the arguments' labels.
export const ffiDemoExports = {
    ffiDemoGreet: mkBase(arg => {
        assertIsString(arg)
        let s: string = arg.val
        return getRuntimeObject().ret(new LVal("Hello, " + s + "!", arg.lev))
    }, "ffiDemoGreet"),

    ffiDemoAdd: mkBase(arg => {
        assertIsNTuple(arg, 2)
        assertIsNumber(arg.val[0])
        assertIsNumber(arg.val[1])
        return getRuntimeObject().ret(
            new LVal( arg.val[0].val + arg.val[1].val
                    , lub ( arg.lev
                          , arg.val[0].lev
                          , arg.val[1].lev
                          )))
    }, "ffiDemoAdd"),
}
