'use strict'
import * as nativeModules from '../registry.mjs'
import { ffiDemoExports } from '../ffidemo.mjs'

// Registers every native module the browser host provides. SimpleFiles and
// Tty are Node's (ffi/node/): a program requiring them links on this host
// as one requiring any absent native module does, with NativeUnavailableError.
export function registerBrowserNatives(): void {
    nativeModules.register('FFIDemo', ffiDemoExports)
}
