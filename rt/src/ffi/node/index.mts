'use strict'
import * as nativeModules from '../registry.mjs'
import { ffiDemoExports } from './ffidemo.mjs'
import { simpleFilesExports } from './simplefiles.mjs'

// Registers every native module the node host provides. Called from the node
// entry (troupe.mts) before the first library link. Deliberately not an
// import-time side effect: each host assembles its own registrations, so
// importing this module must not decide what is registered.
export function registerNodeNatives(): void {
    nativeModules.register('FFIDemo', ffiDemoExports)
    nativeModules.register('SimpleFiles', simpleFilesExports)
}
