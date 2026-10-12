'use strict'
import * as nativeModules from '../registry.mjs'
import { ffiDemoExports } from '../ffidemo.mjs'
import { simpleFilesExports } from './simplefiles.mjs'
import { ttyExports, ttyRestore } from './tty.mjs'
import { testQuestionsExports } from './testquestions.mjs'
import { p2pRelayControlExports } from './p2prelaycontrol.mjs'
import { p2pRelayClientExports } from './p2prelayclient.mjs'

// Registers every native module the node host provides. Called from the node
// entry (troupe.mts) before the first library link. Deliberately not an
// import-time side effect: each host assembles its own registrations, so
// importing this module must not decide what is registered.
export function registerNodeNatives(): void {
    nativeModules.register('FFIDemo', ffiDemoExports)
    nativeModules.register('SimpleFiles', simpleFilesExports)
    nativeModules.register('Tty', ttyExports)
    nativeModules.register('TestQuestions', testQuestionsExports)
    nativeModules.register('P2PRelayControl', p2pRelayControlExports)
    nativeModules.register('P2PRelayClient', p2pRelayClientExports)
    // The terminal restore runs for every program on every termination route
    // that reaches cleanupAsync, registered here so the core cleanup path
    // never imports from ffi/node/.
    nativeModules.registerCleanup(ttyRestore)
}
