'use strict'

// Registry of native modules: named groups of host-implemented primitives. A
// module's export table has the same shape as a loaded library's values table
// (loadLibsAsync.mts), so the linker serves a registered table as the exports
// of an already-loaded library. Availability is exactly what the host
// registered; module names are stored without the "native:" link-record
// prefix, which belongs to the linker.
type NativeExports = Record<string, any>

// Raised when a link record names a native module the host has not
// registered. Classified as an expected inbound error (deserialize.mts), so
// an inbound closure requiring an absent native module is dropped or
// rejected, never a crash.
export class NativeUnavailableError extends Error {
    constructor(name: string) {
        super(`native module '${name}' is not available on this runtime`)
    }
}

const __registry: Map<string, NativeExports> = new Map()

export function register(name: string, exports: NativeExports): void {
    if (__registry.has(name)) {
        throw new Error(`native module '${name}' is already registered`)
    }
    __registry.set(name, exports)
}

export function has(name: string): boolean {
    return __registry.has(name)
}

export function resolve(name: string): NativeExports {
    const table = __registry.get(name)
    if (table === undefined) {
        throw new NativeUnavailableError(name)
    }
    return table
}

// Shutdown restores. A native module that holds host state a program's death
// must not leave behind (the terminal's raw mode, an armed subscription)
// registers a restore function alongside its export table; the runtime's
// cleanup path (cleanupAsync, runtimeMonitored.mts) runs every registered
// restore, in registration order, without importing any host-specific module.
// A restore runs outside any thread, must be safe when nothing was set up,
// and must be safe to run twice — cleanup runs on every termination route
// that reaches it.
const __cleanups: Array<() => void> = []

export function registerCleanup(fn: () => void): void {
    __cleanups.push(fn)
}

export function runCleanups(): void {
    for (const fn of __cleanups) {
        fn()
    }
}
