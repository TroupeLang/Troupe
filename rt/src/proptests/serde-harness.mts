/**
 * Runtime bootstrap and round-trip helper for the serialization / equality
 * property tests that need to call the real `deserialize`.
 *
 * `deserialize` is async and depends on runtime module state: `runtimeMonitored`
 * constructs the runtime object and registers it with the deserializer
 * (`DS.setRuntimeObj`), and loading `deserialize` spawns the `troupec --json-ir`
 * subprocess used to reconstruct dynamic code. Neither a live scheduler nor a
 * thread is required to round-trip pure (non-closure) values: with an empty
 * namespace set the deserializer shortcuts the compiler interaction entirely.
 *
 * Two setup constraints are load-bearing:
 *
 *  1. Import order. `deserialize` and `runtimeMonitored` form an import cycle
 *     (`deserialize` needs `__exitInitiated`; `runtimeMonitored` calls
 *     `DS.setRuntimeObj`). `runtimeMonitored` must be evaluated first — exactly
 *     as the real entry point does — or `DS.setRuntimeObj` runs against an
 *     uninitialized `__rtObj` (TDZ). Hence its import precedes `deserialize`.
 *
 *  2. Teardown. The compiler subprocess and the stdin readline interface keep
 *     the event loop alive, so `node --test` would hang after the suite. The
 *     registered `after` hook shuts both down and unrefs any straggler handle,
 *     WITHOUT calling `process.exit` (which races the runner and can drop a
 *     still-running test, masking a failure). The process then exits naturally
 *     with the runner's own exit code.
 */
import { after } from 'node:test'
// (1) runtimeMonitored MUST load before deserialize — see the note above.
import { setExitInitiated } from '../runtimeMonitored.mjs'
import * as DS from '../deserialize.mjs'
import { IngressResult } from '../deserialize.mjs'
import { serialize } from '../serialize.mjs'
import { closeReadline } from '../builtins/stdio.mjs'
import * as levels from '../Level.mjs'
import { LVal } from '../Lval.mjs'
import assert from 'node:assert'

/**
 * Serialize a value, push it through the real JSON wire encoding, and deserialize
 * it back. The PC level is BOT so the top-level label is preserved (serialize
 * overwrites the root label with lub(value.lev, pc)); the trust level is ROOT so
 * every non-corrupt label classifies as TRUSTED and is restored unchanged.
 * Asserts the ingress result is TRUSTED — a QUARANTINE or DROP here would itself
 * be a finding for the value space these tests generate.
 */
export async function roundTrip(v: LVal): Promise<LVal> {
    const { data } = serialize(v, levels.BOT)
    const wire = JSON.parse(JSON.stringify(data))
    const result = await DS.deserialize(levels.ROOT, wire)
    assert.strictEqual(
        result.result, IngressResult.TRUSTED,
        `expected TRUSTED ingress, got result kind ${result.result}`,
    )
    return result.value!
}

after(() => {
    setExitInitiated()
    try { closeReadline() } catch { /* readline may already be closed */ }
    DS.stopCompiler()
    const getHandles = (process as any)._getActiveHandles
    if (typeof getHandles === 'function') {
        for (const h of getHandles.call(process)) {
            try { h.unref && h.unref() } catch { /* best effort */ }
        }
    }
})
