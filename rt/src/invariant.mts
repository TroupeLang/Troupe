/** An invariant of the runtime's own code. Failing it is a runtime bug. */
export function assert(condition: unknown, message?: string): asserts condition {
    if (!condition) {
        throw new Error(message ?? 'runtime invariant violated');
    }
}
