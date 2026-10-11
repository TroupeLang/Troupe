// The high-resolution clock behind getNanoTime. Wall-clock time (getTime)
// is Date, which every host has; the monotonic nanosecond count is the
// host's, and its resolution is whatever the host offers.

export interface Clock {
    /** Nanoseconds since an arbitrary origin, monotonic within a run. */
    nanoTime(): bigint;
}

let __clock: Clock | null = null;

export function registerClock(c: Clock): void {
    __clock = c;
}

export function getClock(): Clock {
    if (__clock == null) {
        throw new Error('no clock registered on this host');
    }
    return __clock;
}
