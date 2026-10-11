// The browser's clock: performance.now(), milliseconds with a fraction,
// scaled to nanoseconds. Its resolution is what the browser grants -- tens
// of microseconds, coarser without cross-origin isolation -- so successive
// readings may repeat where Node's never do. The count starts at the
// worker's time origin.

import { Clock } from '../../Clock.mjs';

export const browserClock: Clock = {
    nanoTime(): bigint {
        return BigInt(Math.round(performance.now() * 1e6));
    },
};
