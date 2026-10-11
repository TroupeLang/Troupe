// Node's clock: the process's high-resolution monotonic time.

import { Clock } from '../../Clock.mjs';

export const nodeClock: Clock = {
    nanoTime(): bigint {
        return process.hrtime.bigint();
    },
};
