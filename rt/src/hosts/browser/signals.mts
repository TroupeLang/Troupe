// The browser's signals: there are none. A trap is accepted and never
// fires, since nothing delivers a termination signal to a worker; the
// page's way of stopping a node is to terminate the worker.

import { Signals } from '../../Signals.mjs';

export const browserSignals: Signals = {
    trapTerminate(_handler: () => void): void {},
    untrapTerminate(_handler: () => void): void {},
    terminateByDefault(): void {},
};
