// Node's termination signal: SIGTERM on the process.

import { Signals } from '../../Signals.mjs';

export const nodeSignals: Signals = {
    trapTerminate(handler: () => void): void {
        process.on('SIGTERM', handler);
    },

    untrapTerminate(handler: () => void): void {
        process.removeListener('SIGTERM', handler);
    },

    // Node stops listening for a signal once its last listener is removed,
    // so re-raising it ends the process as an untrapped signal does.
    terminateByDefault(): void {
        process.kill(process.pid, 'SIGTERM');
    },
};
