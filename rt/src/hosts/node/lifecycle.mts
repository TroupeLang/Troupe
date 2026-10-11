// Node's lifecycle: ending the node is ending the process, and Ctrl-C ends
// it through the runtime's cleanup with exit code 0.

import { Lifecycle } from '../../Lifecycle.mjs';
import { getResultChannel } from '../../ResultChannel.mjs';

export const nodeLifecycle: Lifecycle = {
    exit(code: number): void {
        process.exit(code);
    },
};

/**
 * Installs the SIGINT handler, replacing any listener another module
 * installed (readline adds its own), so that an interrupt always reaches
 * the runtime's cleanup. Called once the runtime is loaded.
 */
export function installInterruptHandler(cleanup: () => Promise<void>): void {
    process.removeAllListeners("SIGINT");
    process.on('SIGINT', () => {
        (async () => {
            await getResultChannel().sendAndClose({ type: 'process-exit', exitCode: 0 });
            await cleanup();
            process.exit(0);
        })();
    });
}
