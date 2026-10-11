// Node's interrupt handling: Ctrl-C ends the node through the runtime's
// cleanup and exits 0.

import { getResultChannel } from '../../ResultChannel.mjs';

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
