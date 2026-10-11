// Node's console for the runtime's reports: the process's output streams,
// coloured as the colour configuration says.

import { Console } from 'node:console';
import { isColorEnabled } from '../../colorConfig.mjs';
import { HostConsole } from '../../hostConsole.mjs';

export function nodeConsole(): HostConsole {
    return new Console({
        stdout: process.stdout,
        stderr: process.stderr,
        colorMode: isColorEnabled(),
    });
}
