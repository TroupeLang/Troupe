// The browser's lifecycle: ending the node is closing the worker.

import { Lifecycle } from '../../Lifecycle.mjs';

export function browserLifecycle(close: (code: number) => void): Lifecycle {
    return { exit: close };
}
