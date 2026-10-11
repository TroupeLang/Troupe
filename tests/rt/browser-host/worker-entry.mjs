// The runtime worker under Node: the browser host served on a worker
// thread's port, ending the node by ending the thread.
import { parentPort } from 'node:worker_threads';
import { serveRuntime } from '../../../rt/built/hosts/browser/runtimeWorker.mjs';

serveRuntime(parentPort, (code) => process.exit(code));
