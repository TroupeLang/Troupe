/// <reference lib="webworker" />
// Entry point of the runtime worker in a browser: serve the worker's own
// global scope, and end the node by closing the worker.
import { serveRuntime } from './runtimeWorker.mjs';

serveRuntime(self as unknown as Parameters<typeof serveRuntime>[0], () => self.close());
