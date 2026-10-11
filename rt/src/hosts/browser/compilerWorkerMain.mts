/// <reference lib="webworker" />
// Entry of the compiler worker in a browser: `new Worker(url, {type: 'module'})`.

import { serveCompiler } from './compilerWorker.mjs';

serveCompiler(self as unknown as Parameters<typeof serveCompiler>[0]);
