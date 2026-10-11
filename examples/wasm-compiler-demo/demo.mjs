// The compiler worker, as a page uses it.
//
// The worker is build/browser/compiler-worker.mjs (make compiler-worker): the
// runtime's rt/src/hosts/browser/compilerWorkerMain.mts with the WASI shim
// resolved. The page talks to it through the runtime's own compiler service,
// the object a browser Troupe node registers to relink the closures it
// receives, so what this page exercises is the component, not a copy of it.
import { browserCompilerService } from '/rt/hosts/browser/compilerService.mjs';
export { STAGES } from '/rt/hosts/browser/stages.mjs';

let servicePromise = null;

/** Start the worker once and hand it the compiler and the library interfaces. */
async function service() {
  servicePromise ??= (async () => {
    const worker = new Worker('/build/compiler-worker.mjs', { type: 'module' });
    const svc = browserCompilerService(worker);
    const [wasm, names] = await Promise.all([
      fetch('troupec.wasm').then((r) => r.arrayBuffer()),
      fetch('lib-exports.json').then((r) => r.json()),
    ]);
    const interfaces = await Promise.all(names.map(async (n) => {
      const r = await fetch(`lib-exports/${n}`);
      return [n, new Uint8Array(await r.arrayBuffer())];
    }));
    await svc.init(wasm, interfaces);
    return svc;
  })();
  return servicePromise;
}

/** Compile Troupe source: the emitted JavaScript and every intermediate stage. */
export async function compileSource(source) {
  return (await service()).compileSource(source);
}

/**
 * Relink serialized IR blobs the way a node does when a closure arrives.
 * Returns the snippets, one per blob; rejects when the compiler refuses the batch.
 */
export async function linkBlobs(blobs) {
  const started = performance.now();
  const snippets = await (await service()).linkIR(blobs);
  return { ok: true, snippets, errors: '', ms: performance.now() - started };
}
