// The compiler and the runtime, as a page uses them.
//
// Both are workers under build/browser (make browser): the compiler worker
// runs troupec.wasm, the runtime worker runs a Troupe node; the page talks
// to them through build/browser/troupe-page.mjs, the runtime's own page-side
// module. What this page exercises is the component, not a copy of it.
import { fetchInterfaces, startCompiler, compilerPort, startNode, STAGES } from '/build/troupe-page.mjs';
export { STAGES };

let servicePromise = null;

/** Start the compiler worker once, with the compiler and the library interfaces. */
async function service() {
  servicePromise ??= (async () => {
    const interfaces = await fetchInterfaces('lib-exports.json', 'lib-exports/');
    return startCompiler('/build/compiler-worker.mjs', 'troupec.wasm', interfaces);
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

/**
 * Run a compiled program on a fresh node: a runtime worker that fetches the
 * libraries it links and relinks received closures through this page's
 * compiler. Input lines are fed up front. Resolves to the exit message once
 * the node ends; `handlers` receive the output as it comes.
 */
export async function runProgram(js, inputLines, handlers) {
  const node = startNode('/build/troupe-worker.mjs', {
    op: 'run',
    program: js,
    sources: { 'p.trp': handlers.source ?? '' },
    args: { localonly: true, 'no-color': true },
    code: { libraries: '/lib/out/', service: '/trp-rt/out/service.js' },
    compiler: await compilerPort(await service()),
    supervised: true,
  }, handlers);
  for (const line of inputLines) node.feedLine(line);
  return node;
}
