// The compiler and the runtime, as the page uses them.
//
// Both are workers from the site's own files (make web-demo): the compiler
// worker runs troupec.wasm, the runtime worker runs a Troupe node; the page
// talks to them through troupe-page.mjs, the runtime's own page-side
// module. Every URL is relative to the page, so the site serves from any
// path.
import { fetchInterfaces, startCompiler, compilerPort, startNode, STAGES } from './troupe-page.mjs';
export { STAGES };

const here = (path) => new URL(path, import.meta.url).href;

let servicePromise = null;

/** Start the compiler worker once, with the compiler and the library interfaces. */
async function service() {
  servicePromise ??= (async () => {
    const interfaces = await fetchInterfaces(here('lib-exports.json'), here('lib/out/'));
    return startCompiler(here('compiler-worker.mjs'), here('troupec.wasm'), interfaces);
  })();
  return servicePromise;
}

/** Compile Troupe source: the emitted JavaScript, with source positions for
 *  the error reports of a run, and every intermediate stage. */
export async function compileSource(source) {
  return (await service()).compileSource(source, { sourceMap: true });
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
 * compiler. The node is returned running: `handlers` receive its output as
 * it comes, `feedLine` delivers a line of input, and `exit` resolves to the
 * exit message. `options.stdiolev` is the level of the stdio channel. The node is not supervised, so its transcript is a terminal's:
 * the main thread's result is printed, and the logger colours its lines.
 */
export async function runProgram(js, options, handlers) {
  return startNode(here('troupe-worker.mjs'), {
    op: 'run',
    program: js,
    sources: { 'p.trp': handlers.source ?? '' },
    // Local-only, as every node here is; the runtime's line about the
    // network being skipped says nothing a visitor can act on. The terminal
    // is a channel at the level the page says (--stdiolev), public by
    // default: a secret cannot be printed on it without being declassified
    // first, which is what the samples on information flow show.
    args: { localonly: true, 'suppress-local-info-message': true, stdiolev: options.stdiolev ?? '{}' },
    code: { libraries: here('lib/out/'), service: here('trp-rt/out/service.js') },
    compiler: await compilerPort(await service()),
    supervised: false,
  }, handlers);
}

/** The sample programs: the manifest, then each file on request. */
export async function sampleList() {
  return (await fetch(here('samples/samples.json'))).json();
}

export async function sampleSource(file) {
  return (await fetch(here(`samples/${file}`))).text();
}
