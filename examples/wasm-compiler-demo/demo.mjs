// The Troupe compiler, running in the page.
//
// bin/troupec.wasm is an ordinary WASI command module: its only imports are
// wasi_snapshot_preview1 and it exports _start. So there is no glue layer and
// no Haskell-side shim -- the page gives it a filesystem made of JavaScript
// objects, runs it, and reads the files it wrote back out of memory.
//
// That is also how the intermediate stages are obtained. `troupec -v` writes
// out/out.syntax, out/out.cps and the rest as it goes; natively those land on
// disk, and here they land in a Map this page owns. No compiler flag was added
// to serve this demo, and none was needed.
import {
  WASI, File, Directory, OpenFile, PreopenDirectory,
} from './vendor/wasi/index.js';

// The pipeline, in the order the compiler runs it. The file names are the ones
// `-v` writes; the labels are for the tab strip.
export const STAGES = [
  ['out.syntax', 'parsed'],
  ['out.opreassoc', 'operators resolved'],
  ['out.nopats', 'patterns eliminated'],
  ['out.lowered', 'lowered'],
  ['out.alpha', 'alpha renamed'],
  ['out.cps', 'CPS'],
  ['out.cpsopt', 'CPS optimized'],
  ['out.ir', 'IR'],
  ['out.iropt', 'IR optimized'],
  ['out.rawout', 'raw'],
  ['out.rawopt', 'raw optimized'],
  ['out.stack', 'stack'],
];

const encoder = new TextEncoder();
const decoder = new TextDecoder();

let modulePromise = null;
let libExports = null; // [name, Uint8Array][]

/** Compile the module once and reuse it; instantiation is per run. */
function compiler() {
  modulePromise ??= WebAssembly.compileStreaming(fetch('troupec.wasm'));
  return modulePromise;
}

/** The stdlib interface files, mounted so `import <Library>` resolves. */
async function libraryInterfaces() {
  if (libExports) return libExports;
  const names = await (await fetch('lib-exports.json')).json();
  libExports = await Promise.all(names.map(async (n) => {
    const r = await fetch(`lib-exports/${n}`);
    return [n, new Uint8Array(await r.arrayBuffer())];
  }));
  return libExports;
}

/** Recursively collect every regular file under a directory, as text. */
function filesUnder(dir, prefix = '') {
  const out = new Map();
  for (const [name, inode] of dir.contents) {
    const path = prefix ? `${prefix}/${name}` : name;
    if (inode instanceof Directory) {
      for (const [k, v] of filesUnder(inode, path)) out.set(k, v);
    } else if (inode.data) {
      out.set(path, decoder.decode(inode.data));
    }
  }
  return out;
}

/**
 * Run the compiler once over a filesystem built for this request.
 *
 * `files` are placed at the root; `out/` is created because the compiler
 * writes its stage dumps there and does not create the directory itself --
 * natively that is the same requirement, and its absence is a write failure
 * rather than a diagnostic.
 */
async function run(args, { files = [], stdin = '' } = {}) {
  const root = new Map();
  for (const [name, text] of files) {
    root.set(name, new File(typeof text === 'string' ? encoder.encode(text) : text));
  }
  const outDir = new Directory(new Map());
  root.set('out', outDir);

  const interfaces = await libraryInterfaces();
  root.set('troupe', new Directory(new Map([
    ['lib', new Directory(new Map([
      ['out', new Directory(new Map(interfaces.map(([n, d]) => [n, new File(d)])))],
    ]))],
  ])));

  const stdoutFile = new File([]);
  const stderrFile = new File([]);
  const fds = [
    new OpenFile(new File(encoder.encode(stdin))),
    new OpenFile(stdoutFile),
    new OpenFile(stderrFile),
    new PreopenDirectory('/', root),
  ];

  // TROUPE is how the compiler finds the library interfaces: getExecutablePath
  // cannot locate an install root here, so getTroupeHome falls through to the
  // environment variable, which is its documented fallback. PWD mirrors what
  // the toolchain's own runner sets, so relative paths resolve.
  // debug:false — the shim traces every syscall to the console otherwise, and
  // a single compile makes hundreds of them.
  const wasi = new WASI(['troupec', ...args], ['TROUPE=/troupe', 'PWD=/'], fds, { debug: false });
  const instance = await WebAssembly.instantiate(await compiler(), {
    wasi_snapshot_preview1: wasi.wasiImport,
  });

  const started = performance.now();
  let code;
  try {
    code = wasi.start(instance);
  } catch (e) {
    return { code: -1, crash: String(e), stdout: '', stderr: '', files: new Map(), ms: 0 };
  }
  const ms = performance.now() - started;

  return {
    code,
    stdout: decoder.decode(stdoutFile.data),
    stderr: decoder.decode(stderrFile.data),
    files: filesUnder(new Directory(root)),
    ms,
  };
}

/**
 * Compile Troupe source, returning the emitted JavaScript and every
 * intermediate stage.
 */
export async function compileSource(source) {
  const r = await run(['-v', 'p.trp', '-o', 'p.js'], { files: [['p.trp', source]] });
  const stages = new Map();
  for (const [file, label] of STAGES) {
    const text = r.files.get(`out/${file}`);
    if (text !== undefined) stages.set(label, text);
  }
  return {
    ok: r.code === 0,
    code: r.code,
    crash: r.crash,
    js: r.files.get('p.js') ?? '',
    exports: r.files.get('p.exports') ?? '',
    stages,
    log: r.stdout,
    errors: r.stderr,
    ms: r.ms,
  };
}

/**
 * Relink a serialized IR blob, the way a node does when a closure arrives.
 *
 * This is the --json-ir path from rt/src/deserialize.mts: each blob is one
 * base64 line, and the runtime brackets a batch with an "!ECHO <marker>" line
 * that the compiler echoes back. Each response is a JSON {code, sourceMap?}.
 */
export async function linkBlobs(blobs) {
  const marker = '/*-----*/';
  const stdin = `${blobs.map((b) => b.trim()).join('\n')}\n!ECHO ${marker}\n`;
  const r = await run(['--json-ir'], { stdin });

  // Responses are separated by blank lines; the echoed marker ends the batch.
  const parts = r.stdout.split('\n\n').map((s) => s.trim()).filter(Boolean);
  const snippets = [];
  for (const part of parts) {
    if (part === marker) break;
    try {
      snippets.push(JSON.parse(part));
    } catch {
      snippets.push({ code: part, malformed: true });
    }
  }
  return { ok: r.code === 0, code: r.code, crash: r.crash, snippets, errors: r.stderr, ms: r.ms };
}
