// How much slower is compiling a Troupe program under WebAssembly than
// natively, and how much slower again in a browser than under Node?
//
//   node scripts/wasm-compile-bench.mjs [--reps N] [--out FILE] [--no-browser]
//
// Four configurations compile the same programs:
//
//   native     bin/troupec                     the baseline
//   wasmtime   bin/troupec.wasm, wasmtime      wasm with a fast host runtime, no JS engine
//   node       bin/troupec.wasm, node:wasi     the JS-engine wasm path, no browser
//   chrome     bin/troupec.wasm, in a page     what the demo actually pays
//
// Separating wasmtime from node separates "wasm is slower than native" from
// "this JS engine's wasm is slower than wasmtime". A single browser-vs-native
// number would conflate them.
//
// TWO KINDS OF NUMBER, AND THEY ARE NOT INTERCHANGEABLE:
//
//   cold  native, wasmtime -- a fresh process per compile. Includes process
//         start and, for wasm, compiling the module. This is what a CLI pays.
//   warm  node, chrome -- one already-compiled module, instantiated per
//         compile in a live process. This is what a persistent host pays, and
//         what the browser demo pays after the page has loaded.
//
// Comparing a cold column against a warm one measures process startup, not
// compilation. So the ratios computed below stay within a kind: wasmtime/native
// answers "how much does wasm cost", chrome/node answers "how much does the
// browser cost over Node", and both sides of each are measured alike.
//
// Reported figure is the MINIMUM over repetitions, not the mean: this machine
// is rarely idle, contention only ever adds time, and a minimum is both the
// better estimate of the true cost and far more stable under load. The spread
// between minimum and median is reported alongside so a run whose numbers were
// badly disturbed is visible rather than silently averaged in.
import { execFile } from 'node:child_process';
import { readFile, writeFile, mkdir, readdir, stat, rm, cp } from 'node:fs/promises';
import { join, dirname, basename } from 'node:path';
import { fileURLToPath } from 'node:url';
import { promisify } from 'node:util';

const run = promisify(execFile);
const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
const WASM = join(ROOT, 'bin', 'troupec.wasm');
const NATIVE = join(ROOT, 'bin', 'troupec');
const WORK = join(ROOT, 'out', 'wasm-compile-bench');

const argv = process.argv.slice(2);
const flag = (name, dflt) => {
  const i = argv.indexOf(name);
  return i === -1 ? dflt : argv[i + 1];
};
const REPS = Number(flag('--reps', 7));
const OUTFILE = flag('--out', join(WORK, 'results.json'));
const NO_BROWSER = argv.includes('--no-browser');
const PORT = 8137;

/** Programs spanning a size range: the ratio's movement with size is the point. */
async function corpus() {
  const dirs = [join(ROOT, 'tests', 'rt', 'pos'), join(ROOT, 'lib')];
  const found = [];
  for (const d of dirs) {
    for (const name of await readdir(d, { recursive: true })) {
      if (!name.endsWith('.trp')) continue;
      const path = join(d, name);
      const src = await readFile(path, 'utf8').catch(() => null);
      if (src === null) continue;
      // Program-relative imports need a module tree staged alongside; libraries
      // need -l. Neither is what this measures.
      if (/^import "/m.test(src)) continue;
      found.push({ path, bytes: (await stat(path)).size, lines: src.split('\n').length });
    }
  }
  found.sort((a, b) => a.bytes - b.bytes);
  // One representative per size band, so the table shows how the ratio moves
  // with input size rather than 400 rows clustered at the small end. The
  // largest in each band is taken: the point is the trend, and a band's small
  // end is already represented by the band below it.
  const bands = [[0, 300], [300, 1000], [1000, 3000], [3000, 8000],
                 [8000, 20000], [20000, Infinity]];
  const picked = [];
  for (const [lo, hi] of bands) {
    const inBand = found.filter((f) => f.bytes > lo && f.bytes <= hi);
    if (inBand.length) picked.push(inBand.at(-1));
  }
  return picked;
}

const stats = (xs) => {
  const s = [...xs].sort((a, b) => a - b);
  return { min: s[0], median: s[(s.length / 2) | 0], max: s.at(-1) };
};

async function timed(fn) {
  const t0 = performance.now();
  await fn();
  return performance.now() - t0;
}

/** Native and wasmtime both go through a subprocess, so measure them alike. */
async function measureProcess(cmd, args, cwd) {
  const ms = [];
  for (let i = 0; i < REPS; i++) {
    ms.push(await timed(() => run(cmd, args, { cwd }).catch((e) => { throw e; })));
  }
  return stats(ms);
}

async function measureNodeWasi(dir) {
  const { WASI } = await import('node:wasi');
  const bytes = await readFile(WASM);
  const mod = await WebAssembly.compile(bytes);
  const ms = [];
  for (let i = 0; i < REPS; i++) {
    ms.push(await timed(async () => {
      const wasi = new WASI({
        version: 'preview1',
        args: ['troupec', 'p.trp', '-o', 'p.js'],
        env: { TROUPE: ROOT, PWD: dir },
        preopens: { '/': '/' },
        returnOnExit: true,
      });
      const inst = await WebAssembly.instantiate(mod, wasi.getImportObject());
      const cwd = process.cwd();
      try { process.chdir(dir); wasi.start(inst); } finally { process.chdir(cwd); }
    }));
  }
  return stats(ms);
}

async function measureChrome(programs) {
  const { chromium } = await import(
    '/Users/aslan/.ghc-wasm/nodejs/lib/node_modules/playwright/index.mjs');
  const server = (await import('node:child_process')).spawn(
    process.execPath, [join(ROOT, 'examples', 'wasm-compiler-demo', 'serve.mjs'), String(PORT)],
    { stdio: 'ignore' });
  await new Promise((r) => setTimeout(r, 1500));
  try {
    const browser = await chromium.launch({ channel: 'chrome' });
    const page = await browser.newPage();
    await page.goto(`http://localhost:${PORT}/`, { waitUntil: 'load' });
    await page.waitForFunction(() => !document.getElementById('compile').disabled, { timeout: 60000 });
    const out = {};
    for (const p of programs) {
      const src = await readFile(p.path, 'utf8');
      out[p.path] = stats(await page.evaluate(async ([source, reps]) => {
        const m = await import('./demo.mjs');
        await m.compileSource(source);              // warm the module
        const ms = [];
        for (let i = 0; i < reps; i++) {
          const t0 = performance.now();
          await m.compileSource(source);
          ms.push(performance.now() - t0);
        }
        return ms;
      }, [src, REPS]));
    }
    await browser.close();
    return out;
  } finally {
    server.kill();
  }
}

const main = async () => {
  const programs = await corpus();
  console.log(`corpus: ${programs.length} programs, ${programs[0].bytes}..${programs.at(-1).bytes} bytes`);
  console.log(`reps: ${REPS} (reporting the minimum)\n`);

  await rm(WORK, { recursive: true, force: true });
  await mkdir(join(WORK, 'run', 'out'), { recursive: true });
  const dir = join(WORK, 'run');

  const results = [];
  const chrome = NO_BROWSER ? {} : await measureChrome(programs);

  for (const p of programs) {
    await cp(p.path, join(dir, 'p.trp'));
    const native = await measureProcess(NATIVE, ['p.trp', '-o', 'p.js'], dir);
    const wasmtime = await measureProcess(
      join(process.env.HOME, '.ghc-wasm', 'wasmtime', 'bin', 'wasmtime'),
      ['run', '--dir', '.', '--dir', `${ROOT}::/troupe`, '--env', 'TROUPE=/troupe',
       WASM, 'p.trp', '-o', 'p.js'], dir);
    const node = await measureNodeWasi(dir).catch((e) => ({ min: NaN, error: String(e) }));
    results.push({
      program: basename(p.path), bytes: p.bytes, lines: p.lines,
      native, wasmtime, node, chrome: chrome[p.path] ?? null,
    });
    const r = results.at(-1);
    const ms = (v) => (Number.isFinite(v) ? `${v.toFixed(0)}ms`.padStart(8) : '     n/a');
    const ratio = (a, b) => (Number.isFinite(a) && Number.isFinite(b)
      ? `${(a / b).toFixed(1)}x`.padStart(6) : '   n/a');
    console.log(
      `${r.program.padEnd(24)}${String(r.bytes).padStart(7)}B │ cold ` +
      `${ms(r.native.min)} ${ms(r.wasmtime.min)} ${ratio(r.wasmtime.min, r.native.min)}` +
      ` │ warm ${ms(r.node.min)} ${ms(r.chrome?.min)} ${ratio(r.chrome?.min, r.node.min)}`);
  }
  console.log('\n' + ' '.repeat(32) + '│ cold: native  wasmtime  ratio │ warm: node   chrome  ratio');
  console.log('cold = fresh process per compile; warm = live process, module already compiled.');
  console.log('Ratios stay within a kind -- comparing cold against warm would measure startup.');

  await mkdir(dirname(OUTFILE), { recursive: true });
  await writeFile(OUTFILE, JSON.stringify({
    when: new Date().toISOString(), reps: REPS,
    node: process.version, platform: `${process.platform}-${process.arch}`,
    results,
  }, null, 2));
  console.log(`\nwrote ${OUTFILE}`);
};

main().catch((e) => { console.error(e); process.exit(1); });
