#!/usr/bin/env node
// The browser compiler service against the native compiler.
//
// The service is driven exactly as a page drives it, over a message port,
// with the worker side served in this process. Two oracles:
//
//   compileSource -- the JavaScript the wasm compiler emits for a corpus
//   program is byte-identical to what bin/troupec emits for the same source,
//   staged under the same name (positions travel inside blobs).
//
//   linkIR -- the snippets for the blobs lifted out of that JavaScript are
//   the ones `bin/troupec --json-ir` returns, and a repeated blob is served
//   from the worker's memo.
//
// Needs bin/troupec.wasm (make compiler-wasm). Without it the test reports
// that and exits 0, since CI has no wasm toolchain.

import { execFile, execFileSync } from 'node:child_process';
import { readFile, readdir, mkdtemp, writeFile, rm } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { promisify } from 'node:util';

const ROOT = resolve(import.meta.dirname, '..', '..', '..');
const WASM = join(ROOT, 'bin', 'troupec.wasm');
const NATIVE = join(ROOT, 'bin', 'troupec');
const LIB_OUT = join(ROOT, 'lib', 'out');
const run = promisify(execFile);

const PROGRAMS = [
    'tests/rt/pos/core/arithms01.trp',
    'tests/rt/pos/core/fib10.trp',
    'tests/rt/pos/core/freadln.trp',
    'tests/lib/List.trp',
];

if (!existsSync(WASM)) {
    console.log(`SKIP: ${WASM} is not built (make compiler-wasm)`);
    process.exit(0);
}

const { serveCompiler } = await import(join(ROOT, 'rt/built/hosts/browser/compilerWorker.mjs'));
const { browserCompilerService } = await import(join(ROOT, 'rt/built/hosts/browser/compilerService.mjs'));

const { port1, port2 } = new MessageChannel();
serveCompiler(port2);
const service = browserCompilerService(port1);

const names = (await readdir(LIB_OUT)).filter((n) => n.endsWith('.exports')).sort();
const interfaces = await Promise.all(names.map(async (n) => [n, new Uint8Array(await readFile(join(LIB_OUT, n)))]));
await service.init((await readFile(WASM)).buffer, interfaces);

let failed = 0;
function check(cond, what) {
    console.log(`${cond ? 'PASS' : 'FAIL'}: ${what}`);
    if (!cond) failed++;
}

async function nativeCompile(source) {
    const dir = await mkdtemp(join(tmpdir(), 'troupec-native-'));
    try {
        await writeFile(join(dir, 'p.trp'), source);
        await run(NATIVE, ['p.trp', '-o', 'p.js'], { cwd: dir });
        return await readFile(join(dir, 'p.js'), 'utf8');
    } finally {
        await rm(dir, { recursive: true, force: true });
    }
}

async function nativeLink(blobs) {
    const marker = '/*-----*/';
    const stdout = execFileSync(NATIVE, ['--json-ir'], {
        input: `${blobs.join('\n')}\n!ECHO ${marker}\n`, maxBuffer: 64 * 1024 * 1024, encoding: 'utf8',
    });
    const snippets = [];
    for (const part of stdout.split('\n\n').map((s) => s.trim()).filter(Boolean)) {
        if (part === marker) break;
        snippets.push(JSON.parse(part));
    }
    return snippets;
}

for (const program of PROGRAMS) {
    const source = await readFile(join(ROOT, program), 'utf8');
    const r = await service.compileSource(source);
    check(r.ok, `${program}: compiles under the service (exit ${r.code}${r.crash ? ', crash ' + r.crash : ''})`);
    if (!r.ok) continue;
    const native = await nativeCompile(source);
    check(r.js === native, `${program}: emitted JavaScript is byte-identical to bin/troupec's (${r.js.length} bytes)`);
    check(r.stages.size === 12, `${program}: twelve stage dumps recovered (${r.stages.size})`);

    const blobs = [...r.js.matchAll(/\.serialized = "([A-Za-z0-9+/=]+)"/g)].map((m) => m[1]).slice(0, 3);
    if (blobs.length === 0) continue;
    const linked = await service.linkIR(blobs);
    const expected = await nativeLink(blobs);
    check(JSON.stringify(linked) === JSON.stringify(expected),
          `${program}: linkIR of ${blobs.length} blob(s) matches bin/troupec --json-ir`);
    const again = await service.linkIR([blobs[0], blobs[0]]);
    check(JSON.stringify(again) === JSON.stringify([expected[0], expected[0]]),
          `${program}: a repeated blob is served from the memo, in order`);
}

await service.linkIR(['not-a-blob']).then(
    () => check(false, 'a malformed blob is rejected'),
    (e) => check(true, `a malformed blob is rejected (${e.message.split('\n')[0]})`));

console.log(`\nResults: ${failed === 0 ? 'all passed' : failed + ' failed'}`);
port1.close(); port2.close();
process.exit(failed === 0 ? 0 : 1);
