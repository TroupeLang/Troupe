#!/usr/bin/env node
// The golden corpus through the browser host, under Node.
//
// Each test runs in a worker thread the way it runs in a browser worker:
// the same host modules, the same run request, the libraries fetched over
// HTTP. Its stdout is compared with the golden as the golden suite compares
// it (compare.mjs). Tests that persist values need the browser's storage
// and are reported as skipped here; tests/rt/browser-host/page runs them.
//
//   node tests/rt/browser-host/run-corpus.mjs [--filter TEXT] [--jobs N]

import { Worker, MessageChannel } from 'node:worker_threads';
import { readFile, readdir } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { join } from 'node:path';
import { ROOT, listTests, prepareTest } from './corpus.mjs';
import { startServer, codeLocations } from './server.mjs';
import { diffAgainstGolden, runRequest } from './compare.mjs';

const TIMEOUT_MS = 60_000;
const argv = process.argv.slice(2);
const filter = argv.includes('--filter') ? argv[argv.indexOf('--filter') + 1] : '';
const jobs = argv.includes('--jobs') ? Number(argv[argv.indexOf('--jobs') + 1]) : 4;

// The compiler, served in this process when bin/troupec.wasm is built, so
// that every node links through one as it does in a browser; without the
// wasm build (as in CI) the nodes run without a compiler.
async function startCompilerServer() {
    const wasm = join(ROOT, 'bin', 'troupec.wasm');
    if (!existsSync(wasm)) return null;
    const { createCompilerServer } = await import(join(ROOT, 'rt/built/hosts/browser/compilerWorker.mjs'));
    const { browserCompilerService } = await import(join(ROOT, 'rt/built/hosts/browser/compilerService.mjs'));
    const server = createCompilerServer();
    const { port1, port2 } = new MessageChannel();
    server.serve(port2);
    const service = browserCompilerService(port1);
    const libOut = join(ROOT, 'lib', 'out');
    const names = (await readdir(libOut)).filter((n) => n.endsWith('.exports')).sort();
    const interfaces = await Promise.all(names.map(async (n) => [n, new Uint8Array(await readFile(join(libOut, n)))]));
    await service.init((await readFile(wasm)).buffer, interfaces);
    return server;
}

/** Runs one prepared test; resolves to { stdout, stderr, exitCode, error }. */
function runTest(test, code, compilerServer) {
    return new Promise((resolve) => {
        const worker = new Worker(join(ROOT, 'tests/rt/browser-host/worker-entry.mjs'));
        const request = runRequest(test, code, false);
        const transfer = [];
        if (compilerServer) {
            const { port1, port2 } = new MessageChannel();
            compilerServer.serve(port2);
            request.compiler = port1;
            transfer.push(port1);
        }
        const r = { stdout: '', stderr: '', exitCode: null, error: null };
        const timer = setTimeout(() => { r.error = `no exit after ${TIMEOUT_MS / 1000}s`; worker.terminate(); }, TIMEOUT_MS);
        worker.on('message', (m) => {
            if (m.type === 'stdout') r.stdout += m.text;
            else if (m.type === 'stderr') r.stderr += m.text;
            else if (m.type === 'process-exit') r.exitCode = m.exitCode;
            else if (m.type === 'error') r.error = m.message;
        });
        worker.on('error', (e) => { r.error = e.stack ?? String(e); });
        worker.on('exit', () => { clearTimeout(timer); resolve(r); });
        worker.postMessage(request, transfer);
        for (const line of test.input) worker.postMessage({ op: 'stdin', line });
    });
}

const { port, close } = await startServer();
const code = codeLocations(port);
const compilerServer = await startCompilerServer();
console.log(compilerServer ? 'compiler: troupec.wasm, served in this process' : 'compiler: none (bin/troupec.wasm is not built)');
const tests = (await listTests()).filter((p) => p.includes(filter));

let passed = 0, failed = 0, skipped = 0, excluded = 0;
const failures = [];

async function one(path) {
    const test = await prepareTest(path);
    if (test.excluded) {
        excluded++;
        console.log(`EXCLUDED: ${path} (${test.excluded})`);
        return;
    }
    if (test.needsPersistence) {
        skipped++;
        console.log(`SKIP: ${path} (persists values; runs in the browser)`);
        return;
    }
    const r = await runTest(test, code, compilerServer);
    const diff = diffAgainstGolden(test.golden, r.stdout);
    const problem = r.error ?? (r.exitCode !== 0 ? `exit code ${r.exitCode}` : diff);
    if (problem === null) {
        passed++;
        console.log(`PASS: ${path}`);
    } else {
        failed++;
        failures.push(path);
        console.log(`FAIL: ${path}\n${problem}\n${r.stderr ? `--- stderr ---\n${r.stderr}` : ''}`);
    }
}

const queue = tests.slice();
await Promise.all(Array.from({ length: jobs }, async () => {
    while (queue.length > 0) await one(queue.shift());
}));
close();

console.log(`\nbrowser host under Node: ${passed} passed, ${failed} failed, `
    + `${skipped} skipped (persistence), ${excluded} excluded (capabilities not on this host)`);
if (failed > 0) console.log(failures.map((f) => `  ${f}`).join('\n'));
// The compiler's ports, when served here, would keep the process alive.
process.exit(failed > 0 ? 1 : 0);
