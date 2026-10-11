// The golden corpus, run in the browser: one runtime worker per test, the
// test prepared by the server, the verdict by compare.mjs.
//
// window.corpusRun(filter) runs it and resolves to the results, for a
// driver; the Run button does the same from the page.

import { diffAgainstGolden, runRequest } from '/tests/rt/browser-host/compare.mjs';
import { fetchInterfaces, startCompiler, compilerPort, startNode } from '/build/browser/troupe-page.mjs';

const TIMEOUT_MS = 60_000;
const WORKER = '/build/browser/troupe-worker.mjs';
const code = { libraries: `${location.origin}/lib/out/`, service: `${location.origin}/trp-rt/out/service.js` };

// The compiler worker, started once when bin/troupec.wasm is served; every
// node links its received closures through it. Without the wasm build the
// nodes run without a compiler, and the tests that restore a closure say so.
let compilerPromise = null;
function compiler() {
    compilerPromise ??= (async () => {
        if (!(await fetch('/bin/troupec.wasm', { method: 'HEAD' })).ok) return null;
        const interfaces = await fetchInterfaces('/lib-exports.json', '/lib/out/');
        return startCompiler('/build/browser/compiler-worker.mjs', '/bin/troupec.wasm', interfaces);
    })();
    return compilerPromise;
}

async function runTest(test) {
    const service = await compiler();
    const request = runRequest(test, code, false);
    if (service) request.compiler = await compilerPort(service);
    const r = { stdout: '', stderr: '', exitCode: null, error: null };
    const node = startNode(WORKER, request, {
        onStdout: (t) => { r.stdout += t; },
        onStderr: (t) => { r.stderr += t; },
        onError: (m) => { r.error = m; },
    });
    for (const line of test.input) node.feedLine(line);
    const timeout = new Promise((resolve) => setTimeout(() => resolve(null), TIMEOUT_MS));
    try {
        const exit = await Promise.race([node.exit, timeout]);
        if (exit === null) r.error = `no exit after ${TIMEOUT_MS / 1000}s`;
        else r.exitCode = exit.exitCode;
    } catch (e) {
        r.error = e.message;
    } finally {
        node.terminate();
    }
    return r;
}

async function runCorpus(filter = '') {
    const rows = document.getElementById('rows');
    const summary = document.getElementById('summary');
    rows.replaceChildren();
    const tests = (await (await fetch('/corpus')).json()).filter((p) => p.includes(filter));
    const results = [];
    let passed = 0, failed = 0, excluded = 0;
    const row = (verdict, path, detail) => {
        const tr = document.createElement('tr');
        tr.innerHTML = `<td class="verdict ${verdict}">${verdict}</td><td></td>`;
        tr.lastChild.textContent = path;
        if (detail) {
            const pre = document.createElement('pre');
            pre.textContent = detail;
            tr.lastChild.appendChild(pre);
        }
        rows.appendChild(tr);
    };
    for (const path of tests) {
        const test = await (await fetch(`/test?path=${encodeURIComponent(path)}`)).json();
        if (test.excluded) {
            excluded++;
            row('SKIP', path, test.excluded);
            continue;
        }
        const r = await runTest(test);
        const diff = diffAgainstGolden(test.golden, r.stdout);
        const problem = r.error ?? (r.exitCode !== 0 ? `exit code ${r.exitCode}` : diff);
        const verdict = problem === null ? 'PASS' : 'FAIL';
        if (verdict === 'PASS') passed++; else failed++;
        results.push({ path, verdict, problem, stderr: r.stderr });
        row(verdict, path, problem === null ? null : problem + (r.stderr ? `\n--- stderr ---\n${r.stderr}` : ''));
        summary.textContent = `${passed + failed} of ${tests.length} run: ${passed} passed, ${failed} failed`;
    }
    summary.textContent = `done: ${passed} passed, ${failed} failed, ${excluded} excluded of ${tests.length}`;
    return { passed, failed, excluded, results };
}

/**
 * The closure round trip: a closure persisted by a Node node, written to
 * this origin's storage where the browser node's persistence store reads
 * it, restored there through the compiler, and applied.
 */
async function roundTrip() {
    const rows = document.getElementById('rows');
    const summary = document.getElementById('summary');
    const { persisted, restore } = await (await fetch('/round-trip')).json();
    let dir = await navigator.storage.getDirectory();
    const parts = persisted.path.split('/');
    for (const p of ['troupe', ...parts.slice(0, -1)]) dir = await dir.getDirectoryHandle(p, { create: true });
    const w = await (await dir.getFileHandle(parts[parts.length - 1], { create: true })).createWritable();
    await w.write(persisted.text);
    await w.close();
    const r = await runTest(restore);
    const diff = diffAgainstGolden(restore.golden, r.stdout);
    const problem = r.error ?? (r.exitCode !== 0 ? `exit code ${r.exitCode}` : diff);
    const verdict = problem === null ? 'PASS' : 'FAIL';
    const tr = document.createElement('tr');
    tr.innerHTML = `<td class="verdict ${verdict}">${verdict}</td><td></td>`;
    tr.lastChild.textContent = `round trip: ${restore.path} restoring what a Node node persisted`;
    if (problem !== null) {
        const pre = document.createElement('pre');
        pre.textContent = problem + (r.stderr ? `\n--- stderr ---\n${r.stderr}` : '');
        tr.lastChild.appendChild(pre);
    }
    rows.appendChild(tr);
    summary.textContent = `round trip: ${verdict}`;
    return { verdict, problem, stdout: r.stdout, stderr: r.stderr };
}

window.corpusRun = runCorpus;
window.roundTrip = roundTrip;
document.getElementById('roundTrip').addEventListener('click', roundTrip);
document.getElementById('run').addEventListener('click', () => {
    runCorpus(document.getElementById('filter').value);
});
