// What a run needs served over HTTP: the compiled libraries and the service
// module the worker fetches, the worker bundle and the page, and the corpus
// -- its list, and each test prepared on request.

import { createServer } from 'node:http';
import { readFile, readdir } from 'node:fs/promises';
import { join, extname, normalize } from 'node:path';
import { ROOT, listTests, prepareTest, prepareRoundTrip } from './corpus.mjs';

const TYPES = {
    '.html': 'text/html; charset=utf-8',
    '.mjs': 'text/javascript; charset=utf-8',
    '.js': 'text/javascript; charset=utf-8',
    '.json': 'application/json; charset=utf-8',
    '.wasm': 'application/wasm',
};

const SERVED = ['/lib/out/', '/trp-rt/out/', '/build/browser/', '/tests/rt/browser-host/', '/bin/troupec.wasm'];

function send(res, status, body, type) {
    res.writeHead(status, { 'Content-Type': type ?? 'text/plain; charset=utf-8', 'Cache-Control': 'no-cache' });
    res.end(body);
}

async function serveFile(res, path) {
    try {
        send(res, 200, await readFile(path), TYPES[extname(path)]);
    } catch (e) {
        send(res, 404, `not found: ${path}\n`);
    }
}

/** Starts the server on a free port; resolves to { port, close }. */
export function startServer() {
    const prepared = new Map();
    const server = createServer(async (req, res) => {
        const u = new URL(req.url, 'http://localhost');
        const url = decodeURIComponent(u.pathname);
        if (normalize(url).includes('..')) return send(res, 400, 'bad path\n');
        try {
            if (url === '/' || url === '/index.html') return serveFile(res, join(ROOT, 'tests/rt/browser-host/page/index.html'));
            if (url === '/favicon.ico') return send(res, 204, '');
            if (url === '/lib-exports.json') {
                const names = (await readdir(join(ROOT, 'lib', 'out'))).filter((n) => n.endsWith('.exports')).sort();
                return send(res, 200, JSON.stringify(names), TYPES['.json']);
            }
            if (url === '/corpus') return send(res, 200, JSON.stringify(await listTests()), TYPES['.json']);
            if (url === '/round-trip') return send(res, 200, JSON.stringify(await prepareRoundTrip()), TYPES['.json']);
            if (url === '/test') {
                const path = u.searchParams.get('path');
                if (!prepared.has(path)) prepared.set(path, prepareTest(path));
                return send(res, 200, JSON.stringify(await prepared.get(path)), TYPES['.json']);
            }
            if (SERVED.some((p) => url.startsWith(p))) return serveFile(res, join(ROOT, url));
            send(res, 404, 'not found\n');
        } catch (e) {
            send(res, 500, `${e.stack ?? e}\n`);
        }
    });
    return new Promise((resolve) => {
        server.listen(0, '127.0.0.1', () => {
            resolve({ port: server.address().port, close: () => server.close() });
        });
    });
}

/** The code locations a run request names, on this server. */
export function codeLocations(port) {
    const base = `http://127.0.0.1:${port}`;
    return { libraries: `${base}/lib/out/`, service: `${base}/trp-rt/out/service.js` };
}

if (process.argv[1] === new URL(import.meta.url).pathname) {
    const { port } = await startServer();
    console.log(`browser-host corpus page: http://127.0.0.1:${port}/`);
}
