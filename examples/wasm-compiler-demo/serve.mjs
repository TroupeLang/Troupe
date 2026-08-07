// Dev server for the in-browser compiler demo.
//
//   node examples/wasm-compiler-demo/serve.mjs [port]
//
// Serves the page, the WebAssembly build of the compiler, the standard
// library's interface files, and the WASI shim, all from the repository. It
// exists because a page cannot fetch() from file:// and the .wasm needs its
// own content type; nothing here is part of the demo's logic.
import { createServer } from 'node:http';
import { readFile, readdir } from 'node:fs/promises';
import { join, dirname, extname, normalize } from 'node:path';
import { fileURLToPath } from 'node:url';

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = join(HERE, '..', '..');
const SHIM = join(ROOT, 'node_modules', '@bjorn3', 'browser_wasi_shim', 'dist');
const LIB_OUT = join(ROOT, 'lib', 'out');
const WASM = join(ROOT, 'bin', 'troupec.wasm');

const port = Number(process.argv[2] ?? 8080);

const TYPES = {
  '.html': 'text/html; charset=utf-8',
  '.mjs': 'text/javascript; charset=utf-8',
  '.js': 'text/javascript; charset=utf-8',
  '.json': 'application/json; charset=utf-8',
  '.wasm': 'application/wasm',
};

function send(res, status, body, type) {
  const buf = Buffer.isBuffer(body) ? body : Buffer.from(String(body));
  res.writeHead(status, {
    'Content-Type': type ?? 'text/plain; charset=utf-8',
    // Set explicitly so a HEAD response still reports the size -- the page
    // reads it to show how large the compiler is before fetching it.
    'Content-Length': buf.length,
    'Cache-Control': 'no-cache',
  });
  res.end(res.req?.method === 'HEAD' ? undefined : buf);
}

async function serveFile(res, path) {
  try {
    send(res, 200, await readFile(path), TYPES[extname(path)]);
  } catch (e) {
    if (path === WASM) {
      send(res, 404, 'bin/troupec.wasm is missing. Build it with: make compiler-wasm\n');
    } else {
      send(res, 404, `not found: ${path}\n${e.message}\n`);
    }
  }
}

const server = createServer(async (req, res) => {
  // Strip the query and refuse any path that climbs out of what we serve.
  const url = decodeURIComponent(new URL(req.url, 'http://localhost').pathname);
  if (normalize(url).includes('..')) return send(res, 400, 'bad path\n');

  if (url === '/favicon.ico') return send(res, 204, '');
  if (url === '/' || url === '/index.html') return serveFile(res, join(HERE, 'index.html'));
  if (url === '/demo.mjs') return serveFile(res, join(HERE, 'demo.mjs'));
  if (url === '/troupec.wasm') return serveFile(res, WASM);

  if (url.startsWith('/vendor/wasi/')) {
    return serveFile(res, join(SHIM, url.slice('/vendor/wasi/'.length)));
  }

  // The interface files the compiler reads for `import <Library>`: a manifest
  // so the page knows what to fetch, then the files themselves.
  if (url === '/lib-exports.json') {
    try {
      const names = (await readdir(LIB_OUT)).filter((n) => n.endsWith('.exports')).sort();
      return send(res, 200, JSON.stringify(names), TYPES['.json']);
    } catch {
      return send(res, 200, '[]', TYPES['.json']);
    }
  }
  if (url.startsWith('/lib-exports/')) {
    return serveFile(res, join(LIB_OUT, url.slice('/lib-exports/'.length)));
  }

  send(res, 404, 'not found\n');
});

server.listen(port, () => {
  console.log(`troupec-in-the-browser demo: http://localhost:${port}/`);
  console.log(`  compiler:  ${WASM}`);
  console.log(`  libraries: ${LIB_OUT}`);
});
