// Serves the assembled site, build/web-demo (make web-demo), for a browser
// on this machine:
//
//   node examples/web-demo/serve.mjs [port]
//
// It exists because a page cannot fetch() from file:// and the .wasm needs
// its content type; GitHub Pages serves the same directory. Every response
// carries the cross-origin isolation headers, which give the page the finer
// performance.now() clamp; a static host without them serves the site all
// the same.
import { createServer } from 'node:http';
import { readFile } from 'node:fs/promises';
import { join, dirname, extname, normalize } from 'node:path';
import { fileURLToPath } from 'node:url';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..', '..');
const SITE = join(ROOT, 'build', 'web-demo');
const port = Number(process.argv[2] ?? 8080);

const TYPES = {
  '.html': 'text/html; charset=utf-8',
  '.mjs': 'text/javascript; charset=utf-8',
  '.js': 'text/javascript; charset=utf-8',
  '.json': 'application/json; charset=utf-8',
  '.css': 'text/css; charset=utf-8',
  '.wasm': 'application/wasm',
  '.trp': 'text/plain; charset=utf-8',
  '.exports': 'application/octet-stream',
};

function send(res, status, body, type) {
  const buf = Buffer.isBuffer(body) ? body : Buffer.from(String(body));
  res.writeHead(status, {
    'Content-Type': type ?? 'text/plain; charset=utf-8',
    'Content-Length': buf.length,
    'Cache-Control': 'no-cache',
    'Cross-Origin-Opener-Policy': 'same-origin',
    'Cross-Origin-Embedder-Policy': 'require-corp',
  });
  res.end(res.req?.method === 'HEAD' ? undefined : buf);
}

const server = createServer(async (req, res) => {
  const url = decodeURIComponent(new URL(req.url, 'http://localhost').pathname);
  if (normalize(url).includes('..')) return send(res, 400, 'bad path\n');
  if (url === '/favicon.ico') return send(res, 204, '');
  const path = join(SITE, url === '/' ? 'index.html' : url);
  try {
    send(res, 200, await readFile(path), TYPES[extname(path)]);
  } catch {
    send(res, 404, `not found: ${url}\nIs the site built? make web-demo\n`);
  }
});

server.listen(port, () => {
  console.log(`Troupe in the browser: http://localhost:${port}/  (serving ${SITE})`);
});
