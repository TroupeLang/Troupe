// The browser demo's program editor: examples/web-demo/editor.mjs with
// CodeMirror resolved, as one module the page imports from
// build/browser/troupe-editor.mjs. The site is static and self-hosted, so
// nothing is fetched from a CDN.
import resolve from '@rollup/plugin-node-resolve';

export default {
  input: 'examples/web-demo/editor.mjs',
  output: {
    file: 'build/browser/troupe-editor.mjs',
    format: 'es',
  },
  plugins: [resolve({ browser: true })],
};
