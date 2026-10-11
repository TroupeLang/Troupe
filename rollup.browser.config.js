// Probe bundle: attempt to build the existing runtime for a browser worker.
// Purpose is diagnostic -- collect the real resolution failures rather than
// working from a static inventory. Not a shipping config.
import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import json from '@rollup/plugin-json';

export default {
  input: 'rt/built/hosts/browser/main.mjs',
  output: {
    file: 'build/browser-probe/troupe-worker.mjs',
    format: 'es',
    inlineDynamicImports: true,
  },
  // See rollup.config.js: the tree-shaking pass hangs for hours on rt/built.
  treeshake: false,
  plugins: [
    resolve({ browser: true, preferBuiltins: false }),
    commonjs({ ignoreDynamicRequires: true }),
    json(),
  ],
};
