// The runtime worker for a browser: rt/built/hosts/browser/runtimeWorkerMain.mjs
// with the runtime and its packages resolved, as a module a page can start
// with `new Worker('build/browser/troupe-worker.mjs', {type: 'module'})`.
//
// The runtime is a separate chunk, loaded by the dynamic import in the
// bootstrap: several runtime modules read the host's registrations at import
// time, so the runtime must not be evaluated with the entry. The chunks sit
// beside the entry and are fetched relative to it.
import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import json from '@rollup/plugin-json';

// cli-table3 asks for @colors/colors inside a try, as an optional colouring
// of its output; a browser has no package by that name, and the table is
// rendered plain without it.
const withoutColors = {
  name: 'without-colors',
  resolveId: (id) => (id === '@colors/colors/safe' ? '\0colors-stub' : null),
  load: (id) => (id === '\0colors-stub' ? 'export default {};' : null),
};

export default {
  input: 'rt/built/hosts/browser/runtimeWorkerMain.mjs',
  output: {
    dir: 'build/browser',
    entryFileNames: 'troupe-worker.mjs',
    chunkFileNames: 'troupe-worker-[name].mjs',
    format: 'es',
  },
  // See rollup.config.js: the tree-shaking pass hangs for hours on rt/built.
  treeshake: false,
  plugins: [
    withoutColors,
    resolve({ browser: true, preferBuiltins: false }),
    commonjs({ ignoreDynamicRequires: true }),
    json(),
  ],
};
