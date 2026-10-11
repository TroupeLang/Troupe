// The compiler worker for a browser: rt/built/hosts/browser/compilerWorkerMain.mjs
// with the WASI shim resolved, as one module a page can start with
// `new Worker('build/browser/compiler-worker.mjs', {type: 'module'})`.
import resolve from '@rollup/plugin-node-resolve';

export default {
  input: 'rt/built/hosts/browser/compilerWorkerMain.mjs',
  output: {
    file: 'build/browser/compiler-worker.mjs',
    format: 'es',
  },
  plugins: [resolve({ browser: true })],
};
