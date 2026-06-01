import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import json from '@rollup/plugin-json';
import terser from '@rollup/plugin-terser';

// don't unroll node modules. Except winston... Don't ask...
const external = id =>
      !id.startsWith('\0')
      && !id.startsWith('.')
      && !id.startsWith('/')
      && !id.startsWith('rt/')
      && !(id == 'winston');

export default {
  input: 'rt/built/troupe.mjs',
  output: {
    file: 'build/Troupe/rt/built/troupe.js',
    format: 'cjs'
  },

  // Rollup's tree-shaking pass hangs indefinitely (observed 5+ CPU-hours) on
  // the compiled runtime in rt/built; with treeshake disabled the same bundle
  // builds in ~1s. The exact trigger is unconfirmed -- it is a known class of
  // Rollup tree-shaking performance pathology (cf. rollup/rollup#5729), and
  // this graph does have circular dependencies (e.g. runtimeMonitored <->
  // MailboxProcessor <-> QuarantineUtils <-> deserialize), but those have not
  // been confirmed as the cause. We don't need dead-code elimination here
  // (terser still minifies the output), so disable tree-shaking.
  treeshake: false,

  plugins: [
    resolve(),
    commonjs({
      ignoreDynamicRequires: true
    }),
    json(),
    terser({
      mangle: {
        eval: true
      },
      keep_fnames: false,
      keep_classnames: false,
      format: {
        comments: false
      }
    })
  ],
  external
};
