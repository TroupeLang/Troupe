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

  // The compiled runtime in rt/built has circular dependencies (e.g.
  // runtimeMonitored <-> MailboxProcessor <-> QuarantineUtils <-> deserialize),
  // which make Rollup's tree-shaking pass hang indefinitely on this graph.
  // We don't need dead-code elimination here (terser still minifies the
  // output), so disable tree-shaking to keep the bundle build fast.
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
