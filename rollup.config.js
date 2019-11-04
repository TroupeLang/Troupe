import resolve from 'rollup-plugin-node-resolve';
import commonjs from 'rollup-plugin-commonjs';
import json from 'rollup-plugin-json';
// import babel from 'rollup-plugin-babel';
import minify from 'rollup-plugin-babel-minify';
// don't unroll node modules. Except winston... Don't ask...
const external = id =>
      !id.startsWith('\0')
      && !id.startsWith('.')
      && !id.startsWith('/')
      && !(id == 'winston');

module.exports = {
  input: 'rt/built/troupe.js',
  output: {
    file: 'build/Troupe/rt/built/troupe.js',
    format: 'cjs'
  },
  
  plugins: [
    resolve(),
    commonjs({ 
      ignore: ["conditional-runtime-dependency"] 
    }),
    json(),
    // babel({}),
    minify({
       "mangle": { eval : true,
                  // topLevel: true,
                  sort : true,
                  sort : true,
                  screw_ie8 : true
                },
      "keepFnName": false,
      "keepClassName": false,
      comments: false
    }
    )
  ],
  external

};
