// The page's side of a browser Troupe node: rt/built/hosts/browser/page.mjs
// as one module a page imports from build/browser/troupe-page.mjs.
export default {
  input: 'rt/built/hosts/browser/page.mjs',
  output: {
    file: 'build/browser/troupe-page.mjs',
    format: 'es',
  },
};
