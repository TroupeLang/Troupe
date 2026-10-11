// The compiler's pipeline, as `-v` dumps it.

/** The pipeline, in the order the compiler runs it: the file `-v` writes,
 *  and a label for it. */
export const STAGES: Array<[string, string]> = [
    ['out.syntax', 'parsed'],
    ['out.opreassoc', 'operators resolved'],
    ['out.nopats', 'patterns eliminated'],
    ['out.lowered', 'lowered'],
    ['out.alpha', 'alpha renamed'],
    ['out.cps', 'CPS'],
    ['out.cpsopt', 'CPS optimized'],
    ['out.ir', 'IR'],
    ['out.iropt', 'IR optimized'],
    ['out.rawout', 'raw'],
    ['out.rawopt', 'raw optimized'],
    ['out.stack', 'stack'],
];
