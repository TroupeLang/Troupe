/**
 * TypeScript side of the differential lattice harness (Step 3).
 *
 * Reads JSON lines `{"x": [[..]], "y": [[..]]}` from stdin, where each of `x`
 * and `y` is a CNF encoded as `string[][]` (list of clauses, clause = list of
 * principal strings). For each line it emits one canonical verdict line
 * comparing the runtime `implies`/`equals` on the two CNFs.
 *
 * The verdict format (`implies=<t|f> equals=<t|f>`, fixed key order) is kept
 * byte-for-byte identical to the Haskell judge (compiler/dev-test/DCTest.hs
 * `--judge`) so the driver can `diff` the two verdict streams directly.
 *
 * Run: node rt/built/proptests/tools/judge.mjs < corpus.jsonl
 */
import * as readline from 'node:readline'
import { CNF, implies } from '../../levels/DCLabels/cnf.mjs'

function verdict(line: string): string {
    const obj = JSON.parse(line) as { x: string[][]; y: string[][] }
    const x = CNF.fromJSON(obj.x)
    const y = CNF.fromJSON(obj.y)
    const imp = implies(x, y)
    const eq = x.equals(y)
    const b = (v: boolean) => (v ? 't' : 'f')
    return `implies=${b(imp)} equals=${b(eq)}`
}

const rl = readline.createInterface({ input: process.stdin, crlfDelay: Infinity })
const out: string[] = []

rl.on('line', (line: string) => {
    if (line.trim().length === 0) return
    out.push(verdict(line))
})

rl.on('close', () => {
    process.stdout.write(out.length === 0 ? '' : out.join('\n') + '\n')
})
