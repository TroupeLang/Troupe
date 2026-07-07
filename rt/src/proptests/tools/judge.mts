/**
 * TypeScript side of the differential lattice harness.
 *
 * Reads JSON lines from stdin and emits one canonical verdict line per case,
 * kept byte-for-byte identical to the Haskell judge
 * (compiler/dev-test/DCTest.hs `--judge`) so the driver can `diff` the two
 * verdict streams directly.
 *
 * Two case kinds are dispatched on a `kind` discriminator:
 *
 *   - `kind:"cnf"` (or absent, for legacy Step 3 lines):
 *       `{"x": [[..]], "y": [[..]]}` — each of `x`/`y` a CNF as `string[][]`
 *       (list of clauses, clause = list of principal strings). Verdict:
 *       `implies=<t|f> equals=<t|f>` comparing the runtime `implies`/`equals`.
 *
 *   - `kind:"v1"` (Step 4c):
 *       `{"raw": "{ BOB, alice }", "canon": "alice,bob"}` — two V1 label
 *       surface strings. Verdict: `v1eq=<t|f>` where the boolean is
 *       `fromV1String(raw).equals(fromV1String(canon))`, i.e. whether the two
 *       V1 strings denote the same DC label under runtime normalization.
 *
 * Run: node rt/built/proptests/tools/judge.mjs < corpus.jsonl
 */
import * as readline from 'node:readline'
import { CNF, implies } from '../../levels/DCLabels/cnf.mjs'
import { levels } from '../../levels/DCLabels/dclabel.mjs'

const b = (v: boolean) => (v ? 't' : 'f')

function cnfVerdict(obj: { x: string[][]; y: string[][] }): string {
    const x = CNF.fromJSON(obj.x)
    const y = CNF.fromJSON(obj.y)
    return `implies=${b(implies(x, y))} equals=${b(x.equals(y))}`
}

function v1Verdict(obj: { raw: string; canon: string }): string {
    const eq = levels.fromV1String(obj.raw).equals(levels.fromV1String(obj.canon))
    return `v1eq=${b(eq)}`
}

function verdict(line: string): string {
    const obj = JSON.parse(line) as { kind?: string } & Record<string, unknown>
    if (obj.kind === 'v1') {
        return v1Verdict(obj as unknown as { raw: string; canon: string })
    }
    return cnfVerdict(obj as unknown as { x: string[][]; y: string[][] })
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
