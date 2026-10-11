// Comparing a run's output with its golden, as tests/_util/diff.sh does:
// a line that carries a logger timestamp or a uuid is not compared.
// Shared by the Node runner and the page, so it imports nothing.

const IGNORED = [
    /^\d{4}-\d{2}-\d{2}T.*\[RTM\]/,
    /[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/,
];

function comparable(text) {
    return text.split('\n').filter((line) => !IGNORED.some((re) => re.test(line)));
}

/** The differing lines, or null when the outputs agree. */
export function diffAgainstGolden(golden, actual) {
    const g = comparable(golden);
    const a = comparable(actual);
    const out = [];
    const n = Math.max(g.length, a.length);
    for (let i = 0; i < n; i++) {
        if (g[i] !== a[i]) {
            out.push(`-${g[i] ?? '<end>'}`);
            out.push(`+${a[i] ?? '<end>'}`);
        }
    }
    return out.length === 0 ? null : out.join('\n');
}

/** The run request a prepared test makes, over the host's defaults. */
export function runRequest(test, code, supervised) {
    return {
        op: 'run',
        program: test.program,
        sources: { [test.path]: test.source },
        args: { localonly: true, 'no-color': true, ...test.args },
        code,
        supervised,
    };
}
