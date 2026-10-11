// The golden corpus as the browser host runs it: which tests, and each
// test compiled with its input, options and golden.
//
// A test is compiled by bin/troupec from the repository root, under the
// path Golden.hs passes, so the positions in an error report match.

import { execFile } from 'node:child_process';
import { readFile, readdir, mkdtemp, mkdir, rm } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, resolve, relative } from 'node:path';
import { promisify } from 'node:util';

export const ROOT = resolve(import.meta.dirname, '..', '..', '..');
const run = promisify(execFile);

const GROUPS = ['tests/rt/pos', 'tests/rt/neg', 'tests/rt/warn'];

/** Directories the browser host does not run, and the capability each needs. */
const EXCLUDED_DIRS = {
    'tests/rt/pos/codec': 'gzip: no compression on this host',
    'tests/rt/neg/codec': 'gzip: no compression on this host',
    'tests/rt/pos/ffi': 'SimpleFiles and Tty are native modules of the Node host',
    'tests/rt/neg/ffi': 'SimpleFiles and Tty are native modules of the Node host',
    'tests/rt/neg/simplefileio': 'SimpleFiles is a native module of the Node host',
    'tests/rt/pos/signals': 'no signals',
    'tests/rt/pos/tty': 'Tty is a native module of the Node host',
};

/** Why the browser host does not run a test, or null when it does. */
export function exclusionReason(path, source) {
    for (const [dir, why] of Object.entries(EXCLUDED_DIRS)) {
        if (path.startsWith(dir + '/')) return why;
    }
    if (/^\s*(import\s+(Tty|SimpleFileIO)|require\s+native\s+(Tty|SimpleFiles))\b/m.test(source)) {
        return 'Tty and SimpleFiles are native modules of the Node host';
    }
    if (existsSync(join(ROOT, path.replace(/\.trp$/, '.deps.json')))) {
        return 'content-addressed modules need a dependencies file';
    }
    return null;
}

/** Whether a program persists values, which needs the origin private file
 *  system: these run in a browser and not under Node. */
export function needsPersistence(source) {
    return /\b(save|restore)\b/.test(source);
}

async function walk(dir, out) {
    for (const e of await readdir(dir, { withFileTypes: true })) {
        const p = join(dir, e.name);
        if (e.isDirectory()) {
            if (e.name !== 'modsrc') await walk(p, out);
        } else if (e.name.endsWith('.trp')) {
            out.push(relative(ROOT, p));
        }
    }
}

/** Repository-relative paths of the corpus, sorted. */
export async function listTests() {
    const all = [];
    for (const g of GROUPS) await walk(join(ROOT, g), all);
    return all.sort();
}

// The words of a .options file, with shell quoting, comments dropped.
function shellWords(text) {
    const words = [];
    for (const line of text.split('\n')) {
        if (line.startsWith('#')) continue;
        const re = /'([^']*)'|"([^"]*)"|(\S+)/g;
        let m;
        while ((m = re.exec(line)) !== null) words.push(m[1] ?? m[2] ?? m[3]);
    }
    return words;
}

/** The options record a .options file asks for. */
export function parseOptions(text) {
    const args = {};
    const words = shellWords(text);
    for (let i = 0; i < words.length; i++) {
        const w = words[i];
        switch (w) {
            case '--': args.programArgs = words.slice(i + 1); i = words.length; break;
            case '--stdiolev': args.stdiolev = words[++i]; break;
            case '--label-format': args['label-format'] = words[++i]; break;
            case '--nmifc': args.nmifc = true; break;
            case '--no-nmifc': args.nmifc = false; break;
            case '--debug': args.debug = true; break;
            case '--io-root': i++; break;  // SimpleFiles is not on this host
            case '--no-rawopt': break;     // a compiler option; the host sees the default build
            default: throw new Error(`unknown option in a .options file: ${w}`);
        }
    }
    return args;
}

async function optional(path) {
    return existsSync(path) ? await readFile(path, 'utf8') : null;
}

/** A test compiled and ready to run: its program text, source, input
 *  lines, options and golden -- or, for one the host does not run, the
 *  reason under `excluded`. */
export async function prepareTest(path) {
    const source = await readFile(join(ROOT, path), 'utf8');
    const excluded = exclusionReason(path, source);
    if (excluded !== null) return { path, excluded };
    const dir = await mkdtemp(join(tmpdir(), 'troupe-browser-host-'));
    try {
        const out = join(dir, 'p.js');
        await run(join(ROOT, 'bin', 'troupec'), ['-m', path, '-o', out], { cwd: ROOT });
        const program = await readFile(out, 'utf8');
        const inputText = await optional(join(ROOT, path + '.input'));
        const optionsText = await optional(join(ROOT, path + '.options'));
        const golden = await readFile(join(ROOT, path.replace(/\.trp$/, '.nocolor.golden')), 'utf8');
        const input = inputText === null ? [] : inputText.replace(/\n$/, '').split('\n');
        return {
            path, program, source, golden, input,
            args: optionsText === null ? {} : parseOptions(optionsText),
            needsPersistence: needsPersistence(source),
        };
    } finally {
        await rm(dir, { recursive: true, force: true });
    }
}

/**
 * The closure round trip: round-trip/save.trp run on a Node node, which
 * persists a closure, and round-trip/restore.trp prepared for a browser node
 * to restore it. Resolves to { persisted: { path, text }, restore }, where
 * restore is a prepared test whose golden is restore.expected.
 */
export async function prepareRoundTrip() {
    const dir = await mkdtemp(join(tmpdir(), 'troupe-round-trip-'));
    try {
        const saveJs = join(dir, 'save.js');
        await run(join(ROOT, 'bin', 'troupec'), ['tests/rt/browser-host/round-trip/save.trp', '-o', saveJs], { cwd: ROOT });
        await mkdir(join(dir, 'out'));
        await run('node', [join(ROOT, 'rt', 'built', 'troupe.mjs'), '-f', saveJs, '--localonly', '--no-color'], { cwd: dir });
        const path = 'out/saved.round-trip.persist.json';
        const text = await readFile(join(dir, path), 'utf8');
        const restorePath = 'tests/rt/browser-host/round-trip/restore.trp';
        const restoreJs = join(dir, 'restore.js');
        await run(join(ROOT, 'bin', 'troupec'), ['-m', restorePath, '-o', restoreJs], { cwd: ROOT });
        const restore = {
            path: restorePath,
            program: await readFile(restoreJs, 'utf8'),
            source: await readFile(join(ROOT, restorePath), 'utf8'),
            golden: await readFile(join(ROOT, 'tests/rt/browser-host/round-trip/restore.expected'), 'utf8'),
            input: [],
            args: {},
        };
        return { persisted: { path, text }, restore };
    } finally {
        await rm(dir, { recursive: true, force: true });
    }
}
