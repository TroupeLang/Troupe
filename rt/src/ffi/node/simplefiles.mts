'use strict'
import { mkBase } from '../../builtins/UserRuntimeZero.mjs'
import { LVal } from '../../Lval.mjs';
import { Record } from '../../Record.mjs';
import { ROOT } from '../../Level.mjs';
import { assertIsNTuple, assertIsRootAuthority, assertIsString } from '../../Asserts.mjs'
import { __unitbase } from '../../UnitBase.mjs';
import { mkList } from '../../ValuesUtil.mjs';
import { getCliArgs, TroupeCliArg } from '../../TroupeCliArgs.mjs';
import { getRuntimeObject } from '../../SysState.mjs'
import * as fs from 'node:fs';
import * as path from 'node:path';
import * as os from 'node:os';

/**
 * SimpleFiles — the whole-file I/O native module (manifest ffi/SimpleFiles.exports),
 * consumed through lib/SimpleFileIO.trp. Placeholder surface.
 *
 * Operations: readFile, writeFile, readFileBytes, writeFileBytes, appendFile, fileExists,
 * readDir, makeDir, fileStat, removeFile. Directory removal, rename, copy and streaming reads
 * are deliberately absent.
 *
 * Text and bytes:
 *  - readFile/writeFile/appendFile are UTF-8. `readFile` decodes with a fatal decoder: a file
 *    that is not valid UTF-8 is an `Err`, not a string with U+FFFD in it, because the
 *    substitution is invisible to the program and a later write puts the replacement on disk
 *    where the original byte was.
 *  - readFileBytes/writeFileBytes carry the file's bytes as a string with one code unit per
 *    byte (`latin1`), the byte-string convention of the codec built-ins
 *    (rt/src/builtins/codec.mts) and of the tty reader (rt/src/builtins/tty.mts). A string
 *    holding a code unit above 255 is not a byte string, and `writeFileBytes` refuses one as a
 *    thread error rather than truncating it — as base64Encode and gunzip do, and for the same
 *    reason: the convention is only safe if breaking it is loud.
 *  - `appendFile` has no byte-level counterpart. Appending bytes is an open item, not a
 *    decision: the pair above covers the read-modify-write use, and a third operation would be
 *    added when something needs it.
 *
 * Security design (see _dev_planning/_archive/tier2-libraries/spec-simple-file-io.md):
 *  - Every operation requires FULL (ROOT) authority, mirroring `persist`. Untrusted code
 *    cannot reach the filesystem at all, so per-write confidentiality checks, per-path
 *    levels, bounded-integrity read content, and quarantine integration are all deferred
 *    rather than half-answered.
 *  - Read content is labeled at ROOT ("we trust our own files"), exactly as `persist` labels
 *    restored data.
 *  - A single `--io-root` subtree bounds path reachability, orthogonal to authority: even a
 *    bug in ROOT code cannot scribble outside it. `..`, absolute, and symlink escapes are
 *    rejected before any filesystem access.
 *  - Error payloads carry the caller-supplied (io-root-relative) path, never the resolved
 *    absolute path, so observable output is machine-independent (golden determinism).
 */

// Resolve the sandbox root on first use, memoized. Native-module imports must be
// side-effect-free (each host assembles its own registrations), so the resolution cannot run
// at module load as it did when this code was a builtin mixin; deferring it to the first file
// operation also means a program that never touches the filesystem creates no scratch
// directory.
//  - `--io-root <dir>`: created if missing, then realpath'd.
//  - unset: a per-invocation scratch dir under the OS temp dir. Its name never appears in any
//    observable output (errors echo the caller's relative path), so tests stay hermetic and
//    golden-stable without passing any runtime flag.
let __ioRoot: string | null = null;

function ioRoot(): string {
    if (__ioRoot === null) {
        const argv = getCliArgs();
        const configured = argv[TroupeCliArg.IoRoot];
        if (configured) {
            fs.mkdirSync(configured, { recursive: true });
            __ioRoot = fs.realpathSync(configured);
        } else {
            __ioRoot = fs.realpathSync(fs.mkdtempSync(path.join(os.tmpdir(), 'troupe-io-')));
        }
    }
    return __ioRoot;
}

/**
 * `ignoreBOM: true` keeps a leading U+FEFF in the decoded string instead of dropping it, which
 * is what Node's own `buf.toString('utf8')` does: the decoder is here to reject invalid bytes,
 * not to change what a valid file reads as.
 */
const utf8Decoder = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true });

/** Index of the first character outside 0..255, or -1 if the string is bytes (codec.mts). */
function firstNonByte(s: string): number {
    for (let i = 0; i < s.length; i++) {
        if (s.charCodeAt(i) > 0xff) return i;
    }
    return -1;
}

function withinRoot(p: string): boolean {
    const root = ioRoot();
    return p === root || p.startsWith(root + path.sep);
}

type Resolved = { ok: boolean; resolved: string; reason: string };

/**
 * Resolve a caller path against the io-root and reject any escape. Checks the lexical resolution
 * (handles `..` and absolute paths) and then the realpath of the nearest existing ancestor
 * (handles symlink escapes for both existing and not-yet-created targets).
 */
function resolveInSandbox(p: string): Resolved {
    const resolved = path.resolve(ioRoot(), p);
    if (!withinRoot(resolved)) {
        return { ok: false, resolved: '', reason: 'path escapes the io-root sandbox' };
    }
    let probe = resolved;
    // Walk up to the nearest existing ancestor and confirm it does not symlink outside the root.
    // A not-yet-created file (write target) resolves via its existing parent.
    // eslint-disable-next-line no-constant-condition
    while (true) {
        try {
            const real = fs.realpathSync(probe);
            if (!withinRoot(real)) {
                return { ok: false, resolved: '', reason: 'path escapes the io-root sandbox via a symlink' };
            }
            return { ok: true, resolved, reason: '' };
        } catch (e: any) {
            if (e && e.code === 'ENOENT') {
                const parent = path.dirname(probe);
                if (parent === probe) {
                    return { ok: true, resolved, reason: '' };
                }
                probe = parent;
                continue;
            }
            // Through errMessage, as the operation bodies report: a raw e.message here
            // carries the resolved absolute path, which the module's error contract
            // (machine-independent, caller-relative paths only) forbids.
            return { ok: false, resolved: '', reason: errMessage(e) };
        }
    }
}

function errMessage(e: unknown): string {
    if (e && typeof e === 'object' && 'code' in e) {
        const code = (e as any).code;
        if (code === 'ENOENT') return 'file not found';
        if (code === 'EISDIR') return 'path is a directory';
        if (code === 'ENOTDIR') return 'path is not a directory';
        if (code === 'ENOTEMPTY') return 'directory is not empty';
        if (code === 'EACCES' || code === 'EPERM') return 'permission denied';
    }
    return e instanceof Error ? e.message : String(e);
}

/**
 * The `kind` reported for a directory entry or a stat'd path. A symlink is reported as 'other'
 * rather than resolved: the sandbox refuses to traverse symlinks out of the root, so following
 * one here would report a kind for a path the read/write operations will then reject.
 */
function direntKind(d: fs.Dirent): string {
    if (d.isDirectory()) return 'dir';
    if (d.isFile()) return 'file';
    return 'other';
}

function statKind(s: fs.Stats): string {
    if (s.isDirectory()) return 'dir';
    if (s.isFile()) return 'file';
    return 'other';
}

// Result construction. Every leaf is labeled at ROOT: read content is fully trusted
// (only ROOT code reaches this API), matching `persist`'s stance on restored data.
function mkOk(value: LVal): LVal {
    const rec = Record.mkRecord([
        ['tag', new LVal('Ok', ROOT)],
        ['value', value],
    ]);
    return new LVal(rec, ROOT);
}

function mkErr(reason: string, pathStr: string): LVal {
    const errRec = Record.mkRecord([
        ['reason', new LVal(reason, ROOT)],
        ['path', new LVal(pathStr, ROOT)],
    ]);
    const rec = Record.mkRecord([
        ['tag', new LVal('Err', ROOT)],
        ['error', new LVal(errRec, ROOT)],
    ]);
    return new LVal(rec, ROOT);
}

// Suspend the current thread, run an async filesystem op, and resume it with a Result.
function suspendWithResult(build: () => Promise<LVal>) {
    const runtime = getRuntimeObject();
    const theThread = runtime.$t;
    (async () => {
        let res: LVal;
        try {
            res = await build();
        } catch (e) {
            // build() only throws on programming errors; surface as a thread error.
            theThread.throwInSuspended('SimpleFileIO internal error: ' + errMessage(e));
            runtime.__sched.scheduleThread(theThread);
            runtime.__sched.resumeLoopAsync();
            return;
        }
        theThread.returnSuspended(res);
        runtime.__sched.scheduleThread(theThread);
        runtime.__sched.resumeLoopAsync();
    })();
}

export const simpleFilesExports = {
    readFile: mkBase((larg) => {
        assertIsNTuple(larg, 2);
        getRuntimeObject().$t.raiseCurrentThreadPC(larg.lev);
        const arg = larg.val;
        assertIsRootAuthority(arg[0]);
        assertIsString(arg[1]);
        const origPath = arg[1].val;

        const r = resolveInSandbox(origPath);
        if (!r.ok) {
            return getRuntimeObject().ret(mkErr(r.reason, origPath));
        }
        suspendWithResult(async () => {
            let raw: Buffer;
            try {
                raw = await fs.promises.readFile(r.resolved);
            } catch (e) {
                return mkErr(errMessage(e), origPath);
            }
            // The decode is separate from the read so a decoding failure cannot be reported
            // as a filesystem error, and vice versa.
            try {
                return mkOk(new LVal(utf8Decoder.decode(raw), ROOT));
            } catch {
                return mkErr('file is not valid UTF-8', origPath);
            }
        });
    }, 'readFile'),

    // The file's bytes, one code unit each. Same authority, sandbox, labeling and error
    // dispositions as readFile; the difference is that nothing is decoded, so every file
    // reads and no content is altered.
    readFileBytes: mkBase((larg) => {
        assertIsNTuple(larg, 2);
        getRuntimeObject().$t.raiseCurrentThreadPC(larg.lev);
        const arg = larg.val;
        assertIsRootAuthority(arg[0]);
        assertIsString(arg[1]);
        const origPath = arg[1].val;

        const r = resolveInSandbox(origPath);
        if (!r.ok) {
            return getRuntimeObject().ret(mkErr(r.reason, origPath));
        }
        suspendWithResult(async () => {
            try {
                const raw = await fs.promises.readFile(r.resolved);
                return mkOk(new LVal(raw.toString('latin1'), ROOT));
            } catch (e) {
                return mkErr(errMessage(e), origPath);
            }
        });
    }, 'readFileBytes'),

    writeFile: mkBase((larg) => {
        assertIsNTuple(larg, 3);
        getRuntimeObject().$t.raiseCurrentThreadPC(larg.lev);
        const arg = larg.val;
        assertIsRootAuthority(arg[0]);
        assertIsString(arg[1]);
        assertIsString(arg[2]);
        const origPath = arg[1].val;
        const contents = arg[2].val;

        const r = resolveInSandbox(origPath);
        if (!r.ok) {
            return getRuntimeObject().ret(mkErr(r.reason, origPath));
        }
        suspendWithResult(async () => {
            try {
                await fs.promises.writeFile(r.resolved, contents, 'utf8');
                return mkOk(new LVal(__unitbase, ROOT));
            } catch (e) {
                return mkErr(errMessage(e), origPath);
            }
        });
    }, 'writeFile'),

    // One byte per code unit. A string that is not a byte string is a defect in the calling
    // program rather than a bad environment, so it is a thread error and not an Err — the
    // disposition base64Encode and gunzip take for the same argument (codec.mts). It is
    // checked before the path is resolved: the argument is wrong whatever the path is.
    writeFileBytes: mkBase((larg) => {
        assertIsNTuple(larg, 3);
        getRuntimeObject().$t.raiseCurrentThreadPC(larg.lev);
        const arg = larg.val;
        assertIsRootAuthority(arg[0]);
        assertIsString(arg[1]);
        assertIsString(arg[2]);
        const origPath = arg[1].val;
        const contents = arg[2].val as string;

        const bad = firstNonByte(contents);
        if (bad >= 0) {
            getRuntimeObject().$t.threadError(
                `writeFileBytes: argument is not a byte string: character ${bad} is ` +
                `code unit ${contents.charCodeAt(bad)}, above 255`);
            return;
        }

        const r = resolveInSandbox(origPath);
        if (!r.ok) {
            return getRuntimeObject().ret(mkErr(r.reason, origPath));
        }
        suspendWithResult(async () => {
            try {
                await fs.promises.writeFile(r.resolved, Buffer.from(contents, 'latin1'));
                return mkOk(new LVal(__unitbase, ROOT));
            } catch (e) {
                return mkErr(errMessage(e), origPath);
            }
        });
    }, 'writeFileBytes'),

    appendFile: mkBase((larg) => {
        assertIsNTuple(larg, 3);
        getRuntimeObject().$t.raiseCurrentThreadPC(larg.lev);
        const arg = larg.val;
        assertIsRootAuthority(arg[0]);
        assertIsString(arg[1]);
        assertIsString(arg[2]);
        const origPath = arg[1].val;
        const contents = arg[2].val;

        const r = resolveInSandbox(origPath);
        if (!r.ok) {
            return getRuntimeObject().ret(mkErr(r.reason, origPath));
        }
        suspendWithResult(async () => {
            try {
                await fs.promises.appendFile(r.resolved, contents, 'utf8');
                return mkOk(new LVal(__unitbase, ROOT));
            } catch (e) {
                return mkErr(errMessage(e), origPath);
            }
        });
    }, 'appendFile'),

    fileExists: mkBase((larg) => {
        assertIsNTuple(larg, 2);
        getRuntimeObject().$t.raiseCurrentThreadPC(larg.lev);
        const arg = larg.val;
        assertIsRootAuthority(arg[0]);
        assertIsString(arg[1]);
        const origPath = arg[1].val;

        const r = resolveInSandbox(origPath);
        if (!r.ok) {
            // A path outside the sandbox does not "exist" as far as programs can tell.
            return getRuntimeObject().ret(mkOk(new LVal(false, ROOT)));
        }
        suspendWithResult(async () => {
            try {
                await fs.promises.access(r.resolved, fs.constants.F_OK);
                return mkOk(new LVal(true, ROOT));
            } catch {
                return mkOk(new LVal(false, ROOT));
            }
        });
    }, 'fileExists'),

    // Entries of a directory as a list of {name, kind}. The kind comes from the directory
    // entry itself, so walking a tree costs one call per directory rather than one stat per
    // entry. Entry order is whatever the filesystem reports; callers that need a stable order
    // must sort.
    readDir: mkBase((larg) => {
        assertIsNTuple(larg, 2);
        getRuntimeObject().$t.raiseCurrentThreadPC(larg.lev);
        const arg = larg.val;
        assertIsRootAuthority(arg[0]);
        assertIsString(arg[1]);
        const origPath = arg[1].val;

        const r = resolveInSandbox(origPath);
        if (!r.ok) {
            return getRuntimeObject().ret(mkErr(r.reason, origPath));
        }
        suspendWithResult(async () => {
            try {
                const entries = await fs.promises.readdir(r.resolved, { withFileTypes: true });
                const items = entries.map(d => {
                    const rec = Record.mkRecord([
                        ['name', new LVal(d.name, ROOT)],
                        ['kind', new LVal(direntKind(d), ROOT)],
                    ]);
                    return new LVal(rec, ROOT);
                });
                return mkOk(new LVal(mkList(items), ROOT));
            } catch (e) {
                return mkErr(errMessage(e), origPath);
            }
        });
    }, 'readDir'),

    // Create a directory, including any missing parents. Succeeds on an existing directory.
    makeDir: mkBase((larg) => {
        assertIsNTuple(larg, 2);
        getRuntimeObject().$t.raiseCurrentThreadPC(larg.lev);
        const arg = larg.val;
        assertIsRootAuthority(arg[0]);
        assertIsString(arg[1]);
        const origPath = arg[1].val;

        const r = resolveInSandbox(origPath);
        if (!r.ok) {
            return getRuntimeObject().ret(mkErr(r.reason, origPath));
        }
        suspendWithResult(async () => {
            try {
                await fs.promises.mkdir(r.resolved, { recursive: true });
                return mkOk(new LVal(__unitbase, ROOT));
            } catch (e) {
                return mkErr(errMessage(e), origPath);
            }
        });
    }, 'makeDir'),

    // {kind, size, mtime} for a path; mtime in milliseconds since the epoch. Uses lstat, so a
    // symlink reports kind 'other' rather than the kind of its target — consistent with
    // readDir and with the sandbox's refusal to traverse symlinks out of the root.
    fileStat: mkBase((larg) => {
        assertIsNTuple(larg, 2);
        getRuntimeObject().$t.raiseCurrentThreadPC(larg.lev);
        const arg = larg.val;
        assertIsRootAuthority(arg[0]);
        assertIsString(arg[1]);
        const origPath = arg[1].val;

        const r = resolveInSandbox(origPath);
        if (!r.ok) {
            return getRuntimeObject().ret(mkErr(r.reason, origPath));
        }
        suspendWithResult(async () => {
            try {
                const s = await fs.promises.lstat(r.resolved);
                const rec = Record.mkRecord([
                    ['kind', new LVal(statKind(s), ROOT)],
                    ['size', new LVal(s.size, ROOT)],
                    ['mtime', new LVal(s.mtimeMs, ROOT)],
                ]);
                return mkOk(new LVal(rec, ROOT));
            } catch (e) {
                return mkErr(errMessage(e), origPath);
            }
        });
    }, 'fileStat'),

    // Delete a file. Directories are rejected by the filesystem (EPERM/EISDIR); there is no
    // recursive-delete primitive, deliberately.
    removeFile: mkBase((larg) => {
        assertIsNTuple(larg, 2);
        getRuntimeObject().$t.raiseCurrentThreadPC(larg.lev);
        const arg = larg.val;
        assertIsRootAuthority(arg[0]);
        assertIsString(arg[1]);
        const origPath = arg[1].val;

        const r = resolveInSandbox(origPath);
        if (!r.ok) {
            return getRuntimeObject().ret(mkErr(r.reason, origPath));
        }
        suspendWithResult(async () => {
            try {
                await fs.promises.unlink(r.resolved);
                return mkOk(new LVal(__unitbase, ROOT));
            } catch (e) {
                return mkErr(errMessage(e), origPath);
            }
        });
    }, 'removeFile'),
}
