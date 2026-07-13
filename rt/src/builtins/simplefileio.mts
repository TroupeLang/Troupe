import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import { Record } from '../Record.mjs';
import { ROOT } from '../Level.mjs';
import { assertIsNTuple, assertIsRootAuthority, assertIsString } from '../Asserts.mjs'
import { __unitbase } from '../UnitBase.mjs';
import { getCliArgs, TroupeCliArg } from '../TroupeCliArgs.mjs';
import * as fs from 'node:fs';
import * as path from 'node:path';
import * as os from 'node:os';

/**
 * SimpleFileIO — placeholder whole-file I/O runtime primitive.
 *
 * Security design (see _dev_planning/tier2-libraries/spec-simple-file-io.md):
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

// Resolve the sandbox root once at module load.
//  - `--io-root <dir>`: created if missing, then realpath'd.
//  - unset: a per-invocation scratch dir under the OS temp dir. Its name never appears in any
//    observable output (errors echo the caller's relative path), so tests stay hermetic and
//    golden-stable without passing any runtime flag.
const ioRoot: string = (() => {
    const argv = getCliArgs();
    const configured = argv[TroupeCliArg.IoRoot];
    if (configured) {
        fs.mkdirSync(configured, { recursive: true });
        return fs.realpathSync(configured);
    }
    return fs.realpathSync(fs.mkdtempSync(path.join(os.tmpdir(), 'troupe-io-')));
})();

function withinRoot(p: string): boolean {
    return p === ioRoot || p.startsWith(ioRoot + path.sep);
}

type Resolved = { ok: boolean; resolved: string; reason: string };

/**
 * Resolve a caller path against the io-root and reject any escape. Checks the lexical resolution
 * (handles `..` and absolute paths) and then the realpath of the nearest existing ancestor
 * (handles symlink escapes for both existing and not-yet-created targets).
 */
function resolveInSandbox(p: string): Resolved {
    const resolved = path.resolve(ioRoot, p);
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
            return { ok: false, resolved: '', reason: e instanceof Error ? e.message : String(e) };
        }
    }
}

function errMessage(e: unknown): string {
    if (e && typeof e === 'object' && 'code' in e) {
        const code = (e as any).code;
        if (code === 'ENOENT') return 'file not found';
        if (code === 'EISDIR') return 'path is a directory';
        if (code === 'EACCES' || code === 'EPERM') return 'permission denied';
    }
    return e instanceof Error ? e.message : String(e);
}

export function BuiltinSimpleFileIO<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        // Result construction. Every leaf is labeled at ROOT: read content is fully trusted
        // (only ROOT code reaches this API), matching `persist`'s stance on restored data.
        private mkOk(value: LVal): LVal {
            const rec = Record.mkRecord([
                ['tag', new LVal('Ok', ROOT)],
                ['value', value],
            ]);
            return new LVal(rec, ROOT);
        }

        private mkErr(reason: string, pathStr: string): LVal {
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
        private suspendWithResult(build: () => Promise<LVal>) {
            const theThread = this.runtime.$t;
            (async () => {
                let res: LVal;
                try {
                    res = await build();
                } catch (e) {
                    // build() only throws on programming errors; surface as a thread error.
                    theThread.throwInSuspended('SimpleFileIO internal error: ' + errMessage(e));
                    this.runtime.__sched.scheduleThread(theThread);
                    this.runtime.__sched.resumeLoopAsync();
                    return;
                }
                theThread.returnSuspended(res);
                this.runtime.__sched.scheduleThread(theThread);
                this.runtime.__sched.resumeLoopAsync();
            })();
        }

        readFile = mkBase((larg) => {
            assertIsNTuple(larg, 2);
            this.runtime.$t.raiseCurrentThreadPC(larg.lev);
            const arg = larg.val;
            assertIsRootAuthority(arg[0]);
            assertIsString(arg[1]);
            const origPath = arg[1].val;

            const r = resolveInSandbox(origPath);
            if (!r.ok) {
                return this.runtime.ret(this.mkErr(r.reason, origPath));
            }
            this.suspendWithResult(async () => {
                try {
                    const content = await fs.promises.readFile(r.resolved, 'utf8');
                    return this.mkOk(new LVal(content, ROOT));
                } catch (e) {
                    return this.mkErr(errMessage(e), origPath);
                }
            });
        }, 'readFile');

        writeFile = mkBase((larg) => {
            assertIsNTuple(larg, 3);
            this.runtime.$t.raiseCurrentThreadPC(larg.lev);
            const arg = larg.val;
            assertIsRootAuthority(arg[0]);
            assertIsString(arg[1]);
            assertIsString(arg[2]);
            const origPath = arg[1].val;
            const contents = arg[2].val;

            const r = resolveInSandbox(origPath);
            if (!r.ok) {
                return this.runtime.ret(this.mkErr(r.reason, origPath));
            }
            this.suspendWithResult(async () => {
                try {
                    await fs.promises.writeFile(r.resolved, contents, 'utf8');
                    return this.mkOk(new LVal(__unitbase, ROOT));
                } catch (e) {
                    return this.mkErr(errMessage(e), origPath);
                }
            });
        }, 'writeFile');

        appendFile = mkBase((larg) => {
            assertIsNTuple(larg, 3);
            this.runtime.$t.raiseCurrentThreadPC(larg.lev);
            const arg = larg.val;
            assertIsRootAuthority(arg[0]);
            assertIsString(arg[1]);
            assertIsString(arg[2]);
            const origPath = arg[1].val;
            const contents = arg[2].val;

            const r = resolveInSandbox(origPath);
            if (!r.ok) {
                return this.runtime.ret(this.mkErr(r.reason, origPath));
            }
            this.suspendWithResult(async () => {
                try {
                    await fs.promises.appendFile(r.resolved, contents, 'utf8');
                    return this.mkOk(new LVal(__unitbase, ROOT));
                } catch (e) {
                    return this.mkErr(errMessage(e), origPath);
                }
            });
        }, 'appendFile');

        fileExists = mkBase((larg) => {
            assertIsNTuple(larg, 2);
            this.runtime.$t.raiseCurrentThreadPC(larg.lev);
            const arg = larg.val;
            assertIsRootAuthority(arg[0]);
            assertIsString(arg[1]);
            const origPath = arg[1].val;

            const r = resolveInSandbox(origPath);
            if (!r.ok) {
                // A path outside the sandbox does not "exist" as far as programs can tell.
                return this.runtime.ret(this.mkOk(new LVal(false, ROOT)));
            }
            this.suspendWithResult(async () => {
                try {
                    await fs.promises.access(r.resolved, fs.constants.F_OK);
                    return this.mkOk(new LVal(true, ROOT));
                } catch {
                    return this.mkOk(new LVal(false, ROOT));
                }
            });
        }, 'fileExists');
    }
}
