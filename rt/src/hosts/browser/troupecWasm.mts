/// <reference lib="webworker" />
// The Troupe compiler as a WebAssembly module, driven through a WASI shim.
//
// bin/troupec.wasm is a WASI command module: its only imports are
// wasi_snapshot_preview1 and it exports _start. A run is a fresh instance over
// a filesystem made of JavaScript objects: the source at the root, an empty
// out/ for the stage dumps `-v` writes, and the standard library's interface
// files under /troupe/lib/out so that `import <Library>` resolves. TROUPE
// names that root: under WASI the compiler cannot locate itself and falls
// through to the variable (compiler/src/ProcessImports.hs).

import {
    WASI, File, Directory, OpenFile, PreopenDirectory,
} from '@bjorn3/browser_wasi_shim';

import { STAGES } from './stages.mjs';
export { STAGES };

export interface RunResult {
    code: number;
    /** Set when the module trapped or the shim threw, instead of exiting. */
    crash?: string;
    stdout: string;
    stderr: string;
    /** Every regular file left under the root, by path, as text. */
    files: Map<string, string>;
    ms: number;
}

export interface CompileOptions {
    sourceMap?: boolean;
}

export interface CompileResult {
    ok: boolean;
    code: number;
    crash?: string;
    js: string;
    exports: string;
    stages: Map<string, string>;
    log: string;
    errors: string;
    ms: number;
}

/** One module of a compiled project: its code and the names the
 *  dependencies file gives it. */
export interface CompiledModule {
    code: string;
    path: string;
    name: string;
}

/** A compiled project: the main program's artifact and every module it
 *  imports, by content hash, as the runtime links them. */
export interface ProjectResult extends CompileResult {
    /** The dependencies file the compiler established, parsed. */
    deps: { deps: Array<{ path: string; hash: string; name: string }> };
    modules: Record<string, CompiledModule>;
}

export interface LinkedBlob {
    code: string;
    sourceMap?: any;
    /** The compiler's response for the blob was not JSON; `code` holds it. */
    malformed?: boolean;
}

export interface LinkResult {
    ok: boolean;
    code: number;
    crash?: string;
    snippets: LinkedBlob[];
    errors: string;
    ms: number;
}

const encoder = new TextEncoder();
const decoder = new TextDecoder();

/** Every regular file under a directory, by path, as text. */
function filesUnder(dir: Directory, prefix = ''): Map<string, string> {
    const out = new Map<string, string>();
    for (const [name, inode] of dir.contents) {
        const path = prefix ? `${prefix}/${name}` : name;
        if (inode instanceof Directory) {
            for (const [k, v] of filesUnder(inode, path)) out.set(k, v);
        } else if ((inode as File).data) {
            out.set(path, decoder.decode((inode as File).data));
        }
    }
    return out;
}

export class TroupecWasm {
    /**
     * @param module the compiled bin/troupec.wasm
     * @param interfaces the interface files the compiler reads for `import`
     *        and `require native`, each by its path under the Troupe root:
     *        lib/out/<Name>.exports and ffi/<Name>.exports. A bare file name
     *        is taken as lib/out/<name>.
     */
    constructor(
        private readonly module: WebAssembly.Module,
        private readonly interfaces: Array<[string, Uint8Array]>,
    ) {}

    static async fromBytes(bytes: BufferSource, interfaces: Array<[string, Uint8Array]>): Promise<TroupecWasm> {
        return new TroupecWasm(await WebAssembly.compile(bytes), interfaces);
    }

    /** Runs the compiler once over a filesystem built for this request. */
    async run(args: string[], { files = [], stdin = '' }: {
        files?: Array<[string, string | Uint8Array]>; stdin?: string;
    } = {}): Promise<RunResult> {
        const root = new Map<string, any>();
        // A path with directories is placed in them, created as needed, so a
        // program's modules sit beside it as they do on disk.
        const place = (path: string, file: any) => {
            const parts = path.split('/').filter((p) => p !== '' && p !== '.');
            let dir = root;
            for (const p of parts.slice(0, -1)) {
                if (!dir.has(p)) dir.set(p, new Directory(new Map()));
                dir = dir.get(p).contents;
            }
            dir.set(parts[parts.length - 1], file);
        };
        for (const [name, text] of files) {
            place(name, new File(typeof text === 'string' ? encoder.encode(text) : text));
        }
        // The compiler writes its stage dumps to out/ and does not create it;
        // natively its absence is a write failure rather than a diagnostic.
        root.set('out', new Directory(new Map()));
        for (const [name, data] of this.interfaces) {
            place(name.includes('/') ? `troupe/${name}` : `troupe/lib/out/${name}`, new File(data));
        }

        const stdoutFile = new File([]);
        const stderrFile = new File([]);
        const fds = [
            new OpenFile(new File(encoder.encode(stdin))),
            new OpenFile(stdoutFile),
            new OpenFile(stderrFile),
            new PreopenDirectory('/', root),
        ];

        // PWD mirrors what the toolchain's own runner sets, so relative paths
        // resolve. debug:false: the shim traces every syscall otherwise.
        const wasi = new WASI(['troupec', ...args], ['TROUPE=/troupe', 'PWD=/'], fds, { debug: false });
        const instance = await WebAssembly.instantiate(this.module, {
            wasi_snapshot_preview1: wasi.wasiImport,
        });

        const started = performance.now();
        let code: number;
        try {
            code = wasi.start(instance as any);
        } catch (e) {
            return { code: -1, crash: String(e), stdout: '', stderr: '', files: new Map(), ms: 0 };
        }
        const ms = performance.now() - started;

        return {
            code,
            stdout: decoder.decode(stdoutFile.data),
            stderr: decoder.decode(stderrFile.data),
            files: filesUnder(new Directory(root)),
            ms,
        };
    }

    /** Compiles Troupe source: the emitted JavaScript, its exports and every
     *  intermediate stage. */
    /**
     * Compiles a program. With `sourceMap`, the emitted code carries source
     * positions (troupec -m), which an error report at run time names
     * together with the source line.
     */
    async compileSource(source: string, options: CompileOptions = {}): Promise<CompileResult> {
        const args = ['-v', ...(options.sourceMap ? ['-m'] : []), 'p.trp', '-o', 'p.js'];
        const r = await this.run(args, { files: [['p.trp', source]] });
        return this.compileResult(r);
    }

    /**
     * Compiles a project: the files by their paths, `main` among them. The
     * modules the main file imports are compiled first and their hashes
     * recorded in the dependencies file (troupec --update-deps), which is
     * re-established on every compile; the result carries each module's
     * code by its hash, as the runtime links it.
     */
    async compileProject(files: Array<[string, string]>, main: string, options: CompileOptions = {}): Promise<ProjectResult> {
        // --update-deps compiles the modules and writes the dependencies
        // file, and no program; the program is then compiled against it.
        const depsPath = main.replace(/\.trp$/, '') + '.deps.json';
        const pins = await this.run(['--update-deps', main], { files });
        if (pins.code !== 0 || pins.crash) {
            return { ...this.compileResult(pins), deps: { deps: [] }, modules: {} };
        }
        const depsText = pins.files.get(depsPath) ?? '{"deps": []}';
        const args = ['-v', ...(options.sourceMap ? ['-m'] : []), main, '-o', 'p.js'];
        const r = await this.run(args, { files: [...files, [depsPath, depsText]] });
        const result = this.compileResult(r);
        result.ms += pins.ms;
        const deps = JSON.parse(depsText);
        const modules: Record<string, CompiledModule> = {};
        for (const d of deps.deps) {
            const slash = d.path.lastIndexOf('/');
            const dir = slash < 0 ? '' : d.path.slice(0, slash + 1);
            const base = slash < 0 ? d.path : d.path.slice(slash + 1);
            const code = r.files.get(`${dir}out/${base}.js`);
            if (code !== undefined) modules[d.hash] = { code, path: d.path, name: d.name };
        }
        return { ...result, deps, modules };
    }

    private compileResult(r: RunResult): CompileResult {
        const stages = new Map<string, string>();
        for (const [file, label] of STAGES) {
            const text = r.files.get(`out/${file}`);
            if (text !== undefined) stages.set(label, text);
        }
        return {
            ok: r.code === 0,
            code: r.code,
            crash: r.crash,
            js: r.files.get('p.js') ?? '',
            exports: r.files.get('p.exports') ?? '',
            stages,
            log: r.stdout,
            errors: r.stderr,
            ms: r.ms,
        };
    }

    /**
     * Relinks serialized IR blobs, the way a node does when a closure arrives:
     * the --json-ir protocol of rt/src/deserialize.mts, one base64 blob per
     * line, the batch bracketed by an "!ECHO <marker>" line the compiler
     * echoes back, and one JSON `{code, sourceMap?}` per blob in return.
     */
    async linkBlobs(blobs: string[]): Promise<LinkResult> {
        const marker = '/*-----*/';
        const stdin = `${blobs.map((b) => b.trim()).join('\n')}\n!ECHO ${marker}\n`;
        const r = await this.run(['--json-ir'], { stdin });

        // Responses are separated by blank lines; the echoed marker ends the batch.
        const parts = r.stdout.split('\n\n').map((s) => s.trim()).filter(Boolean);
        const snippets: LinkedBlob[] = [];
        for (const part of parts) {
            if (part === marker) break;
            try {
                snippets.push(JSON.parse(part));
            } catch {
                snippets.push({ code: part, malformed: true });
            }
        }
        return { ok: r.code === 0, code: r.code, crash: r.crash, snippets, errors: r.stderr, ms: r.ms };
    }
}
