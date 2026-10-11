// Node's code store: artifacts come from the Troupe install tree.
//
// This holds the filesystem knowledge that used to sit inside the loader --
// the lib/out layout, the trp-rt service location, and content-addressed
// module resolution through the dependencies file.

import * as fs from 'node:fs';
import { resolve, isAbsolute } from 'node:path';
import { CodeStore, ArtifactNotLinkableError } from '../../CodeStore.mjs';
import { getTroupeRoot } from './troupeRoot.mjs';
import { seedModuleResolver, resolveModuleFile, moduleDisplayName } from './moduleResolver.mjs';

const { readFile } = fs.promises;

/**
 * Check if a path points to a regular file (not a device, pipe, socket, etc.).
 * Returns false for special files like /dev/stdin that can't be re-read.
 */
function isRegularFile(path: string): boolean {
    try {
        return fs.statSync(path).isFile();
    } catch {
        return false;
    }
}

/**
 * Resolve a potentially relative file path to an absolute path.
 * Only returns paths to regular files (excludes devices, pipes, sockets).
 */
function resolveSourcePath(filePath: string): string | null {
    if (isAbsolute(filePath)) {
        return isRegularFile(filePath) ? filePath : null;
    }
    const cwdPath = resolve(process.cwd(), filePath);
    return isRegularFile(cwdPath) ? cwdPath : null;
}

export const nodeCodeStore: CodeStore = {
    seedModules(depsFile: string): void {
        seedModuleResolver(depsFile);
    },

    async readArtifact(libname: string, root: string | null): Promise<string> {
        let filename: string | null;
        if (libname.startsWith("module:")) {
            // A "module:" identity names a compiled module addressed by its
            // content hash, resolved to <root>/<dir>/out/<Base>.js through the
            // dependencies-file seed (its path cannot be reconstructed from the
            // hash alone). This path serves both the running program's own
            // modules and the modules a received closure references: the
            // resolver answers against the receiver's dependencies, so a closure
            // links only modules the receiver already has, matched by hash.
            filename = resolveModuleFile(root, libname);
            if (filename == null) {
                throw new ArtifactNotLinkableError(moduleDisplayName(libname));
            }
        } else {
            filename = getTroupeRoot() + "/lib/out/" + libname + ".js";
        }
        return await readFile(filename, 'utf8');
    },

    async loadService(): Promise<string> {
        return await readFile(getTroupeRoot() + '/trp-rt/out/service.js', 'utf8');
    },

    displayName(libname: string): string {
        return moduleDisplayName(libname);
    },

    readSource(filePath: string): string | null {
        const resolvedPath = resolveSourcePath(filePath);
        if (!resolvedPath) return null;
        try {
            return fs.readFileSync(resolvedPath, 'utf-8');
        } catch {
            return null;
        }
    },
};
