'use strict'
import * as fs from 'node:fs'
import * as path from 'node:path'
import * as levels from './Level.mjs';
const { readFile } = fs.promises

import { getTroupeRoot } from './troupeRoot.mjs'
import { seedModuleResolver, resolveModuleFile, moduleDisplayName } from './moduleResolver.mjs'
import { mkLogger } from './logger.mjs'
const logger = mkLogger('lib')

const info = x => logger.info(x)
const debug = x => logger.debug(x)

// A loaded library: its exported value functions keyed by declaration name,
// and the exported datatype group hashes it embeds (this.__datatypeHashes, set
// by the compiler for library builds).
type LoadedLib = { values: Record<string, any>, datatypeHashes: string[] }

// Memoized per-process library loader. Each library is read from disk,
// instantiated, its own dependencies linked, and its export table computed
// exactly once; the in-flight promise is cached so concurrent requests share
// one instantiation. Both value linking and the datatype skew check go through
// here, so a library is never loaded or instantiated more than once.
const __libcache: Record<string, Promise<LoadedLib>> = {}

function loadLib(libname: string, rtObj, root): Promise<LoadedLib> {
    if (__libcache[libname]) {
        debug('lib cache hit on: ' + libname)
        return __libcache[libname]
    }
    const p = (async (): Promise<LoadedLib> => {
        // Find the file. Libraries load from the standard location; a "module:"
        // identity names a compiled module addressed by its content hash and
        // resolved to <root>/<dir>/out/<Base>.js through the dependencies-file
        // seed (its path cannot be reconstructed from the hash alone). This path
        // serves both the running program's own modules and the modules a
        // received closure references: the resolver answers against the
        // receiver's dependencies, so a closure links only modules the receiver
        // already has, matched by hash.
        let filename
        if (libname.startsWith("module:")) {
            filename = resolveModuleFile(root, libname)
            if (filename == null) {
                // Not locally discoverable — this program records no
                // dependencies, or none of them has this hash. A received
                // module-bearing closure naming a module the receiver does not
                // have is cleanly rejected here (all-or-nothing: it never links).
                throw new Error(
                    `cannot link module ${moduleDisplayName(libname)}: it is not `
                    + `among this program's dependencies`)
            }
        } else {
            filename = getTroupeRoot() + "/lib/out/" + libname + ".js"
        }
        let input = await readFile(filename, 'utf8')
        // TODO: check for error! 2018-07-03: aa
        let Lib: any = new Function('rt', input)
        let libinstance = new Lib(rtObj)

        // Link this library's own dependencies before computing its table.
        // A module artifact inherits the project root through this recursion
        // (an artifact recording its own __moduleRoot overrides it).
        await loadLibsAsync(libinstance, rtObj, root)

        // export() is a compiler-generated function returning the value table;
        // libload mode is the runtime's hack for evaluating it (2019-01-03: AA).
        rtObj.setLibloadMode()
        let table = libinstance.export({__dataLevel:levels.BOT}).val.toArray()
        rtObj.setNormalMode()

        let values: Record<string, any> = Object.create(null)
        for (let i = 0; i < table.length; i++) {
            let name = table[i].val[0].val
            if (!(name in values)) values[name] = table[i].val[1].val
        }
        return { values, datatypeHashes: libinstance.__datatypeHashes || [] }
    })()
    __libcache[libname] = p
    return p
}

export async function loadLibsAsync(obj, rtObj, moduleRoot = null) {
    let libs = obj.libs
    obj.libs = {}
    // The main program records its project root and dependencies file when it
    // uses modules; module artifacts inherit the root through the recursion, and
    // the resolver map (a process-level singleton) is seeded once from the
    // dependencies file the main program points at.
    const root = obj.__moduleRoot ?? moduleRoot
    if (obj.__moduleDepsFile) seedModuleResolver(obj.__moduleDepsFile)
    for (let n = 0; n < libs.length; n++) {
        let lib = libs[n].lib
        let decl = libs[n].decl
        let loaded = await loadLib(lib, rtObj, root)
        if (decl in loaded.values) {
            obj.libs[lib + "." + decl] = loaded.values[decl]
        }
    }

    // Datatype version-skew check (normalization.md §10). Enforced here, in the
    // single universal library-load path, but kept as a self-contained pass so
    // it is easy to locate and reason about independently of value linking.
    await checkConsumedDatatypes(obj, rtObj, root)
}

// Verify that every datatype group hash the artifact consumed at compile time
// is still among the loaded library's exported group hashes (membership, not
// equality: library extension is harmless, a changed consumed group is not).
async function checkConsumedDatatypes(obj, rtObj, root) {
    let consumed = obj.__consumedDatatypeHashes
    if (!consumed) return
    for (let libname of Object.keys(consumed)) {
        let exported = (await loadLib(libname, rtObj, root)).datatypeHashes
        for (let h of consumed[libname]) {
            if (!exported.includes(h)) {
                const disp = moduleDisplayName(libname)
                throw new Error(
                    "datatype version skew: library '" + disp + "' no longer exports a "
                    + "datatype group that was consumed at compile time (group hash " + h
                    + "). The importer was compiled against a different version of '" + disp
                    + "'; recompile it against the current library.")
            }
        }
    }
}
