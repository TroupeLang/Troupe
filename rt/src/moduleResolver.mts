'use strict'
import * as fs from 'node:fs'
import * as path from 'node:path'

// The runtime seed of content-addressed module resolution. A module's IR
// identity is "module:<hash>"; the compiled artifacts and diagnostics need to
// map that hash back to a location and a user-visible name. The dependencies
// file (<main>.deps.json, pointed to by the main program's __moduleDepsFile) is
// the single authority for that mapping (content-addressed-identity.md §3).
//
// This is a process-level singleton, populated once per dependencies file and
// merged (never cleared), so nested module loads and the serialization guard
// share one resolver.

type ResolverEntry = { path: string, name: string }

const resolverMap: Record<string, ResolverEntry> = Object.create(null)
const seededFiles: Set<string> = new Set()

// Seed the resolver from a dependencies file. Idempotent per file; entries from
// several files merge into one map.
export function seedModuleResolver(depsFile: string): void {
    if (seededFiles.has(depsFile)) return
    seededFiles.add(depsFile)
    const raw = fs.readFileSync(depsFile, 'utf8')
    const doc = JSON.parse(raw)
    for (const e of (doc.deps ?? [])) {
        resolverMap[e.hash] = { path: e.path, name: e.name }
    }
}

// Resolve a "module:<hash>" identity to its compiled artifact, under the given
// project root: <root>/<dir(path)>/out/<base(path)>.js, with the path taken
// from the seeded map (the hash cannot reconstruct a path on its own).
export function resolveModuleFile(root: string, libname: string): string {
    const hash = libname.slice("module:".length)
    const entry = resolverMap[hash]
    if (!entry) {
        throw new Error(
            `cannot resolve module ${moduleDisplayName(libname)}: no entry for hash `
            + `${hash} in the dependencies file`)
    }
    return path.join(root, path.dirname(entry.path), "out", path.basename(entry.path) + ".js")
}

// Render a "module:<hash>" identity as "module:<name>" for diagnostics, using
// the seeded map; returns the input unchanged if it is not a module identity or
// the hash is unknown.
export function moduleDisplayName(libname: string): string {
    if (!libname.startsWith("module:")) return libname
    const hash = libname.slice("module:".length)
    const entry = resolverMap[hash]
    return entry ? "module:" + entry.name : libname
}
