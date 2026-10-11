import { fileURLToPath } from 'node:url';
import { dirname, resolve } from 'node:path';
import { existsSync } from 'node:fs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// From rt/built/hosts/node/ or rt/src/hosts/node/, go up to repo root
const selfLocatedRoot = resolve(__dirname, '../../../..');

function getSelfLocatedRoot(): string | null {
    // Check for .troupe-root marker file
    const markerPath = resolve(selfLocatedRoot, '.troupe-root');
    if (existsSync(markerPath)) {
        return selfLocatedRoot;
    }
    return null;
}

let cachedRoot: string | null = null;

export function getTroupeRoot(): string {
    if (cachedRoot) return cachedRoot;

    // 1. Try self-location first (for worktree support)
    const selfRoot = getSelfLocatedRoot();
    if (selfRoot) {
        cachedRoot = selfRoot;
        return selfRoot;
    }

    // 2. Fall back to TROUPE env var
    if (process.env.TROUPE) {
        cachedRoot = process.env.TROUPE;
        return process.env.TROUPE;
    }

    throw new Error(
        'Cannot determine Troupe home folder. ' +
        'Set the TROUPE environment variable or run from a valid Troupe installation.'
    );
}
