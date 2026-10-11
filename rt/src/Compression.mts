// gzip for the codec builtins. The host supplies the implementation; the
// byte-level requirements -- the header's OS byte, the decompression cap --
// stay with the builtins (builtins/codec.mts).

/** The decompressed output exceeded the cap the caller set. */
export class DecompressionLimitError extends Error {
    constructor() {
        super('decompressed data exceeds the cap');
        this.name = 'DecompressionLimitError';
    }
}

export interface Compression {
    gzip(data: Uint8Array): Uint8Array;

    /** Throws DecompressionLimitError when the output would exceed
     *  maxOutputLength bytes, and whatever the implementation throws on
     *  malformed input. */
    gunzip(data: Uint8Array, maxOutputLength: number): Uint8Array;
}

let __compression: Compression | null = null;

export function registerCompression(c: Compression): void {
    __compression = c;
}

export function getCompression(): Compression {
    if (__compression == null) {
        throw new Error('no compression on this host');
    }
    return __compression;
}
