// Node's compression: zlib.

import { gzipSync, gunzipSync } from 'node:zlib';
import { Compression, DecompressionLimitError } from '../../Compression.mjs';

export const nodeCompression: Compression = {
    gzip(data: Uint8Array): Uint8Array {
        return gzipSync(data);
    },

    gunzip(data: Uint8Array, maxOutputLength: number): Uint8Array {
        try {
            return gunzipSync(data, { maxOutputLength });
        } catch (e) {
            if (e.code === 'ERR_BUFFER_TOO_LARGE') throw new DecompressionLimitError();
            throw e;
        }
    },
};
