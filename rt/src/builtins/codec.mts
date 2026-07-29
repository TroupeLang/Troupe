'use strict'
//
// gzip and base64, the two primitives a Troupe program needs to read and write
// the serialized IR blob that carries mobile code.
//
// A blob is base64 of "TRPI", a version byte, and a gzip stream of the UTF-8
// encoded document (compiler/src/IRBlob.hs). Nothing here knows that: the
// framing is assembled and taken apart by Troupe code, so the format version
// has one home, in the compiler. These four functions are the mechanical
// layers under it.
//
//   gzip         : text  -> bytes      base64Encode : bytes -> ascii
//   gunzip       : bytes -> text       base64Decode : ascii -> bytes
//
// **Byte strings.** Troupe has no byte arrays, so the intermediate values are
// strings in which every character is one byte: code units 0..255, as `latin1`
// in Node's terms. `gzip` produces one and `base64Encode` consumes one. A
// string carrying anything above 255 is not a byte string, and passing one is
// an error rather than a silent truncation -- the convention is only safe if
// breaking it is loud.
//
// All four carry the label of their input and neither raise nor lower it: they
// are pure functions of the string, and nothing about compression declassifies.

import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import { assertIsString } from '../Asserts.mjs';
import { gzipSync, gunzipSync } from 'node:zlib';

// The cap the compiler enforces when decompressing a blob
// (IRBlob.maxDecompressedBytes). A stream from a remote node is untrusted
// input, and gunzip is where a decompression bomb would be paid for.
const maxDecompressedBytes = 64 * 1024 * 1024;

const utf8Decoder = new TextDecoder('utf-8', { fatal: true });

/** Index of the first character outside 0..255, or -1 if the string is bytes. */
function firstNonByte(s: string): number {
    for (let i = 0; i < s.length; i++) {
        if (s.charCodeAt(i) > 0xff) return i;
    }
    return -1;
}

export function BuiltinCodec<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        // A string holding an unpaired surrogate has no UTF-8 encoding: Node
        // substitutes U+FFFD for it, and the round trip would come back quietly
        // altered. Refused instead, for the same reason a non-byte string is.
        gzip = mkBase((arg) => {
            assertIsString(arg);
            const s = arg.val as string;
            if (!s.isWellFormed()) {
                this.runtime.$t.threadError(
                    "gzip: argument has no UTF-8 encoding: it contains an unpaired surrogate");
                return;
            }
            const out = gzipSync(Buffer.from(s, 'utf8'));
            return this.runtime.ret(new LVal(out.toString('latin1'), arg.lev));
        }, "gzip")

        gunzip = mkBase((arg) => {
            assertIsString(arg);
            const s = arg.val as string;
            const bad = firstNonByte(s);
            if (bad >= 0) {
                this.runtime.$t.threadError(
                    `gunzip: argument is not a byte string: character ${bad} is ` +
                    `code unit ${s.charCodeAt(bad)}, above 255`);
                return;
            }
            let raw: Buffer;
            try {
                raw = gunzipSync(Buffer.from(s, 'latin1'),
                                 { maxOutputLength: maxDecompressedBytes });
            } catch (e) {
                this.runtime.$t.threadError(
                    e.code === 'ERR_BUFFER_TOO_LARGE'
                    ? `gunzip: decompressed data exceeds the ${maxDecompressedBytes}-byte cap`
                    : `gunzip: ${e.message}`);
                return;
            }
            let text: string;
            try {
                text = utf8Decoder.decode(raw);
            } catch (e) {
                this.runtime.$t.threadError(`gunzip: decompressed data is not UTF-8`);
                return;
            }
            return this.runtime.ret(new LVal(text, arg.lev));
        }, "gunzip")

        base64Encode = mkBase((arg) => {
            assertIsString(arg);
            const s = arg.val as string;
            const bad = firstNonByte(s);
            if (bad >= 0) {
                this.runtime.$t.threadError(
                    `base64Encode: argument is not a byte string: character ${bad} is ` +
                    `code unit ${s.charCodeAt(bad)}, above 255`);
                return;
            }
            return this.runtime.ret(
                new LVal(Buffer.from(s, 'latin1').toString('base64'), arg.lev));
        }, "base64Encode")

        // Node's decoder ignores anything outside the alphabet, so a corrupt
        // blob would decode to plausible-looking bytes. Checked here instead:
        // whitespace is skipped, everything else must be base64.
        base64Decode = mkBase((arg) => {
            assertIsString(arg);
            const s = (arg.val as string).replace(/\s+/g, '');
            if (!/^[A-Za-z0-9+/]*={0,2}$/.test(s) || s.length % 4 !== 0) {
                this.runtime.$t.threadError("base64Decode: argument is not base64");
                return;
            }
            return this.runtime.ret(
                new LVal(Buffer.from(s, 'base64').toString('latin1'), arg.lev));
        }, "base64Decode")
    }
}
