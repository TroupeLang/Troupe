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
//   gzip         : text  -> bytes            base64Encode : bytes -> ascii
//   gunzip       : bytes -> Result text      base64Decode : ascii -> Result bytes
//
// **Byte strings.** Troupe has no byte arrays, so the intermediate values are
// strings in which every character is one byte: code units 0..255, as `latin1`
// in Node's terms. `gzip` produces one and `base64Encode` consumes one. A
// string carrying anything above 255 is not a byte string, and passing one is
// an error rather than a silent truncation -- the convention is only safe if
// breaking it is loud.
//
// **Two kinds of failure, told apart.** The two directions that consume
// untrusted data -- a blob from a remote node -- return `{tag = "Ok", value}` /
// `{tag = "Err", error = {reason}}`, wrapped as an outcome by lib/BytesAndZips.
// Troupe has no exception handling, so a thread error would be unrecoverable,
// and bad mobile code is data rather than a defect. A malformed *argument*
// stays a thread error: a string that is not bytes, or one with no UTF-8
// encoding, is a mistake in the calling program, and nothing sensible can be
// returned for it.
//
// All four carry the label of their input and neither raise nor lower it: they
// are pure functions of the string, and nothing about compression declassifies.
// That includes the tag, which says whether the input decoded and is therefore
// as sensitive as the input.

import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import { Record } from '../Record.mjs';
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

// Every leaf carries the argument's label, so a result says exactly as much as
// the input it was derived from.
function mkOk(value: string, lev): LVal {
    return new LVal(Record.mkRecord([
        ['tag', new LVal('Ok', lev)],
        ['value', new LVal(value, lev)],
    ]), lev);
}

function mkErr(reason: string, lev): LVal {
    const errRec = Record.mkRecord([['reason', new LVal(reason, lev)]]);
    return new LVal(Record.mkRecord([
        ['tag', new LVal('Err', lev)],
        ['error', new LVal(errRec, lev)],
    ]), lev);
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
            // The gzip header's OS field (offset 9) is whichever platform this
            // Node's zlib was built for -- 19 on macOS, 3 on Linux. It is the
            // only byte of the output not determined by the argument, so
            // without this a program that gzips public data and sends it also
            // tells the recipient what it is running on. 255 is RFC 1952's
            // "unknown"; nothing reads the field, and DecompressionStream is
            // required to ignore it. Same reasoning and the same value as the
            // compiler's own producer, IRBlob.setGzipOS.
            if (out.length > 9) out[9] = 255;
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
                return this.runtime.ret(mkErr(
                    e.code === 'ERR_BUFFER_TOO_LARGE'
                    ? `decompressed data exceeds the ${maxDecompressedBytes}-byte cap`
                    : e.message,
                    arg.lev));
            }
            try {
                return this.runtime.ret(mkOk(utf8Decoder.decode(raw), arg.lev));
            } catch (e) {
                return this.runtime.ret(mkErr("decompressed data is not UTF-8", arg.lev));
            }
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
                return this.runtime.ret(mkErr("not base64", arg.lev));
            }
            return this.runtime.ret(
                mkOk(Buffer.from(s, 'base64').toString('latin1'), arg.lev));
        }, "base64Decode")
    }
}
