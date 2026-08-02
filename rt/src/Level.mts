import {levels, Level, QuarantineOptions } from './levels/DCLabels/dclabel.mjs'
import { DowngradeResult, DowngradeKind, DowngradeDimension } from './DowngradeEnums.mjs'
import { getRuntimeObject } from './SysState.mjs'
export { mkLevel, Level, QuarantineOptions } from './levels/DCLabels/dclabel.mjs'


// import {levels } from './levels/tagsets.mjs'
// export { mkLevel, Level } from './levels/tagsets.mjs'

export function lub(...x) { return levels.lub (...x) }
export function glb(a,b)  { return levels.glb (a,b)  }
export function coalesce(a:Level, b:Level): Level { return a.coalesce(b) }
export function flowsTo (a:Level,b:Level) { return levels.flowsTo (a,b) }
export function actsFor (a:Level, b:Level, options?: QuarantineOptions) { return levels.actsFor(a, b, options) }
export function privFlowsTo (auth:Level, x:Level, y:Level): boolean { return levels.privFlowsTo(auth, x, y) }
export function okToDowngrade (kind: DowngradeKind, dimension: DowngradeDimension) {
    return levels.okToDowngrade(kind, dimension);
}
export function okToDeclassify (from: Level, to:Level, auth: Level, bl: Level, isNMIFC: boolean, pc?: Level): DowngradeResult {
    return levels.okToDeclassify (from, to, auth, bl, isNMIFC, pc);
}
export function okToEndorse (from: Level, to:Level, auth: Level, bl: Level, isNMIFC: boolean, pc?: Level): DowngradeResult {
    return levels.okToEndorse (from, to, auth, bl, isNMIFC, pc);
}
export function okToCrossDimensionalDowngrade (from: Level, to:Level, auth: Level, bl: Level, isNMIFC: boolean, pc?: Level): DowngradeResult {
    return levels.okToCrossDimensionalDowngrade (from, to, auth, bl, isNMIFC, pc);
}
export function fromSingleTag(x:string) { return levels.fromV1String(x)}

/** V1 label literals in generated code (`rt.mkV1Label`); reported on the
 *  running thread, which exists by construction. */
export function mkV1Level (x:string ) {
	try {
		return levels.fromV1String (x);
	} catch (e) {
		const thread = getRuntimeObject().__sched.__currentThread;
		thread.threadError(
			`Invalid V1 label \`{${x}}\`: ${e.message}\n` +
			`V1 labels use commas to separate principals (e.g., \`{alice, bob}\`).\n` +
			`For DC labels with separate confidentiality/integrity, use \`<...;...>\` syntax.`
		);
	}
}

/**
 * Parse a level supplied on the command line, in either surface syntax:
 * V1 `{alice, bob}` or V2 `<conf;integ>`.
 *
 * Flag levels are parsed at module load, before any thread exists, so a parse
 * failure cannot be reported as a thread error: it is a startup error, printed
 * and followed by a non-zero exit.
 */
export function levelFromFlag (flag: string, x: string): Level {
	try {
		return levels.fromString (x);
	} catch (e) {
		console.error(
			`Error: invalid level for --${flag}: \`${x}\`\n` +
			` | ${e.message}\n` +
			` | V1 labels use braces and commas: '{alice, bob}'\n` +
			` | V2 labels give both dimensions: '<alice;#root-integrity>'`
		);
		process.exit(1);
	}
}
export const BOT  = levels.BOT
export const TOP  = levels.TOP
export const ROOT = levels.ROOT
export const NULL = levels.NULL
