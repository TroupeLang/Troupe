'use strict'
import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs'
import { __unit } from '../UnitVal.mjs'
import { TroupeType, ClosureType } from '../TroupeTypes.mjs'
import archy from 'archy'
import chalk from 'chalk'
import { isColorEnabled } from '../colorConfig.mjs'

// ============================================================
// CONFIGURABLE COLOR SCHEME
// Easily modify these to change the debug output appearance
// ============================================================
type ColorFn = (s: string) => string

interface DebugValueColors {
    valueLabel: ColorFn    // For @<...> (security value label)
    typeLabel: ColorFn     // For %<...> (security type label)
    typeName: ColorFn      // For (record), (list), etc.
    fieldName: ColorFn     // For field names and indices
    stringValue: ColorFn   // For string literals
    numberValue: ColorFn   // For numbers
    specialValue: ColorFn  // For closures, capabilities, etc.
}

const DEFAULT_COLORS: DebugValueColors = {
    valueLabel: chalk.cyan,
    typeLabel: chalk.magenta,
    typeName: chalk.dim,
    fieldName: chalk.white,
    stringValue: chalk.green,
    numberValue: chalk.yellow,
    specialValue: chalk.gray,
}

// No-op colors when color is disabled
const NO_COLORS: DebugValueColors = {
    valueLabel: (s: string) => s,
    typeLabel: (s: string) => s,
    typeName: (s: string) => s,
    fieldName: (s: string) => s,
    stringValue: (s: string) => s,
    numberValue: (s: string) => s,
    specialValue: (s: string) => s,
}

function getColors(): DebugValueColors {
    return isColorEnabled() ? DEFAULT_COLORS : NO_COLORS
}
// ============================================================

function buildArchyTree(
    lval: LVal,
    depth: number,
    maxDepth: number,
    seen: Set<any>,
    c: DebugValueColors
): archy.Data {
    const rawVal = lval.val
    const troupeType = lval.troupeType
    const levStr = lval.lev.stringRep()
    const tlevStr = lval.tlev.stringRep()
    const labels = ` ${c.valueLabel('@' + levStr)} ${c.typeLabel('%' + tlevStr)}`

    // Depth limit check
    if (depth > maxDepth) {
        return { label: c.typeName('...') + labels }
    }

    // Handle each type
    switch (troupeType) {
        case TroupeType.UNIT:
            return { label: '()' + labels }

        case TroupeType.BOOLEAN:
            return { label: String(rawVal) + labels }

        case TroupeType.NUMBER:
            return { label: c.numberValue(String(rawVal)) + labels }

        case TroupeType.STRING:
            return { label: c.stringValue(`"${rawVal}"`) + labels }

        case TroupeType.LEVEL:
            return { label: rawVal.stringRep() + labels }

        case TroupeType.AUTHORITY:
            return { label: rawVal.stringRep() + labels }

        case TroupeType.PROCESS_ID:
            return { label: rawVal.stringRep() + labels }

        case TroupeType.CAPABILITY:
            return { label: c.specialValue('<capability>') + labels }

        case TroupeType.CLOSURE: {
            const closureType = rawVal._closureType
            let closureDesc: string
            if (closureType === ClosureType.BUILTINFN) {
                closureDesc = '<builtin fn>'
            } else if (closureType === ClosureType.SANDBOXKONT) {
                closureDesc = '<sandboxkont>'
            } else if (closureType === ClosureType.SERVICEFN) {
                closureDesc = '<service fn>'
            } else {
                closureDesc = 'fn => ..'
            }
            return { label: c.specialValue(closureDesc) + labels }
        }

        case TroupeType.LOCALOBJECT:
            return { label: c.specialValue('<localobject>') + labels }

        case TroupeType.RECORD: {
            if (seen.has(rawVal)) {
                return { label: c.typeName('(record)') + ' <circular>' + labels }
            }
            seen.add(rawVal)
            const entries = Array.from(rawVal.__obj.entries())
            return {
                label: c.typeName('(record)') + labels,
                nodes: entries.map(([k, v]) => ({
                    label: c.fieldName(k) + ': ',
                    nodes: [buildArchyTree(v as LVal, depth + 1, maxDepth, new Set(seen), c)]
                }))
            }
        }

        case TroupeType.LIST: {
            if (seen.has(rawVal)) {
                return { label: c.typeName('(list)') + ' <circular>' + labels }
            }
            seen.add(rawVal)
            const arr = rawVal.toArray()
            return {
                label: c.typeName(`(list) [${arr.length}]`) + labels,
                nodes: arr.map((elem: LVal, i: number) => ({
                    label: c.fieldName(`[${i}]`) + ': ',
                    nodes: [buildArchyTree(elem, depth + 1, maxDepth, new Set(seen), c)]
                }))
            }
        }

        case TroupeType.TUPLE: {
            if (seen.has(rawVal)) {
                return { label: c.typeName('(tuple)') + ' <circular>' + labels }
            }
            seen.add(rawVal)
            // Use Array.from to avoid RawTuple.map creating new RawTuple
            const tupleArr: LVal[] = Array.from(rawVal)
            return {
                label: c.typeName(`(tuple) (${tupleArr.length})`) + labels,
                nodes: tupleArr.map((elem: LVal, i: number) => ({
                    label: c.fieldName(`[${i}]`) + ': ',
                    nodes: [buildArchyTree(elem, depth + 1, maxDepth, new Set(seen), c)]
                }))
            }
        }

        default:
            return { label: `<unknown type ${troupeType}>` + labels }
    }
}

export function BuiltinDebugValue<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        debugValue = mkBase((arg: LVal) => {
            // Parse arguments: single value or (value, depth) tuple
            let valueToInspect: LVal = arg
            let maxDepth = Infinity

            if (arg.troupeType === TroupeType.TUPLE && arg.val.length === 2) {
                const second = arg.val[1]
                if (typeof second.val === 'number') {
                    valueToInspect = arg.val[0]
                    maxDepth = second.val
                }
            }

            // Build tree and print
            const colors = getColors()
            const tree = buildArchyTree(valueToInspect, 0, maxDepth, new Set(), colors)
            console.log('\n' + archy(tree))

            return this.runtime.ret(__unit)
        }, "debugValue")
    }
}
