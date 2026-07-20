import { Level } from './Level.mjs';
import { LVal, listStringRep } from './Lval.mjs';
import { TroupeAggregateRawValue } from './TroupeRawValue.mjs';
import { TroupeType } from './TroupeTypes.mjs';
import * as levels from './Level.mjs'

/**
 * A flagged (syntactic-variant) tuple is well-formed iff it has arity 1 (a
 * nullary constructor: tag only) or 2 (an applied constructor: tag, payload),
 * with slot 0 holding the string tag. This is the one definition of the flagged
 * shape, shared by the deserialization boundary (deserialize.mts, which rejects
 * a malformed inbound shape as corrupt data) and the printer below (which falls
 * back to plain-tuple rendering for a malformed shape), so neither side can
 * drift from the other.
 */
export function isWellFormedSynVariant(x: LVal[]): boolean {
  return (x.length === 1 || x.length === 2) && typeof x[0].val === 'string';
}

export class RawTuple extends Array<LVal> implements TroupeAggregateRawValue {
  dataLevel: Level;
  _troupeType = TroupeType.TUPLE;
  isTuple = true;
  stringRep = null;
  _isSynVariant: boolean;

  constructor(x: LVal[], isSynVariant: boolean = false) {
    super(...x)
    this._isSynVariant = isSynVariant;
    this.stringRep = function (omitLevels = false, taintRef = null) {
      // A flagged tuple of an unexpected shape falls through to plain-tuple
      // rendering rather than crashing on a missing slot 0 or silently dropping
      // payload slots.
      if (isSynVariant && isWellFormedSynVariant(x)) {
        // slot 0 holds the tag "<hash>#<datatype>#<ctor>"; display the
        // segment after the last '#'. A nullary constructor is a 1-tuple
        // (tag only) and prints as the bare name; an applied constructor is
        // a 2-tuple (tag, payload) and prints as "(name payload)".
        let tag = x[0].val.toString();
        let name = tag.substring(tag.lastIndexOf('#') + 1);
        if (x.length === 2) {
          return ("(" + name + " " + x[1].stringRep(omitLevels, taintRef) + ")");
        }
        return name;
      }
      return ("(" + listStringRep(x, omitLevels, taintRef) + ")");
    };

    let dataLevels = x.map(lv => lv.dataLevel);
    this.dataLevel = levels.lub(...dataLevels);
  }
}
