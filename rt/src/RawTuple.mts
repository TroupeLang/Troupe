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
  _isSynVariant: boolean;

  constructor(x: LVal[], isSynVariant: boolean = false) {
    super(...x)
    this._isSynVariant = isSynVariant;
    let dataLevels = x.map(lv => lv.dataLevel);
    this.dataLevel = levels.lub(...dataLevels);
  }

  // A prototype method, not a per-instance closure: everything it needs is
  // already on `this` (the tuple IS the element array), and tuples are
  // constructed on hot paths where a closure per value is a measured cost.
  stringRep(omitLevels = false, taintRef = null) {
    // A flagged tuple of an unexpected shape falls through to plain-tuple
    // rendering rather than crashing on a missing slot 0 or silently dropping
    // payload slots.
    if (this._isSynVariant && isWellFormedSynVariant(this)) {
      // slot 0 holds the tag "<hash>#<datatype>#<ctor>"; display the
      // segment after the last '#'. A nullary constructor is a 1-tuple
      // (tag only) and prints as the bare name; an applied constructor is
      // a 2-tuple (tag, payload) and prints as "(name payload)".
      let tag = this[0].val.toString();
      let name = tag.substring(tag.lastIndexOf('#') + 1);
      if (this.length === 2) {
        return ("(" + name + " " + this[1].stringRep(omitLevels, taintRef) + ")");
      }
      return name;
    }
    return ("(" + listStringRep(this, omitLevels, taintRef) + ")");
  }
}
