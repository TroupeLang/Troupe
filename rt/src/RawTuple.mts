import { Level } from './Level.mjs';
import { LVal, listStringRep } from './Lval.mjs';
import { TroupeAggregateRawValue } from './TroupeRawValue.mjs';
import { TroupeType } from './TroupeTypes.mjs';
import * as levels from './Level.mjs'

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
      if (isSynVariant) {
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
