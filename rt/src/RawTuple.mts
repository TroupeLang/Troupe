import { Level } from './Level.mjs';
import { LVal, listStringRep } from './Lval.mjs';
import { TroupeAggregateRawValue } from './TroupeRawValue.mjs';
import { TroupeType } from './TroupeTypes.mjs';
import * as levels from './options.mjs'

export class RawTuple extends Array<LVal> implements TroupeAggregateRawValue {
  dataLevel: Level;
  _troupeType = TroupeType.TUPLE;
  isTuple = true;
  stringRep = null;

  constructor(x: LVal[]) {
    super(...x)
    this.stringRep = function (omitLevels = false, taintRef = null) {
      return ("(" + listStringRep(x, omitLevels, taintRef) + ")");
    };

    let dataLevels = x.map(lv => lv.dataLevel);
    this.dataLevel = levels.lubs.call(null, dataLevels);
  }
}
