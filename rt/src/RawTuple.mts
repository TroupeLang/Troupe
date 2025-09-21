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
  _isADT: boolean;  

  constructor(x: LVal[], isADT: boolean) {
    super(...x)
    this._isADT = isADT;  
    this.stringRep = function (omitLevels = false, taintRef = null) {
	if (this._isADT) {
	    if (this.length === 2) {
		let tag = this[0].val.toString()
		let val = this[1].stringRep(omitLevels, taintRef)
		return "(" + tag + " " + val + ")"
	    } else {
		return this[0].val.toString()
	    }
	} else {
	    return ("(" + listStringRep(x, omitLevels, taintRef) + ")");
	}
    };

    let dataLevels = x.map(lv => lv.dataLevel);
    this.dataLevel = levels.lubs.call(null, dataLevels);
  }
}
