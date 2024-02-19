import {listStringRep} from './Lval.mjs'
import { RawList } from './RawList.mjs'
import { TroupeType } from './TroupeTypes.mjs'
import * as levels from './options.mjs'





export function isListFlagSet (x:any) {  
  return (x.isList == true )
}

export function isTupleFlagSet (x:any) {
  return (x.isTuple == true)
}

export function mkTuple(x) {
  x.stringRep = function (omitLevels = false, taintRef  = null) {
    return ("(" + listStringRep(x, omitLevels, taintRef) + ")")
  }
  x.isTuple = true;
  x._troupeType = TroupeType.TUPLE

  
  let dataLevels = x.map (lv => lv.dataLevel);
  x.dataLevel = levels.lubs.call(null, dataLevels);  
  return x;
}


export function mkList(x) {
  return RawList.fromArray (x);  
}
