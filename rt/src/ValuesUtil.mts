import { LVal, listStringRep} from './Lval.mjs'
import { RawList } from './RawList.mjs'
import { RawTuple } from './RawTuple.mjs'

export function isListFlagSet (x:any) {  
  return (x.isList == true )
}

export function isTupleFlagSet (x:any) {
  return (x.isTuple == true)
}

/**
 * Takes an array of labelled values and makes a new Troupe tuple object out of it.
 */
export function mkTuple(x: LVal[]) {
  return new RawTuple(x)
}


/**
 * Takes an array of labelled values and makes a new Troupe list object out of it.
 */
export function mkList(a: LVal[]) {
  return RawList.fromArray(a);
}
