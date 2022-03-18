import {ClosureType, TroupeType} from './TroupeTypes'
import {TroupeAggregateRawValue} from './TroupeRawValue'
import levels from './options' 
import { getRuntimeObject } from './SysState'

export function BaseFunctionWithExplicitArg(f, name = null) : TroupeAggregateRawValue{
  
  let closure : any = () => {
    let thread = getRuntimeObject().$t;
    return f (thread.arg_as_lval);
  }
  closure.env = null;
  closure.fun = f  
  closure._troupeType = TroupeType.CLOSURE; 
  closure._closureType = ClosureType.BUILTINFN;
  closure.stringRep = () => {
    if (name) {
      return `<basefun:${name}>`
    } else {
      return "<basefun:_>"
    }
  }    
  closure.dataLevel = levels.BOT; 
  return closure;
}

  
export function ServiceFunction (f, name=null) : TroupeAggregateRawValue {  
  let closure : any = () => f ();
  closure.env = null;
  closure.fun = f  
  closure._troupeType = TroupeType.CLOSURE; 
  closure._closureType = ClosureType.SERVICEFN;
  closure.stringRep = () => {
    if (name) {
      return `<basefun:${name}>`
    } else {
      return "<basefun:_>"
    }
  }    
  closure.dataLevel = levels.BOT; 
  return closure;  
}