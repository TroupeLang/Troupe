import {TroupeAggregateRawValue} from './TroupeRawValue.mjs'
import {ClosureType, TroupeType} from './TroupeTypes.mjs'
import * as levels from './options.mjs' 
export function RawClosure (e, _t, f) : TroupeAggregateRawValue { 
  let closure:any = () => {    
    return f (e)
  }
  closure.env=e
  closure.namespace = _t 
  closure.fun = f
  closure._troupeType = TroupeType.CLOSURE
  closure._closureType = ClosureType.REGULARFN
  closure.stringRep  = (omitLevels = false ) => {
    return "fn => .." 
  }
  
  closure.dataLevel = e.__dataLevel
  closure.toString = () => {
    return ("[RawClosure]" + f.toString ())
  }
  return closure;  
}


export function SandboxResumption(f) : TroupeAggregateRawValue {
  let closure:any = () => {    
    return f ()
  }
  
  
  closure.fun = f
  closure._troupeType = TroupeType.CLOSURE
  closure._closureType = ClosureType.SANDBOXKONT
  closure.stringRep  = (omitLevels = false ) => {
    return "<sandboxkont>" 
  }
  
  closure.dataLevel = levels.BOT
  closure.toString = () => {
    return ("<sandboxkont>")
  }
  return closure;  
}