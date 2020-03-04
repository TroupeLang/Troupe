import {TroupeType, getTroupeType} from './TroupeTypes'
import {TLVal, LVal} from './Lval'
const proc = require('./process.js');
import levels  = require ('./options')



export function runtimeEquals (x:LVal, y:LVal) {  
  let t1 = getTroupeType(x.val)
  let t2 = getTroupeType(y.val)

  function baseBoolean (b:boolean, l=levels.lub (x.lev, y.lev)) {
    return new TLVal (b, l, levels.BOT) 
  }

  function levelEquality (o1, o2) {
    let b1 = levels.flowsTo (o1, o2)
    if (!b1)
      return baseBoolean (false);
    return baseBoolean(levels.flowsTo(o2,o1))
  }

  if (t1 != t2) return baseBoolean(false)
  let o1 = x.val 
  let o2 = y.val 
  

  switch (t1){
    case TroupeType.ATOM:
      return baseBoolean(x.val.atom == y.val.atom);
      break;
    case TroupeType.PROCESS_ID:
      return baseBoolean(proc.pid_val_equals(x.val, y.val));
      break;
    case TroupeType.LEVEL:
      return levelEquality(o1,o2);
      break;
    case TroupeType.AUTHORITY:
      return levelEquality(o1.authorityLevel, o2.authorityLevel);
      break;    
    case TroupeType.LIST:
    case TroupeType.TUPLE: 
      if (o1.length != o2.length) 
        return baseBoolean(false)
      else {
        let l = levels.lub (x.lev, y.lev);
        for (let j = 0; j < o1.length; j ++ ) {
          let z = runtimeEquals(o1[j], o2[j]);
          l = levels.lub (l, z.lev)
          if (!z.val) {
            return baseBoolean(false, l)
          }
        }
        return baseBoolean(true, l)
      };
      break;
    default:       
      return baseBoolean (x.val == y.val) 
  }
}