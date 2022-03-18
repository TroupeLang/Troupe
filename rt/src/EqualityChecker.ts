import {TroupeType} from './TroupeTypes'
import {LVal} from './Lval'
const proc = require('./process.js');
import levels  = require ('./options')



export function runtimeEquals (x:LVal, y:LVal) {  
  let t1 = x.troupeType
  let t2 = y.troupeType

  function baseBoolean (b:boolean, l=levels.lub (x.lev, y.lev)) {
    return new LVal (b, l, levels.BOT) 
  }

  function levelEquality (o1, o2) {
    let b1 = levels.flowsTo (o1, o2)
    if (!b1)
      return baseBoolean (false);
    return baseBoolean(levels.flowsTo(o2,o1))
  }

  function arrayEquality (o1, o2) {
    if (o1.length != o2.length) 
      return baseBoolean(false)
   
    let l = levels.lub (x.lev, y.lev);
    for (let j = 0; j < o1.length; j ++ ) {
      let z = runtimeEquals(o1[j], o2[j]);
      l = levels.lub (l, z.lev)
      if (!z.val) {
        return baseBoolean(false, l)
      }
    }
    return baseBoolean(true, l)
  
  }

  function recordEquality (o1,o2) {
    let l = levels.lub (x.lev, y.lev); 
    if (o1.__obj.size != o2.__obj.size) {
      return baseBoolean(false, l);
    }

    for (let [k,v] of o1.__obj.entries()) {
      if (o2.__obj.has(k)) {
        let u = o2.__obj.get(k);
        let z = runtimeEquals (v,u);
        l = levels.lub (l, z.lev);
        if (!z.val) {
          return baseBoolean(false,l)
        }
      } else {
        return baseBoolean (false, l);
      }
    }

    return baseBoolean(true, l)
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
      return arrayEquality(o1.toArray(), o2.toArray());
      break;
    case TroupeType.TUPLE: 
      return arrayEquality(o1,o2)
      break;
    case TroupeType.RECORD:
      return recordEquality(o1,o2)
    default:       
      return baseBoolean (x.val == y.val) 
  }
}