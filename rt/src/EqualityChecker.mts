import {TroupeType} from './TroupeTypes.mjs'
import {LVal} from './Lval.mjs'
import { TroupeRawValue } from './TroupeRawValue.mjs';
import * as proc from './process.mjs'
import * as levels  from './options.mjs'
import { Level } from './Level.mjs';

/**
 * Compute deep equality of the given unlabelled raw values.
 * The returned value label is the join of all visited nested labelled values.
 */
export function runtimeEquals(x: TroupeRawValue, y: TroupeRawValue): LVal {
  function baseBoolean(b: boolean, l: Level = levels.BOT) {
    return new LVal(b, l, levels.BOT)
  }

  function levelEquality(o1, o2) {
    return baseBoolean(levels.flowsTo(o1, o2) && levels.flowsTo(o2, o1))
  }

  function arrayEquality(o1, o2) {
    if (o1.length != o2.length)
      return baseBoolean(false)

    // Join of the labels of values compared so far
    let l = levels.BOT
    for (let j = 0; j < o1.length; j++) {
      let z = runtimeEquals(o1[j].val, o2[j].val);
      l = levels.lub(l, z.lev, o1[j].lev, o2[j].lev)
      if (!z.val) {
        return baseBoolean(false, l)
      }
    }
    return baseBoolean(true, l)

  }

  function recordEquality(o1, o2) {
    if (o1.__obj.size != o2.__obj.size) {
      return baseBoolean(false);
    }

    // Join of the labels of values compared so far
    let l = levels.BOT
    for (let [k, v] of o1.__obj.entries()) {
      if (o2.__obj.has(k)) {
        let u = o2.__obj.get(k);
        let z = runtimeEquals(v.val, u.val);
        l = levels.lub(l, z.lev, v.lev, u.lev);
        if (!z.val) {
          return baseBoolean(false, l)
        }
      } else {
        console.log("does not have field")
        return baseBoolean(false, l);
      }
    }
    return baseBoolean(true, l)
  }

  // If both types are basic types (and not Troupe types), this check will pass
  // and basic comparison with "==" will be used in the default case below.
  if (x._troupeType != y._troupeType) return baseBoolean(false)

  let o1: any = x
  let o2: any = y

  switch (x._troupeType) {
    case TroupeType.ATOM:
      return baseBoolean(o1.atom == o2.atom);
    case TroupeType.PROCESS_ID:
      return baseBoolean(proc.pid_val_equals(o1, o2));
    case TroupeType.LEVEL:
      return levelEquality(o1, o2);
    case TroupeType.AUTHORITY:
      return levelEquality(o1.authorityLevel, o2.authorityLevel);
    case TroupeType.LIST:
      return arrayEquality(o1.toArray(), o2.toArray());
    case TroupeType.TUPLE:
      return arrayEquality(o1, o2)
    case TroupeType.RECORD:
      return recordEquality(o1, o2)
    default:
      return baseBoolean(o1 == o2)
  }
}
