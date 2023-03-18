import { TroupeRawValue } from "./TroupeRawValue.mjs";
import { TroupeType } from "./TroupeTypes.mjs";
import { BOT } from "./options.mjs"

export function pid_equals (o1, o2) {
    let eq = o1.val.pid.toString() == o2.val.pid.toString();
    //console.log("pid eq", o1, o2, eq);
    return (eq);
  }

export function pid_val_equals (v1, v2) {
  let eq = v1.pid.toString() == v2.pid.toString();  
  return eq;
}  
  
export class ProcessID implements TroupeRawValue {
    _troupeType = TroupeType.PROCESS_ID
    uuid: any;
    pid: any;
    node: any;
    stringRep: () => string;
    equals: (o1: any, o2: any) => boolean;
    dataLevel = BOT 
    constructor(rt_uuid, pid, node) {      
      this.uuid = rt_uuid;
      this.pid = pid;
      this.node = node ; // getLocalNode();
      this.stringRep = toString;
      this.equals = pid_equals;
      this.stringRep = this.toString;
    }   
  
    toString () {      
      let x = this.pid.toString();
      // console.log (x);
      return x;
    }
}

