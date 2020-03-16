function pid_equals (o1, o2) {
    let eq = o1.val.pid.toString() == o2.val.pid.toString();
    //console.log("pid eq", o1, o2, eq);
    return (eq);
  }

function pid_val_equals (v1, v2) {
  let eq = v1.pid.toString() == v2.pid.toString();  
  return eq;
}  
  
class ProcessID {
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

module.exports = {
    pid_equals: pid_equals,
    pid_val_equals: pid_val_equals,
    ProcessID : ProcessID
};
