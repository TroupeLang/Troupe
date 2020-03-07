export const enum TroupeType {
  UNIT=0,
  BOOLEAN=1,
  NUMBER=2,
  STRING=3,
  PROCESS_ID=4,
  LEVEL=5,
  AUTHORITY=6,
  CAPABILITY=7,
  ATOM=8,
  /* up until this point only base types */  

  /* aggregate types */
  CLOSURE=100,
  LVAL=101,        // TODO: AA; 2020-03-03 ; this should be only used for transports;
  TUPLE=102,
  LIST=103
}


export function isLVal(x) {
  return   (typeof x.val != "undefined" &&
            typeof x.lev != "undefined" && 
            typeof x.tlev != "undefined" );
}

export  function isClosure(x) {
  return  (typeof x.env != "undefined"
        && typeof x.fun != "undefined"
        && typeof x.namespace != "undefined"
      )
}

export  function isProcessId (x) {

  return (typeof x.pid != "undefined"
    && typeof x.node != "undefined"
    && typeof x.uuid != "undefined" )
}

export function isTuple(x) {
  return (typeof x.isTuple != "undefined" )
}

export function isList(x) {
  return (typeof x.isList != "undefined")
}

export function isLevel (x) {
  return (typeof x.isLevel != "undefined")
}

export function isAuthority (x) {
  return (typeof x.authorityLevel != "undefined");
}

export function isCapability (x) {
  return x._troupeType == TroupeType.CAPABILITY
}

export function isAtom (x) {
  return (typeof x.atom != "undefined");
}

// 2020-03-03; AA; observe that the use of
// these function together with a switch 
// statement creates an extra branching
// but this is a placeholder for future
// refactoring where LVals contain explicit
// type information 

// observe: that the argument is not an
// lval but is the vaule field of 
// the lval.
export function getTroupeType (x:any) {
  
  if (x._troupeType) { // hack; 2020-03-07; aa
    return x._troupeType;
  }

  if (isClosure(x)) {
    return TroupeType.CLOSURE
  } else if (isTuple(x)) {
    return TroupeType.TUPLE
  } else if (isList(x)) {
    return TroupeType.LIST
  } else if (isProcessId(x)) {
    return TroupeType.PROCESS_ID
  } else if (isLevel(x)) {
    return TroupeType.LEVEL
  } else if (isAuthority(x)) {
    return TroupeType.AUTHORITY
  } else if (isAtom(x)) {
    return TroupeType.ATOM
  } else if (typeof(x) === 'number') {
    return TroupeType.NUMBER
  } else if (typeof(x) === 'boolean') {
    return TroupeType.BOOLEAN
  } else if (typeof(x) === 'string') {
    return TroupeType.STRING
  } else if (isLVal(x)) {
    return TroupeType.LVAL;
  } else if (x._is_unit) {
    return TroupeType.UNIT
  } else if (isCapability(x)) {
    return TroupeType.CAPABILITY
  }
  throw new Error (`Cannot identify troupe type for value ${x.toString()}`);
}

export function isBaseType (t:TroupeType) {
  return t <= TroupeType.ATOM
}

export function isAggregate(t:TroupeType) {
  return t >= TroupeType.CLOSURE
}