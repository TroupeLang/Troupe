export const enum TroupeType {
  UNIT=0,
  BOOLEAN,
  NUMBER,
  STRING,
  PROCESS_ID,
  LEVEL,
  AUTHORITY,
  CLOSURE,
  ATOM,
  /* up until this point only base types */  
  LVAL, // TODO: AA; 2020-03-03 ; this should be only used for transports;
  TUPLE,
  LIST
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
  } else if (Number.isInteger (x)) {
    return TroupeType.NUMBER
  } else if (typeof(x) === 'boolean') {
    return TroupeType.BOOLEAN
  } else if (typeof(x) === 'string') {
    return TroupeType.STRING
  } else if (isLVal(x)) {
    return TroupeType.LVAL;
  }

}

export function isBaseType (t:TroupeType) {
  return t <= TroupeType.ATOM
}