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
  LIST=103,
  RECORD=104, 
  /* meaningless to serialize */
  LOCALOBJECT=200
}

export const enum ClosureType { 
  /* okay to serialize */
  REGULARFN = 0,         

  /* meaningless to serialize */
  BUILTINFN = 1,       
  SANDBOXKONT = 2  ,
  SERVICEFN = 3    
}

export function isLVal(x) {
  return   (typeof x.val != "undefined" &&
            typeof x.lev != "undefined" && 
            typeof x.tlev != "undefined" );
}

export function isSerializableClosure (ct)  {
  return (ct == ClosureType.REGULARFN);
}

export function getTypeForBasicValue (x:any) {
  switch (typeof(x)) {
    case 'number':
      return TroupeType.NUMBER
    case 'boolean':
      return TroupeType.BOOLEAN
    case 'string':
      return TroupeType.STRING
  }  
  throw new Error (`Cannot identify troupe type for value ${JSON.stringify(x)}  of type ${typeof x}`);
}

export function getTroupeType (x:any) {  
  if (x._troupeType != undefined) { 
    return x._troupeType;
  }

  throw new Error (`Cannot identify troupe type for value ${x.toString()} of type ${typeof x}`);
}

/*
export function isBaseType (t:TroupeType) {
  return t <= TroupeType.ATOM
}

export function isAggregate(t:TroupeType) {
  return t >= TroupeType.CLOSURE
}
*/