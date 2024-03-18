'use strict'
import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero.mjs'
import { LVal } from '../Lval.mjs';
import { assertIsNTuple, assertIsRecord, assertIsString, assertIsUnit, assertNormalState } from '../Asserts.mjs'
import { Record } from "../Record.mjs";
import { lub } from '../options.mjs';
import { __unit } from '../UnitVal.mjs';
import { TroupeType } from '../TroupeTypes.mjs';

export function BuiltinTypeInformation<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {        
	// returns a string containing the type information 
        getType = mkBase((larg) => {
	     let _t = "unknown" // 2024-03-18; todo: add proper type	
	     switch (larg.val._troupeType) {
		case TroupeType.UNIT: 
			_t = "unit";
			break;
		case TroupeType.BOOLEAN: 
			_t = "boolean";
			break;
		case TroupeType.NUMBER: 
			_t = "number";
			break;
				
		case TroupeType.STRING: 
			_t = "string";
			break;
		case TroupeType.PROCESS_ID: 
			_t = "process_id";
			break;
		case TroupeType.LEVEL: 
			_t = "level";
			break;
		case TroupeType.AUTHORITY: 
			_t = "authority";
			break;
		case TroupeType.CLOSURE: 
			_t = "function";
			break;
		case TroupeType.TUPLE: 
			_t = "tuple";
			break;
		case TroupeType.LIST: 
			_t = "list";
			break;
		case TroupeType.RECORD: 
			_t = "record";
			break;
		case TroupeType.LOCALOBJECT: 
			_t = "localobject";
			break;
		default:
			switch (typeof larg.val)  {
				case 'string':
					_t = "string";
					break;
				case 'number':
					_t = "number";
					break;
				case 'boolean':
					_t = "boolean"
					break;
			}
	     }
	     return this.runtime.ret (
		 new LVal ( _t
			  , lub (larg.tlev, this.runtime.$t.pc)
			  , this.runtime.$t.pc)
	     )



        })
    }
}
