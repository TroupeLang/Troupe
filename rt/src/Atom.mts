import runId from "./runId.mjs"
import { TroupeType } from "./TroupeTypes.mjs"
import { TroupeRawValue } from "./TroupeRawValue.mjs";
import * as levels from  './options.mjs'

let rt_uuid = runId

export class Atom implements TroupeRawValue  {
  atom: string 
  creation_uuid: any;
  _troupeType = TroupeType.ATOM
  dataLevel = levels.BOT
  
  constructor (name:string, creation_uuid = rt_uuid) {
    this.atom = name; 
    this.creation_uuid = creation_uuid    
  }

  stringRep (_omitLevels = false) {
      return this.atom
  }  
}
