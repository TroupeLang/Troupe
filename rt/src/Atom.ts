import runId from "./runId"
import { TroupeType } from "./TroupeTypes"
import { TroupeRawValue } from "./TroupeRawValue";
import levels = require ('./options')

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
