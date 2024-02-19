import {TroupeType} from './TroupeTypes.mjs'
import { Level } from './Level.mjs'

export interface TroupeRawValue {
    _troupeType: TroupeType;
    dataLevel: Level ;
    stringRep (omitLevels?: boolean, taintRef?: any): string;
}

export interface TroupeAggregateRawValue extends TroupeRawValue {
    dataLevel: Level
}