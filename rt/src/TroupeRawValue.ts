import {TroupeType} from './TroupeTypes'
import { Level } from './Level'

export interface TroupeRawValue {
    _troupeType: TroupeType;
    dataLevel: Level ;
    stringRep (omitLevels?: boolean, taintRef?: any): string;
}

export interface TroupeAggregateRawValue extends TroupeRawValue {
    dataLevel: Level
}