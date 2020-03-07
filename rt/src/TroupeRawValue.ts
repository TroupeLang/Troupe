import {TroupeType} from './TroupeTypes'

export interface TroupeRawValue {
    _troupeType: TroupeType;
    stringRep (omitLevels?: boolean, taintRef?: any): string;
}