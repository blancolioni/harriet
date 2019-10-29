import * as t from './actionTypes';

export interface SplitBoxAction {
    type: typeof t.SPLIT_BOX
    horizontal : boolean
    boxId : number
  }
  
export type DashboardActionTypes = SplitBoxAction

export function splitBox (boxId : number, horizontal : boolean) : DashboardActionTypes {
    return {
        type: t.SPLIT_BOX,
        boxId,
        horizontal,
        }
}
