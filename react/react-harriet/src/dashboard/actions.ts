import * as t from './actionTypes';
import { Box } from './model';

export interface SetLayoutAction {
  type: typeof t.SET_LAYOUT,
  boxes : Box[],
}

export interface SplitBoxAction {
    type: typeof t.SPLIT_BOX
    horizontal : boolean
    boxId : number
  }
  
export type DashboardActionTypes = SetLayoutAction | SplitBoxAction

export function setLayout(boxes : Box[]) : DashboardActionTypes {
  return {
    type: t.SET_LAYOUT,
    boxes,    
  }
}

export function splitBox (boxId : number, horizontal : boolean) : DashboardActionTypes {
    return {
        type: t.SPLIT_BOX,
        boxId,
        horizontal,
        }
}
