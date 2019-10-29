import * as t from './actionTypes';

export interface UpdateToolbarAction {
    type: typeof t.UPDATE_TOOLBAR
    dateImage? : string
    cashImage? : string
  }

export interface SetSpeedAction {
  type: typeof t.SET_SPEED
  newSpeed: number
}

export type ToolbarActionTypes = UpdateToolbarAction | SetSpeedAction

export function updateToolbar (dateImage? : string, cashImage? : string) : ToolbarActionTypes {
    return {
        type: t.UPDATE_TOOLBAR,
        dateImage : dateImage,
        cashImage : cashImage,
        }
}

export function setSpeed(newSpeed : number) : ToolbarActionTypes {
  return {
    type: t.SET_SPEED,
    newSpeed,
  }
}