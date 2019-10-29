import * as t from './actionTypes';

export interface UpdateToolbarAction {
    type: typeof t.UPDATE_TOOLBAR
    dateImage : string
    cashImage : string
  }
  
export type ToolbarActionTypes = UpdateToolbarAction

export function updateToolbar (dateImage : string, cashImage : string) : ToolbarActionTypes {
    return {
        type: t.UPDATE_TOOLBAR,
        dateImage : dateImage,
        cashImage : cashImage,
        }
}
