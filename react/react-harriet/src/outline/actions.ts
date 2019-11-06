import * as t from './actionTypes';
import { State } from './model';

export interface UpdateOutlineAction {
    type: typeof t.UPDATE
    outline: State
  }

export type OutlineActionTypes = UpdateOutlineAction

export function updateOutline (outline : State) : OutlineActionTypes {
    return {
        type: t.UPDATE,
        outline,
        }
}
