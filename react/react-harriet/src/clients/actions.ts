import * as t from './actionTypes';
import { State } from './model';

export interface NewClientAction {
    type: typeof t.NEW_CLIENT,
    viewName: string,
  }
  
export interface UpdateClientAction {
  type: typeof t.UPDATE_CLIENT
  clientId : number
  newState : State
}

export interface CloseClientAction {
  type: typeof t.CLOSE_CLIENT
  clientId: number
}

export type ClientActionTypes = NewClientAction | UpdateClientAction | CloseClientAction

export function newClient (viewName : string) : ClientActionTypes {
  return {
      type: t.NEW_CLIENT,
      viewName: viewName,
      }
}

export function updateClient (clientId : number, newState : State) : ClientActionTypes {
  return {
      type: t.UPDATE_CLIENT,
      clientId,
      newState,
      }
}

export function closeClient (clientId : number) : ClientActionTypes {
  return {
      type: t.CLOSE_CLIENT,
      clientId,
      }
}


