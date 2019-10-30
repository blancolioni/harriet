import * as t from './actionTypes';
import { ClientState } from './model';
import { string } from 'prop-types';

export interface NewClientAction {
    type: typeof t.NEW_CLIENT,
    viewName: string,
    viewTitle : string,
    modelName: string,
    modelArgs: string,
  }
  
export interface UpdateClientAction {
  type: typeof t.UPDATE_CLIENT
  clientId : number
  newState : ClientState
}

export interface CloseClientAction {
  type: typeof t.CLOSE_CLIENT
  clientId: number
}

export type ClientActionTypes = NewClientAction | UpdateClientAction | CloseClientAction

export function newClient (viewName : string, viewTitle : string, modelName: string, modelArgs: string) : ClientActionTypes {
  return {
      type: t.NEW_CLIENT,
      viewName,
      viewTitle,
      modelName,
      modelArgs,
      }
}

export function updateClient (clientId : number, newState : ClientState) : ClientActionTypes {
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


