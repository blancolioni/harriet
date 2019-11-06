import * as t from './actionTypes';
import { ClientState } from './model';
import { string } from 'prop-types';

export interface ClientAction {
  clientId : number,
}

export interface NewClientAction extends ClientAction {
    type: typeof t.NEW_CLIENT,
    viewName: string,
    viewTitle : string,
    modelName: string,
    modelArgs: string,
  }
  
export interface UpdateClientAction extends ClientAction {
  type: typeof t.UPDATE_CLIENT
  newState : ClientState
}

export interface RequestUpdateAction extends ClientAction {
  type: typeof t.REQUEST_UPDATE
  detail: number
}

export interface CloseClientAction extends ClientAction {
  type: typeof t.CLOSE_CLIENT
}

export type ClientActionTypes = NewClientAction | UpdateClientAction | RequestUpdateAction | CloseClientAction

export function newClient (clientId : number, viewName : string, viewTitle : string, modelName: string, modelArgs: string) : ClientActionTypes {
  return {
      type: t.NEW_CLIENT,
      clientId,
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

export function requestUpdate (clientId : number, detail : number) : ClientActionTypes {
  return {
    type: t.REQUEST_UPDATE,
    clientId,
    detail,
  }
}

export function closeClient (clientId : number) : ClientActionTypes {
  return {
      type: t.CLOSE_CLIENT,
      clientId,
      }
}


