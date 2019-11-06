import * as t from './actionTypes';
import { ClientDispatch } from '../clients/model';
import { ClientAction } from '../clients/actions';

export interface ZoomAction extends ClientAction {
    type: typeof t.ZOOM
    name: string
  }
  
export type SystemActionTypes = ZoomAction

export function zoom(clientId : number, name: string) : SystemActionTypes {
    return {
        type: t.ZOOM,
        clientId,
        name
        }
}
