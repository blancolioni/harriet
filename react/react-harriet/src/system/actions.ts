import * as t from './actionTypes';
import { ClientDispatch } from '../clients/model';
import { ClientAction } from '../clients/actions';

export interface ZoomAction extends ClientAction {
    type: typeof t.ZOOM
  }
  
export type SystemActionTypes = ZoomAction

export function zoom(clientId : number) : SystemActionTypes {
    return {
        type: t.ZOOM,
        clientId,
        }
}
