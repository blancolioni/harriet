import * as t from './actionTypes';
import { ClientDispatch } from '../clients/model';
import { ClientAction } from '../clients/actions';

export interface ZoomAction extends ClientAction {
    type: typeof t.ZOOM
  }
  
export type TableActionTypes = ZoomAction

export function sort(clientId : number) : TableActionTypes {
    return {
        type: t.ZOOM,
        clientId,
        }
}
