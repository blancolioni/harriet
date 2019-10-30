import * as t from './actionTypes';
import { ClientDispatch } from '../clients/model';
import { ClientAction } from '../clients/actions';

export interface SortAction extends ClientAction {
    type: typeof t.SORT
    columnIndex: number
    ascending: boolean
  }
  
export type TableActionTypes = SortAction

export function sort(clientId : number, columnIndex : number, ascending : boolean) : TableActionTypes {
    return {
        type: t.SORT,
        clientId,
        columnIndex,
        ascending,
        }
}
