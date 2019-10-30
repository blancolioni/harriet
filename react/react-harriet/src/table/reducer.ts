import * as t from './actionTypes';
import * as clients from '../clients/actionTypes';
import { State, tableInitialState } from './model';
import { TableActionTypes } from './actions';
import { clientInitialState } from '../clients/model';
import { ClientActionTypes } from '../clients/actions';

export default (state = tableInitialState(clientInitialState), action: TableActionTypes | ClientActionTypes): State => {
  switch (action.type) {
    case t.SORT:
        return {
            ...state,
            sortColumn: action.columnIndex,
            sortAscending: action.ascending,
        };

    case clients.UPDATE_CLIENT:
        console.log('update-client', action.newState);
        return {
            ...state,
            ...action.newState,
        }
    default:
        return state;
  }
};
