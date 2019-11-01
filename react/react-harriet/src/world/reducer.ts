import * as t from './actionTypes';
import * as clients from '../clients/actionTypes';
import { State, worldInitialState } from './model';
import { TableActionTypes } from './actions';
import { clientInitialState } from '../clients/model';
import { ClientActionTypes } from '../clients/actions';

export default (state = worldInitialState(clientInitialState), action: TableActionTypes | ClientActionTypes): State => {
  switch (action.type) {

    case clients.UPDATE_CLIENT:
        return {
            ...state,
            ...action.newState,
        }
    default:
        return state;
  }
};
