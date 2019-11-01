import * as t from './actionTypes';
import * as clients from '../clients/actionTypes';
import { State, systemInitialState } from './model';
import { SystemActionTypes } from './actions';
import { clientInitialState } from '../clients/model';
import { ClientActionTypes } from '../clients/actions';

export default (state = systemInitialState(clientInitialState), action: SystemActionTypes | ClientActionTypes): State => {
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
