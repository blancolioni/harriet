import * as clients from '../clients/actionTypes';
import { State, shipInitialState } from './model';
import { clientInitialState } from '../clients/model';
import { ClientActionTypes } from '../clients/actions';

export default (state = shipInitialState(clientInitialState), action: ClientActionTypes): State => {
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
