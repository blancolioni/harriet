import * as t from './actionTypes';
import { State } from './model';
import { ClientActionTypes } from './actions';
import { initialState as clientInitialState } from '../models'

export const initialState: State = {
    clients: [],
};

export default (state = initialState, action: ClientActionTypes): State => {
  switch (action.type) {
    case t.NEW_CLIENT:
        let newClients = state.clients.slice();
        let baseState = {
            loading: true,
            viewName: action.viewName,
            title: action.viewTitle,
            modelName: action.modelName,
        };
        newClients.push(clientInitialState(action.modelName, baseState));
        return {
            ...state,
            clients: newClients,
        };

    default:
        return state;
  }
};
