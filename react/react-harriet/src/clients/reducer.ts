import * as t from './actionTypes';
import * as auth from '../login/actionTypes';

import { State, clientInitialState as emptyZeroClient } from './model';
import { ClientAction, ClientActionTypes } from './actions';
import { initialState as clientInitialState } from '../models'
import { LoginActionTypes } from '../login/actions';

export const initialState: State = {
    clients: [emptyZeroClient],
};

type ClientReducer = (state : any, action : any) => any;

export default (clientReducers: ClientReducer[]) => {
 const reducer = (state = initialState, action: ClientActionTypes | LoginActionTypes): State => {
  switch (action.type) {
    case t.NEW_CLIENT:
        let newClients = state.clients.slice();
        let baseState = {
            loading: true,
            clientId: action.clientId,
            viewName: action.viewName,
            title: action.viewTitle,
            modelName: action.modelName,
        };
        newClients.push(clientInitialState(action.viewName, baseState));
        return {
            ...state,
            clients: newClients,
        };

    case auth.LOGOUT:
        return initialState;

    default:
        if ('clientId' in action) {
            let clientAction = action as ClientAction
            let s = state.clients[clientAction.clientId]
            for (const r of clientReducers) {
                s = r(s, clientAction)
            }
            let newClients = state.clients.slice();
            newClients[clientAction.clientId] = s;
            return {
                ...state,
                clients: newClients,
            }
        }
        return state;
  }
}
return reducer;
}
