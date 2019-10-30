import * as t from './actionTypes';
import { State } from './model';
import { ClientActionTypes } from './actions';

const initialState: State = {
    clients: [],
};

export default (state = initialState, action: ClientActionTypes): State => {
  switch (action.type) {
    case t.NEW_CLIENT:
        let newClients = state.clients.slice();
        newClients.push({
            loading: true,
            viewName: action.viewName,
            title: action.viewName + ' loading ...',
            modelName: action.modelName,
           // modelArgs : action.modelArgs,
        });
        return {
            ...state,
            clients: newClients,
        };

    default:
        return state;
  }
};
