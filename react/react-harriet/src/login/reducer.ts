import * as t from './actionTypes';
import { State } from './model';
import { LoginActionTypes } from './actions';

const initialState: State = {
    loggedIn: false,
    submitted: false,
    loading: false,
};

export default (state = initialState, action: LoginActionTypes): State => {
  switch (action.type) {
    case t.AUTHORIZED:
        return {
            ...state,
            loggedIn: true,
            userName: action.userName,
            factionName: action.factionName,
            token: action.token,
        };

    case t.LOGIN:
        return {
            ...state,
            submitted: true,
            loading: true,
        }

    case t.LOGOUT:
        return initialState;

    case t.LOGIN_FAILED:
        return {
            ...initialState,
            error: action.error,
        }

    default:
        return state;
  }
};
