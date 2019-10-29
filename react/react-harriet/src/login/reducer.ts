import * as t from './actionTypes';
import { State } from './model';

const initialState: State = {
    loggedIn: false,
};

export default (state = initialState, action: any): State => {
  switch (action.type) {
    case t.AUTHORIZED:
        return {
            ...state,
            loggedIn: true,
            userName: action.userName,
            token: action.token,
        };

    case t.LOGOUT:
        return initialState;

    default:
        return state;
  }
};
