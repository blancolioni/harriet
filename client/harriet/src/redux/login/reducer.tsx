import { LOGIN, LOGOUT, LoginState, LoginActionTypes } from './login';

const initialState : LoginState = {
    loggedIn : false,
    user : '',
    id : '',
    faction : '',
}

export default function reducer
    (state = initialState,
     action : LoginActionTypes
    ) : LoginState {
        switch (action.type) {
            case LOGIN:
                console.log("logging in", action);
                return {
                    loggedIn: true,
                    user: action.userName,
                    id: action.sessionId,
                    faction: action.factionName,
                };

            case LOGOUT:
                    console.log("logging out", action);
                    return {
                        loggedIn: false,
                        user : '',
                        id : '',
                        faction : '',
                }

            default:
                return state;
        }
}