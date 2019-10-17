import { LOGIN, LOGOUT } from '../actionTypes';

export default function reducer(state = { loggedIn: false}, action) {
    switch (action.type) {
        case LOGIN:
            return {
                ...state,
                loggedIn: true,
                user: action.payload.content.user,
                id: action.payload.content.id,
                faction: action.payload.content.faction,
            };

        case LOGOUT:
            return {
                loggedIn: false,
            }

        default:
            return state;
    }
}