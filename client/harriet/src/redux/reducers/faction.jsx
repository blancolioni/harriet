import { SET_CASH, UPDATE_FACTION } from '../actionTypes';

export default function reducer(state = { cash: 0 }, action) {
    switch (action.type) {
        case SET_CASH:
            return {
                ...state,
                cash: action.faction.cash,
            };

        case UPDATE_FACTION:
            return {
                ...state,
                cash: action.faction.cash,
            };
        default:
            return state;
    }
}