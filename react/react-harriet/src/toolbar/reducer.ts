import * as t from './actionTypes';
import { State } from './model';
import { ToolbarActionTypes } from './actions';

const initialState: State = {
    dateImage : '',
    cashImage : '0.00',
    updateSpeed: 0,
};

export default (state = initialState, action: ToolbarActionTypes): State => {
  switch (action.type) {
    case t.UPDATE_TOOLBAR:
        return {
            ...state,
            dateImage: action.dateImage || state.dateImage,
            cashImage: action.cashImage || state.cashImage,
        };

    case t.SET_SPEED:
        return {
            ...state,
            updateSpeed: action.newSpeed
        }

    default:
        return state;
  }
};
