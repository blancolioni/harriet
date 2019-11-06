import * as t from './actionTypes';
import { State } from './model';
import { OutlineActionTypes } from './actions';

const initialState: State = {
  key: 'root',
  label: 'Outliner',
  nodes: [],
};

export default (state = initialState, action: OutlineActionTypes): State => {
  switch (action.type) {
    case t.UPDATE:
        return {
            ...state,
            nodes: action.outline.nodes,
        };

    default:
        return state;
  }
};
