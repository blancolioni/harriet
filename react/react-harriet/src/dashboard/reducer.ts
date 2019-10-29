import * as t from './actionTypes';
import { State, Box } from './model';
import { DashboardActionTypes } from './actions';

const initialState: State = {
    boxes : new Box(0,{left:1, right: 13, top: 1, bottom: 13}).splitVertical(1, 2)
};

export default (state = initialState, action: DashboardActionTypes): State => {
  switch (action.type) {
    case t.SPLIT_BOX:
        let newArray = state.boxes.slice();
        let newChild1 = newArray.length + 1;
        let newChild2 = newArray.length + 2;
        let oldBox = state.boxes[action.boxId];
        let newBoxes = oldBox.split(action.horizontal, [newChild1, newChild2]);
        newArray[action.boxId] = newBoxes[0];
        newArray.push(newBoxes[1]);
        newArray.push(newBoxes[2]);

        return {
            ...state,
            boxes : newArray,
        };

    default:
        return state;
  }
};
