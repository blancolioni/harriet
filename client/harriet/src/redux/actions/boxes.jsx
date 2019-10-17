import { SPLIT, SET_CLIENT, INIT_BOXES } from '../actionTypes';

export const splitVertical = boxId => ({
    type: SPLIT,
    payload: {
        vertical: true,
        boxId: boxId,
        },
    });

export const splitHorizontal = boxId => ({
    type: SPLIT,
    payload: {
        vertical: false,
        boxId: boxId,
        },
    });

export const setClient = contents => ({
    type: SET_CLIENT,
    payload: {
        contents,
        },
    });
    
export const initBoxes = contents => ({
    type: INIT_BOXES,
    payload: {
        contents,
        },
    });
 