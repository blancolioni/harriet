import { UPDATE_TIME, SET_SPEED } from '../actionTypes';

export const setTime = content => ({
    type: UPDATE_TIME,
    payload: {
        content
        },
    });

export const setSpeed = newSpeed => ({
    type: SET_SPEED,
    newSpeed,
    });
    