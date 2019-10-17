import { UPDATE_TIME, SET_SPEED } from '../actionTypes';

export default function reducer(state = { time: 0, timeImage: '9999-99-99', updateSpeed: 0, paused: true }, action) {
    switch (action.type) {
        case UPDATE_TIME:
            return {
                ...state,
                time: action.payload.content.time,
                timeImage: action.payload.content.timeImage,
            };

        case SET_SPEED:
            const newSpeed = action.newSpeed;
            return {
                ...state,
                paused: newSpeed == 0,
                updateSpeed: newSpeed,
            };

        default:
            return state;
    }
}