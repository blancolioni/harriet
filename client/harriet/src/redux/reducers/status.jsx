import { UPDATE_TIME, SET_SPEED } from '../actionTypes';

export default function reducer(state = { time: 0, timeImage: '9999-99-99', updateSpeed: 0, paused: true }, action) {
    switch (action.type) {
        case UPDATE_TIME:
            console.log('update-time', action.payload.time, action)
            return {
                ...state,
                time: action.payload.content.time,
                timeImage: action.payload.content.timeImage,
            };

        case SET_SPEED:
            const newSpeed = action.newSpeed;
            console.log('set-speed', action)
            return {
                ...state,
                paused: newSpeed == 0,
                updateSpeed: newSpeed,
            };

        default:
            return state;
    }
}