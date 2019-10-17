import { UPDATE_TIME, SET_SPEED, SET_CASH, UPDATE_FACTION } from '../actionTypes';

export const setCash = cash => ({
    type: SET_CASH,
    faction: { cash: cash },
    });

export const updateFaction = faction => ({
    type: UPDATE_FACTION,
    faction,
    });
    