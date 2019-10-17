import { LOGIN, LOGOUT } from '../actionTypes';

export const login = content => ({
    type: LOGIN,
    payload: {
        content
        },
    });

export const logout = content => ({
    type: LOGOUT,
    payload: {
        content
        },
    });
    