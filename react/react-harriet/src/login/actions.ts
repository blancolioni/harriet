import * as t from './actionTypes';

export interface LoginAction {
    type: typeof t.LOGIN
    userName : string
    password : string
  }
  
export interface AuthorizedAction {
    type: typeof t.AUTHORIZED
    userName : string
    factionName : string
    token : string
}

export interface LoginFailedAction {
    type: typeof t.LOGIN_FAILED,
    error: string,
}

export interface LogoutAction {
    type: typeof t.LOGOUT
}

export type LoginActionTypes = LoginAction | AuthorizedAction | LoginFailedAction | LogoutAction

export function login (userName : string, password : string) : LoginActionTypes {
    return {
        type: t.LOGIN,
        userName: userName,
        password: password,
        }
}

export function authorize(userName  : string, factionName : string, token : string) : LoginActionTypes {
    return {
        type: t.AUTHORIZED,
        userName: userName,
        factionName: factionName,
        token: token,
    }
}

export function loginFailed(error : string) : LoginActionTypes {
    return {
        type: t.LOGIN_FAILED,
        error: error,
    }
}

export function logout() : LoginActionTypes {
    return {
        type: t.LOGOUT,
    }
}
