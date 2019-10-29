import * as t from './actionTypes';

export interface LoginAction {
    type: typeof t.LOGIN
    userName : string
    password : string
  }
  
export interface AuthorizedAction {
    type: typeof t.AUTHORIZED
    userName : string
    token : string
}

export interface LoginFailedAction {
    type: typeof t.LOGIN_FAILED,
    error: string,
}

export function login (userName : string, password : string) : LoginAction {
    return {
        type: t.LOGIN,
        userName: userName,
        password: password,
        }
}

export function authorize(userName  : string, token : string) : AuthorizedAction {
    return {
        type: t.AUTHORIZED,
        userName: userName,
        token: token,
    }
}

export function loginFailed(error : string) : LoginFailedAction {
    return {
        type: t.LOGIN_FAILED,
        error: error,
    }
}