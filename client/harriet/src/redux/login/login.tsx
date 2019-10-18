export const LOGIN = "LOGIN";
export const LOGOUT = "LOGOUT";

export interface LoginState {
    loggedIn : boolean
    user : string
    id : string
    faction : string
}

interface LoginAction {
    type        : typeof LOGIN
    sessionId   : string
    userName    : string
    factionName : string
}

interface LogoutAction {
    type : typeof LOGOUT
}

export type LoginActionTypes = LoginAction | LogoutAction

export function login(sessionId : string, userName : string, factionName : string   ) : LoginAction {
    console.log("action login", sessionId, userName, factionName)
    return {
        type: LOGIN,
        sessionId,
        userName,
        factionName,
    }
}

export function logout()  : LogoutAction {
    return {
        type: LOGOUT,
    }
}
