export const START_CLIENT = "START_CLIENT";
export const UPDATE_CLIENT = "UPDATE_CLIENT";

export interface ModelState {
    readonly title      : string
}

export interface ClientState {
    readonly clientId   : number
    readonly clientType : string  
    readonly state      : ModelState
}

interface StartClientAction {
    type       : typeof START_CLIENT
    clientType : string
    startState : ModelState
}

interface UpdateClientAction {
    type : typeof UPDATE_CLIENT
    state : ModelState
}

export type ClientActionTypes = StartClientAction | UpdateClientAction

export function startClient(clientId : number, clientType : string, startState : ModelState) : StartClientAction {
    console.log('startClient', clientId, clientType, startState);
    return {
        type: START_CLIENT,
        clientType,
        startState,
    }
}

export function updateClient(clientId : number, state : ModelState) : UpdateClientAction {
    console.log('updateClient', clientId, state);
    return {
        type: UPDATE_CLIENT,
        state
    }
}