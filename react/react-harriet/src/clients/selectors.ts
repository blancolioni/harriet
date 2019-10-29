import { State } from './model';

export const clients = (state : State) => state.clients;
export const client = (state: State, clientId : number) => state.clients[clientId];

