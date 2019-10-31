import { ClientState } from '../clients/model';

export interface State extends ClientState {
}

export const worldInitialState = (baseState : ClientState) => ({
  ...baseState,
});
