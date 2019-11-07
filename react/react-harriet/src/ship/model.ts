import { ClientState } from '../clients/model';
import { ShipObject } from '../system/model';

export interface State extends ClientState {
  ship : ShipObject | null
}

export const shipInitialState = (baseState : ClientState) => ({
  ...baseState,
  ship: null,
});
