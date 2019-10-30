import { ClientState } from '../clients/model';
import version from '../version';

export interface State extends ClientState  {
  output: string[]
};

export const shellInitialState = (baseState : ClientState) => ({
  ...baseState,
  output: ['Harriet ' + version],
});
