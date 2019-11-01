import { ClientState } from '../clients/model';

export enum SystemObjectType {
  World = 'WORLD',
  Star = 'STAR',
}

export interface SystemObject {
  type: SystemObjectType,
  name: string,
  mass: number,
  orbit: number,
  longitude: number,
  radius: number,
  day: number,
  axisTilt: number,
  temperature : number,
  dependents: SystemObject[],
}

export interface StarObject extends SystemObject {
  red   : number,
  green : number,
  blue  : number,
}

export interface WorldObject extends SystemObject {
  composition : string,
  climate     : string,
}

export interface State extends ClientState {
  systemName : string,
  primary    : StarObject | null,
}

export const systemInitialState = (baseState : ClientState) => ({
  ...baseState,
  systemName: '',
  primary: null,
});
