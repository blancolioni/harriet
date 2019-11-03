import { ClientState } from '../clients/model';

export enum SystemObjectType {
  Ship = 'SHIP',
  Star = 'STAR',
  World = 'WORLD',
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

export interface WorldPoint {
  x : number
  y : number
  z : number
}
export interface WorldSector {
  red    : number,
  green  : number,
  blue   : number,
  normal : WorldPoint,
  border : WorldPoint[],
}

export interface WorldObject extends SystemObject {
  composition : string,
  climate     : string,
  surface     : WorldSector[],
}

export interface ShipObject extends SystemObject {
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
