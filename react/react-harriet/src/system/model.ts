import { ClientState } from '../clients/model';

export enum SystemObjectType {
  Ship = 'SHIP',
  Star = 'STAR',
  World = 'WORLD',
}

export interface SystemObject {
  type: SystemObjectType,
  id: string,
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
  color  : string
  normal : WorldPoint,
  border : WorldPoint[],
  model  : string,
}

export interface WorldObject extends SystemObject {
  composition : string,
  climate     : string,
  surface     : WorldSector[],
}

export enum ComponentShape {
  Cuboid = "CUBOID",
  Cylinder = "CYLINDER",
  Ellipsoid = "ELLIPSOID",
  Truncated_Cone = "TRUNCATED_CONE"
}

export interface ShipModule {
  shape : ComponentShape
  x : number
  y : number
  z : number
  dx : number
  dy : number
  dz : number
}

export interface ShipObject extends SystemObject {
  modules : ShipModule[]
}

export interface State extends ClientState {
  systemName : string,
  primary    : StarObject | null,
  zoom       : string,
}

export const systemInitialState = (baseState : ClientState) => ({
  ...baseState,
  systemName: '',
  primary: null,
  zoom: '',
});
