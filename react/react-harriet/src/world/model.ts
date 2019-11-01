import { ClientState } from '../clients/model';
import { WorldObject } from '../system/model';

export enum Composition {
  Hydrogen = "HYDROGEN",
  Gaseous = "GASEOUS",
  Ice = "ICE",
  Rock = "ROCK",
  Rock_Ice = "ROCK_ICE",
  Rock_Iron = "ROCK_IRON",
}

export enum Climate {
  Airless = "AIRLESS", 
  Desert = "DESERT",
  Iceball = "ICEBALL",
  Martian = "MARTIAN",
  Temperate = "TEMPERATE",
  Venusian = "VENUSIAN",
  Water = "WATER",
  Jovian = "JOVIAN",
}

export interface State extends ClientState {
  world : WorldObject | null
}

export const worldInitialState = (baseState : ClientState) => ({
  ...baseState,
  world: null,
});
