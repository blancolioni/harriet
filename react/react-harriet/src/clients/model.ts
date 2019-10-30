export type ClientState = {
  loading : boolean,
  viewName: string,
  modelName: string,
  title: string,
  };

export type State = {
  clients: ClientState[],
}