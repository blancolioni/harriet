export type ClientState = {
  loading : boolean,
  viewName: string,
  title: string,
  };

export type State = {
  clients: ClientState[],
}