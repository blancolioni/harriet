export type ClientState = {
  loading : boolean,
  viewName: string,
  modelName: string,
  title: string,
  };

export const clientInitialState : ClientState = {
  loading: true,
  viewName: "",
  modelName: "",
  title: "",
}

export type State = {
  clients: ClientState[],
}