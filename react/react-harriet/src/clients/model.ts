export type ClientState = {
  loading : boolean,
  clientId : number,
  viewName: string,
  modelName: string,
  title: string,
  };

export type ClientDispatch = {
  requestUpdate : (detail : number) => void
  execute : (command : string) => void
};

export const clientInitialState : ClientState = {
  loading: true,
  clientId: -1,
  viewName: "",
  modelName: "",
  title: "",
}

export type State = {
  clients: ClientState[],
}