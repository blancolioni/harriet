import { ClientState } from './clients/model'
import { shellInitialState } from './shell/model'
import { tableInitialState } from './table/model';

interface Model {
    create   : (baseState : ClientState) => ClientState
}

interface ModelTable {
    [key : string] : Model
}

const modelTable : ModelTable = {
    Shell: {
        create: shellInitialState,
    },
    Table: {
        create: tableInitialState,
    },
}

export function initialState (modelName : string, baseState : ClientState) : ClientState {
    return modelTable[modelName].create(baseState);
}