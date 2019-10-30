import { ClientState } from './clients/model'
import { shellInitialState } from './shell/model'

interface Model {
    create   : (baseState : ClientState) => ClientState
}

interface ModelTable {
    [key : string] : Model
}

const modelTable : ModelTable = {
    shell: {
        create: shellInitialState,
    }
}

export function initialState (modelName : string, baseState : ClientState) : ClientState {
    return modelTable[modelName].create(baseState);
}