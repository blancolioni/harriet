import * as t from './actionTypes';
import { State } from './model';
import { ShellActionTypes } from './actions';
import version from '../version';
import { clientInitialState } from '../clients/model';

const initialState: State = {
    ...clientInitialState,
    output: ['Harriet ' + version]
};

export default (state = initialState, action: ShellActionTypes): State => {
  switch (action.type) {
    case t.COMMAND:
        let newOutput = state.output.slice();
        newOutput.push ('> ' + action.command);
        return {
            ...state,
            output: newOutput,
        };

    default:
        return state;
  }
};
