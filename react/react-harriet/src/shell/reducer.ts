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
        console.log(state);
        let newOutput = state.output.slice();
        newOutput.push ('> ' + action.command);
        console.log('shell/command', newOutput.join())
        return {
            ...state,
            output: newOutput,
        };

    case t.OUTPUT:
        const newLines = state.output.slice().concat(action.lines);
        console.log('shell/output', newLines)
        return {
            ...state,
            output: newLines,
        };
    
    default:
        return state;
  }
};
