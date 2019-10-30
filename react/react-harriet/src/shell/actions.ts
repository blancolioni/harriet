import * as t from './actionTypes';
import { ClientDispatch } from '../clients/model';
import { ClientAction } from '../clients/actions';

// export interface ShellDispatch extends ClientDispatch {
//   execute : typeof execute;
// }

export interface CommandAction extends ClientAction {
    type: typeof t.COMMAND
    command : string
  }
  
export interface OutputAction extends ClientAction {
  type: typeof t.OUTPUT
  lines: string[]
}

export type ShellActionTypes = CommandAction | OutputAction

export function execute(clientId : number, command : string) : ShellActionTypes {
    return {
        type: t.COMMAND,
        clientId,
        command,
        }
}

export function addToOutput(clientId : number, lines : string[]) : ShellActionTypes {
  console.log('addToOutput', clientId, lines)
  return {
    type: t.OUTPUT,
    clientId,
    lines,
  }
}
