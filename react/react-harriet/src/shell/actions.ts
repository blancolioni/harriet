import * as t from './actionTypes';

export interface CommandAction {
    type: typeof t.COMMAND
    command : string
  }
  
export type ShellActionTypes = CommandAction

export function execute(command : string) : ShellActionTypes {
    return {
        type: t.COMMAND,
        command: command,
        }
}
