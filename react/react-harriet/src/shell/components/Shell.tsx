import React from 'react';

import { State } from '../model';

import Console from './Console';
import LineInput from './LineInput';
import { ClientDispatch } from '../../clients/model';

interface ShellDispatch extends ClientDispatch {
    execute : (command : string) => void
}

interface Props {
    clientState : State,
    clientDispatch : ShellDispatch
}

export default class Shell extends React.Component<Props,State> {
    
    render() {
        console.log('shell', 'render', this.props.clientState.output)
        return (
            <div>
                <Console lines={this.props.clientState.output}></Console>
                <LineInput execute={this.props.clientDispatch.execute}></LineInput>
            </div>
        );
    }
}
