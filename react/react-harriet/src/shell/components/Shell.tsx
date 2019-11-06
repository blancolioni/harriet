import React from 'react';

import { State } from '../model';

import Console from './Console';
import LineInput from './LineInput';
import { ClientDispatch } from '../../clients/model';

interface ShellDispatch extends ClientDispatch {
}

interface Props {
    clientState : State,
    clientDispatch : ShellDispatch
}

export default class Shell extends React.Component<Props,State> {
    
    render() {
        return (
            <div>
                <Console lines={this.props.clientState.output}></Console>
                <LineInput execute={this.props.clientDispatch.execute}></LineInput>
            </div>
        );
    }
}
