import React from 'react';
import { connect } from 'react-redux'

import { logout } from '../../login/actions';

import { State } from '../model';

import Console from './Console';
import LineInput from './LineInput';

interface Props {
    clientState : State,
}

class Component extends React.Component<Props,State> {

    render() {
        return (
            <div>
                <Console lines={this.props.clientState.output}></Console>
                <LineInput></LineInput>
            </div>
        );
    }
}

export default connect(
    null,
    { logout }
)(Component)
