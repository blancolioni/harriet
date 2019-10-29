import React from 'react';
import { connect } from 'react-redux'

import { logout } from '../../login/actions';

import { State } from '../model';
import { AppState } from '../../rootReducer';

import LineInput from './LineInput';

interface Props {
}

class Component extends React.Component<Props,State> {

    render() {
        return (
            <div>
                <LineInput></LineInput>
            </div>
        );
    }
}

function mapStateToProps(state: AppState) : Props {
    return {
    };
  }

export default connect(
    mapStateToProps,
    { logout }
)(Component)
