import React from 'react';
import { connect } from 'react-redux'

import { Toolbar } from '../Toolbar';
import Dashboard from '../Dashboard/Dashboard';
import { wsConnect } from '../redux/actions/websocket';

class Harriet extends React.Component {

    componentDidMount() {
        const { id } = this.props;
        if (id) {
            this.subscribeToServer();
        }
    }

    subscribeToServer = () => {
        const { id, wsConnect } = this.props;
        console.log(id,wsConnect);
        wsConnect(id);
    }

    render() {
        return (
            <div>
                <Toolbar></Toolbar>
                <Dashboard></Dashboard>
            </div>
        );
    }

    mapState
}

function mapStateToProps(state) {
    return {
      id: state.login.id,
    };
  }

export default connect(
    mapStateToProps,
    { wsConnect }
)(Harriet);

