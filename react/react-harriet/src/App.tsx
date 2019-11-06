import React from 'react';
import { connect } from 'react-redux'

import Login from './login/components';
import Toolbar from './toolbar/components';
import Outline from './outline/components';
import Dashboard from './dashboard/components';

interface AppProps {
  loggedIn: boolean
}

function App(props : AppProps) {
    if (props.loggedIn) {
        return (
            <div className="wrapper">
                <Outline/>
                <div className="app-content">
                    <Toolbar/>
                    <Dashboard />
                </div>
            </div>
        );
    } else {
        return (<Login/>);
    }
}

function mapStateToProps(state : any) : AppProps {
    return { loggedIn: state.login.loggedIn }
}

export default connect(mapStateToProps)(App);
