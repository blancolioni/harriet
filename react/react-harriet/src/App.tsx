import React from 'react';
import { connect } from 'react-redux'

import Login from './login/components';
import Toolbar from './toolbar/components';
import Dashboard from './dashboard/components';

interface AppProps {
  loggedIn: boolean
}

function App(props : AppProps) {
    if (props.loggedIn) {
        return (
            <div>
             <Toolbar/>
             <Dashboard />
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
