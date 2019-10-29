import React from 'react';
import { connect } from 'react-redux'

import Login from './login/components';

interface AppProps {
  loggedIn: boolean
}

function App(props : AppProps) {
    if (props.loggedIn) {
        return (
          <div></div>
        );
    } else {
        return (<Login/>);
    }
}

function mapStateToProps(state : any) : AppProps {
    return { loggedIn: state.login.loggedIn }
}

export default connect(mapStateToProps)(App);
