import React from 'react';
import { connect } from 'react-redux'

import Harriet from '../Harriet/Harriet';
import { LoginPage } from '../authentication';

function App(props) {
    if (props.loggedIn) {
        return (<Harriet></Harriet>);
    } else {
        return (<LoginPage></LoginPage>);
    }
}

function mapStateToProps(state) {
    return { loggedIn: state.login.loggedIn }
}

export default connect(mapStateToProps)(App);
