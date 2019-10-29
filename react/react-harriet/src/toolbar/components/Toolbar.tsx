import React from 'react';
import { connect } from 'react-redux'

import { logout } from '../../login/actions';

import { State } from '../model';
import { AppState } from '../../rootReducer';

interface ToolbarProps {
    currentDate : string,
    currentCash : string,
    userName : string,
    factionName : string,
    logout: typeof logout,
}

class Toolbar extends React.Component<ToolbarProps,State> {

    render() {
        return (
            <nav className="navbar navbar-expand-lg navbar-dark bg-dark">
                <a className="navbar-brand" href="#">{this.props.factionName}</a>
                <div className="collapse navbar-collapse" id="navbarNavAltMarkup">
                    <ul className="navbar-nav">
                        <li className="nav-item">
                            {this.props.currentDate}
                        </li>
                        <li className="nav-item">
                            <i className="fas fa-coins"></i>
                            {this.props.currentCash}
                        </li>
                    </ul>
                </div>
                {/* <div className="btn-group">
                    {[0,1,2].map(speed => {
                        return (
                            <SpeedButton 
                                key={speed}
                                buttonSpeed={speed}
                                selectedSpeed={selectedSpeed} 
                                onClick={() => this.changeSpeed(speed)}
                            >

                            </SpeedButton>
                        );
                    })}
                </div> */}
                <button className="btn btn-success" onClick={this.props.logout}>Logout</button>
            </nav>
            );
    }
}

function mapStateToProps(state: AppState) : ToolbarProps {
    return {
        currentDate : state.toolbar.dateImage,
        currentCash : state.toolbar.cashImage,
        userName : state.login.userName || '',
        factionName: state.login.factionName || '',
        logout: logout,
    };
  }

export default connect(
    mapStateToProps,
    { logout }
)(Toolbar)
