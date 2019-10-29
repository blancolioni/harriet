import React from 'react';
import { connect } from 'react-redux'

import { logout } from '../../login/actions';
import { setSpeed } from '../actions';

import { State } from '../model';
import { AppState } from '../../rootReducer';

interface ToolbarProps {
    currentDate : string,
    currentCash : string,
    userName : string,
    factionName : string,
    selectedSpeed : number,
    logout: typeof logout,
    setSpeed: typeof setSpeed,
}

type  SpeedProps  = {
    selected      : boolean
    buttonSpeed   : number
    onClick       : typeof setSpeed
    children : any
}

function SpeedButton(props : SpeedProps) {
    const mainClass = props.selected ? "primary" : "secondary";
    const className = "btn btn-" + mainClass + " btn-sm";
    const buttonClasses = ["pause", "play", "forward"];
    const buttonClass = buttonClasses[props.buttonSpeed];

    return (
        <button className={className} onClick={() => props.onClick(props.buttonSpeed)}><i className={"fas fa-" + buttonClass}></i></button>
    );
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
                <div className="btn-group">
                    {[0,1,2].map(speed => {
                        return (
                            <SpeedButton 
                                key={speed}
                                buttonSpeed={speed}
                                selected={this.props.selectedSpeed===speed} 
                                onClick={() => this.props.setSpeed(speed)}
                            >

                            </SpeedButton>
                        );
                    })}
                </div>
                <button className="btn btn-success" onClick={this.props.logout}>Logout</button>
            </nav>
            );
    }
}

function mapStateToProps(state: AppState)  {
    return {
        currentDate : state.toolbar.dateImage,
        currentCash : state.toolbar.cashImage,
        selectedSpeed: state.toolbar.updateSpeed,
        userName : state.login.userName || '',
        factionName: state.login.factionName || '',
    };
  }

export default connect(
    mapStateToProps,
    { logout, setSpeed }
)(Toolbar)
