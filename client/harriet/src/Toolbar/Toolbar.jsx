import React from 'react';
import { userService } from '../_services';
import { connect } from 'react-redux'
import { logout } from '../redux/actions/login';
import { setSpeed } from '../redux/actions/status';

import '../Concorde.css';

function SpeedButton(props) {
    const mainClass = props.buttonSpeed == props.selectedSpeed ? "primary" : "secondary";
    const className = "btn btn-" + mainClass + " btn-sm";
    const buttonClasses = ["pause", "play", "forward"];
    const buttonClass = buttonClasses[props.buttonSpeed];

    return (
        <button className={className} onClick={props.onClick}><i className={"fas fa-" + buttonClass}></i></button>
    );
}

class Toolbar extends React.Component {

    handleLogout = () => {
        this.props.logout({});
    }

    changeSpeed = newSpeed => {
        console.log("update speed", newSpeed);
        userService.postRequest('status/updateSpeed/' + newSpeed)
            .then((result) => result.json())   
            .then((resp) => {
                this.props.setSpeed(resp.updateSpeed);
            });
    }

    render() {
        const selectedSpeed = this.props.status.updateSpeed;
        return (
            <nav className="navbar navbar-expand-lg navbar-dark bg-dark">
                <a className="navbar-brand" href="#">{this.props.user}</a>
                <div className="collapse navbar-collapse" id="navbarNavAltMarkup">
                    <span>
                        {this.props.status.timeImage}
                    </span>
                </div>
                <div className="btn-group">
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
                </div>
                <button className="btn btn-success" onClick={this.handleLogout}>Logout</button>
            </nav>
            );
    }
}

function mapStateToProps(state) {
    return {
      status: state.status,
      user: state.login.faction,
    };
  }

export default connect(
    mapStateToProps,
    { logout, setSpeed }
)(Toolbar)
