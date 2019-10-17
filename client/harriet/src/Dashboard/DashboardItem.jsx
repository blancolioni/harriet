import React from 'react';
import DashboardTitleBar from './DashboardTitleBar';

import { userService } from '../_services';

class DashboardItem extends React.Component {

    constructor (props) {
        super(props);

        this.state = {
            loading: true,
        }
    }

    componentDidMount() {
        
        userService.postRequest('new-client', { model: this.props.model, modelArg: this.props.modelArg})
        .then(response => response.json())
        .then(
            result => {
                this.setState({
                    isLoaded: true,
                    clientId: result.clientId,
                });
                if (this.props.onConnected) {
                    this.props.onConnected (result.clientId);
                }
            }
        )
    }
    
    render() {
        if (!this.state.isLoaded) {
            return (
                <div className="concorde-loading">Loading ...</div>
            );
        } else {
            return (
                <div className="concorde-dashboard-item">
                    <DashboardTitleBar 
                        text={this.props.title}
                        clientId={this.state.clientId} 
                        boxId={this.props.boxId} 
                        onDashboardCommand={this.props.onDashboardCommand}
                    />
                    <div className="concorde-dashboard-body">
                        {this.props.children}
                    </div>
                </div>
            );
        }
    }
}

export default DashboardItem;