import React from 'react';
import DashboardTitleBar from './DashboardTitleBar';

import { userService } from '../_services';

export interface DashboardItemState {
    loading  : boolean
    clientId : number
    title    : string
}

export interface DashboardItemProps {
    modelName : string
    modelArg  : string
    boxId     : number
    onConnected? : (clientId : number) => void
    onDashboardCommand? : () => void
}

export const initialDashboardItemState = {
    loading : true,
    clientId: 0,
    title : "Loading"
}

export class DashboardItem extends React.Component<DashboardItemProps, DashboardItemState> {

    constructor (props : DashboardItemProps) {
        super(props);

        this.state = initialDashboardItemState;
    }

    componentDidMount() {
        
        userService.postRequest('new-client', { model: this.props.modelName, modelArg: this.props.modelArg})
        .then(response => response.json())
        .then(
            result => {
                this.setState({
                    loading: false,
                    clientId: result.clientId,
                });
                if (this.props.onConnected) {
                    this.props.onConnected (result.clientId);
                }
            }
        )
    }
    
    render() {
        if (this.state.loading) {
            return (
                <div className="concorde-loading">Loading ...</div>
            );
        } else {
            return (
                <div className="concorde-dashboard-item">
                    <DashboardTitleBar 
                        text={this.state.title}
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
