import React from 'react';
import { connect } from 'react-redux'

import { State } from '../model';
import { AppState } from '../../rootReducer';
import { ClientState } from '../../clients/model'
import Client from '../../clients/components';
import { execute } from '../../shell/actions';
import { requestUpdate } from '../../clients/actions';

interface DashboardProps {
    state : State,
    clients: ClientState[],
    execute: (clientId : number, command: string) => void,
    requestUpdate: (clientId : number, detail: number) => void,
}

class Dashboard extends React.Component<DashboardProps,State> {
    render() {
        let boxes = this.props.state.boxes;
        let top = boxes[0];
        return (
            <div className="concorde-dashboard-grid">
                {
                    top.mapLeaves (id => boxes[id], (box) => {
                        let anchor = box.anchor;
                        let cellStyle = {
                            gridColumnStart: anchor.left,
                            gridColumnEnd: anchor.right,
                            gridRowStart: anchor.top,
                            gridRowEnd: anchor.bottom,
                        }
                        
                        if (box.clientId >= 0) {
                            let clientState = this.props.clients[box.clientId];
                            let clientDispatch = {
                                execute: (command : string) => this.props.execute(box.clientId, command),
                                requestUpdate: (detail : number) => this.props.requestUpdate(box.clientId, detail),
                            }
                            return (
                                <div className="concorde-dashboard-cell" style={cellStyle} key={box.id}>
                                    <Client clientState={clientState} clientDispatch={clientDispatch}></Client>
                                </div>
                                );
                            } else {
                                return (
                                <div className="concorde-dashboard-cell" style={cellStyle} key={box.id}>
                                </div>
                                );
                            }
                        })
                }
            </div>
            );
    }
}

function mapStateToProps(state : AppState) {
    return {
        state: state.dashboard,
        clients: state.clients.clients,
    };
  }

export default connect(
    mapStateToProps,
    { execute, requestUpdate },
)(Dashboard)
