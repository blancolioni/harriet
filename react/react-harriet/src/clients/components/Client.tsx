import React from 'react';
import { connect } from 'react-redux'

import { ClientState } from '../model';
import Shell from '../../shell/components/Shell';

interface ViewTable {
    [key : string] : any
}

const viewTable : ViewTable = {
    Shell: Shell,
}

interface ClientProps {
    clientState: ClientState
}

interface TitleBarProps {
    title : string
    split : (horizontal : boolean) => void
    close : () => void
}

function ClientTitleBar(props : TitleBarProps) {

    return (
        <div className="concorde-dashboard-titlebar">
            <span>{props.title}</span>
            <span className="concorde-titlebar-right">
                <button className="concorde-titlebar-button" onClick={(e) => props.split(true)}>
                    <i className="fas fa-grip-lines-vertical"></i>
                </button>
                <button className="concorde-titlebar-button" onClick={() => props.split(false)}>
                    <i className="fas fa-grip-lines"></i>
                </button>
                <button className="concorde-titlebar-button" onClick={(e) => props.close()}>
                    <i className="fas fa-window-close"></i>
                </button>
            </span>
        </div>
    );
}

class Client extends React.Component<ClientProps,ClientState> {

    constructor(props : ClientProps) {
        super(props);
    }

    render() {
        const View = viewTable[this.props.clientState.viewName];
        return (
            <div className="concorde-dashboard-item">
                <ClientTitleBar title={this.props.clientState.title} split={(h) => {}} close={() => {}}></ClientTitleBar>
                <View clientState={this.props.clientState}>
                </View>
            </div>
            );
     }
}

export default connect(
    null,
    null
)(Client)
