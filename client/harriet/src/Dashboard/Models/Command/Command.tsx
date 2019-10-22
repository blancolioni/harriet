import React from 'react';
import { connect } from 'react-redux'
import { userService } from '../../../_services';
import { DashboardItem, DashboardItemProps, DashboardItemState, initialDashboardItemState } from '../../DashboardItem';
import { setClient } from '../../../redux/actions/boxes';

import '../../../Concorde.css';

interface CommandInputState {
    input : string
}

interface CommandInputProps {
    onCommand : (command : string) => void
}

class CommandInput extends React.Component<CommandInputProps, CommandInputState> {

    constructor(props : CommandInputProps) {
        super(props);

        this.state = {
            input : ''
        }

        this.handleTextChange = this.handleTextChange.bind(this);
        this.handleKeyPress = this.handleKeyPress.bind(this);
    }

    handleTextChange(e : React.ChangeEvent<HTMLInputElement>) {
        this.setState({
            input: e.target.value,
        });
    }

    handleKeyPress(e : React.KeyboardEvent<HTMLInputElement>) {
      if(e.charCode === 13){
          let cmd = this.state.input;
          //  e.target.value = '';
          this.setState({
              input: ''
          });
        this.props.onCommand(cmd);
      } 
    }
    
    render() {
      return (
            <div className="input-group mb-3">
                <div className="input-group-prepend">
                    <span className="input-group-text">{localStorage.getItem('admin') ? '&gt;' : '#'}</span>
                </div>
            <input type="text" className="form-control" aria-label="Command" onKeyPress={this.handleKeyPress} onChange={this.handleTextChange} />
            </div>
        );
     }
}

interface CommandState extends DashboardItemState  {
    output: string[]
}

interface CommandProps extends DashboardItemProps {
    setClient : (payload: any) => void
    controlHandler? : (control : string) => void

}

class Command extends React.Component<CommandProps,CommandState> {

    constructor(props : CommandProps) {
        super(props);

        this.state = {
            ...initialDashboardItemState,
            title: 'Command',
            output: [],
        }

        this.sendCommand = this.sendCommand.bind(this);
        this.onConnected = this.onConnected.bind(this);
    }

    sendCommand(cmd : string) {

        userService.postRequest('client/' + this.state.clientId, {data: cmd})
            .then((result) => result.json())   
            .then((resp) => {
                console.log('command resp', resp)
                if (resp.control) {
                    for(const cmd of resp.control) {
                        if (cmd.control === 'replace-view') {
                            console.log("replace view", cmd)
                            this.props.setClient({
                                boxId: this.props.boxId,
                                view: cmd.view,
                                model: cmd.model,
                                modelArgs: cmd.modelArgs,
                                title: cmd.view,
                                client: this.state.clientId,
                                });
                        }
                    }            
                }
                // if (this.props.controlHandler && resp.control) {
                //     this.props.controlHandler (resp.control);
                // }
            });
    }

    onConnected(clientId : number) {

        this.setState(state => {
            return {
                ...state,
                clientId: clientId,
            }
        });
    }

    render() {
        return (
            <DashboardItem 
                modelName="shell" 
                modelArg=""
                onConnected={this.onConnected} 
                onDashboardCommand={this.props.onDashboardCommand}
                boxId={this.props.boxId}
            >
                <CommandInput onCommand={this.sendCommand}/>
            </DashboardItem>
        );
    }
}

export default connect(
    null,
    { setClient }
)(Command)
