import React from 'react';
import { userService } from '../../../_services';
import DashboardItem from '../../DashboardItem';

import '../../../Concorde.css';

class CommandInput extends React.Component {

    constructor(props) {
        super(props);
        this.state = { input: ''}
        this.handleTextChange = this.handleTextChange.bind(this);
        this.handleKeyPress = this.handleKeyPress.bind(this);
    }

    handleTextChange(e) {
        this.setState({
            input: e.target.value,
        });
    }

    handleKeyPress(e) {
      if(e.charCode === 13){
          let cmd = this.state.input;
          e.target.value = '';
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

class Command extends React.Component {

    constructor(props) {
        super(props);

        this.state = {
            output: [],
            clientId: 0,
        }

        this.sendCommand = this.sendCommand.bind(this);
        this.onConnected = this.onConnected.bind(this);
    }

    sendCommand(cmd) {

        userService.postRequest('client/' + this.state.clientId, {data: cmd})
            .then((result) => result.json())   
            .then((resp) => {
                if (this.props.controlHandler && resp.control) {
                    this.props.controlHandler (resp.control);
                }
            });
    }

    onConnected(clientId) {

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
                title="Command" 
                model="shell" 
                onConnected={this.onConnected} 
                onDashboardCommand={this.props.onDashboardCommand}
                boxId={this.props.boxId}
            >
                <CommandInput onCommand={this.sendCommand}/>
            </DashboardItem>
        );
    }
}

export { Command };
