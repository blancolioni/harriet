import React from 'react';
import { connect } from 'react-redux'

import { execute } from '../actions';

interface Props {
    execute: typeof execute;
}

interface State {
    input: string,
}

class Component extends React.Component<Props,State> {

    constructor(props : Props) {
        super(props);
        this.state = { input: ''}
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
          this.setState({
              input: ''
          });
        this.props.execute(cmd);
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

export default connect(
    null,
    { execute }
)(Component)
