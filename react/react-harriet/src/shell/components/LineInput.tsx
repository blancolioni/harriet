import React from 'react';

interface Props {
    execute: (command : string) => void
}

interface State {
    input: string,
}

export default class Component extends React.Component<Props,State> {

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
            <input type="text" className="form-control" aria-label="Command" value={this.state.input} onKeyPress={this.handleKeyPress} onChange={this.handleTextChange} />
            </div>
        );
     }
}
