import React from 'react';
import { connect } from 'react-redux'

interface ClientProps {
}

interface ClientState {
}

class Client extends React.Component<ClientProps,ClientState> {

    constructor(props : ClientProps) {
        super(props);
    }

    render() {
      return (
          <div>
              {this.props.children}
          </div>
        );
     }
}

class ClientList extends React.Component<ClientProps[], ClientState[]> {

}

export default connect(
    null,
    null
)(ClientList)
