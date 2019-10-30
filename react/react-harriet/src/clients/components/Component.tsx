import React from 'react';
import { connect } from 'react-redux'

import { ClientState } from '../model';

interface ClientProps {
    clientState: ClientState
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

export default connect(
    null,
    null
)(Client)
