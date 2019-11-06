import React from 'react';
import { connect } from 'react-redux'
import TreeMenu from 'react-simple-tree-menu';

import { State } from '../model';
import { AppState } from '../../rootReducer';

interface Props {
  treeData: State[]
}


class Outline extends React.Component<Props,State> {

    render() {
        return (
            <nav id="outline" className="navbar-dark bg-dark">
                <div className="sidebar-header">
                    <h3>Harriet</h3>
                </div>
                <TreeMenu 
                   data={this.props.treeData}
                   hasSearch={false}
                />
            </nav>
            );
    }
}

function mapStateToProps(state: AppState)  {
    return {
      treeData: state.outline.nodes,
    };
  }

export default connect(
    mapStateToProps,
    {  }
)(Outline)
