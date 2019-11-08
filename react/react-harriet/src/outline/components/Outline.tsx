import React from 'react';
import { connect } from 'react-redux'
import TreeMenu from 'react-simple-tree-menu';

import { State } from '../model';
import { AppState } from '../../rootReducer';
import { zoomObject } from '../../clients/actions';

interface Props {
  treeData: State[],
  zoomObject: typeof zoomObject,
}


class Outline extends React.Component<Props,State> {

  treeNodes = (data : State[], maxLevel : number) : string[] => {
    let result : string[] = []
    let level = 1;
    const go = (path : string, items: State[]) => {
      for (const item of items) {
        result.push (path + item.key);
        if (level < maxLevel) {
          ++level;
          go (path + item.key + '/', item.nodes);
          --level;
        }
      }
    }
    go('', data);

    return result;

  }

    render() {
      const initNodes = this.treeNodes(this.props.treeData, 1);
        return (
            <nav id="outline" className="navbar-dark bg-dark">
                <div className="sidebar-header">
                    <h3>Harriet</h3>
                </div>
                <TreeMenu 
                   data={this.props.treeData}
                   hasSearch={false}
                   initialOpenNodes={initNodes}
                   resetOpenNodesOnDataUpdate={true}
                   onClickItem={(props) => { this.props.zoomObject(props.key.substring(props.key.lastIndexOf('/') + 1)) }}
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
    { zoomObject }
)(Outline)
