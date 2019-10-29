import React from 'react';
import { connect } from 'react-redux'

import { State } from '../model';
import { AppState } from '../../rootReducer';

interface DashboardProps {
    state : State,
}

class Dashboard extends React.Component<DashboardProps,State> {
    render() {
        console.log('dashboard', this.props.state.boxes);
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
                        return (
                            <div className="concorde-dashboard-cell" style={cellStyle} key={box.id}>
                            </div>
                            );
                        })
                }
            </div>
            );
    }
}

function mapStateToProps(state: AppState) : DashboardProps {
    return {
        state: state.dashboard,
    };
  }

export default connect(
    mapStateToProps,
    { }
)(Dashboard)
