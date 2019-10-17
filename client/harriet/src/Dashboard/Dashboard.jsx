import React from 'react';
import { connect } from 'react-redux'

import { mapLeaves } from './Box';
import { userService } from '../_services';
import { initBoxes } from '../redux/actions/boxes';

import DashboardCell from './DashboardCell';

class Dashboard extends React.Component {

    componentDidMount() {
        userService.getRequest('environment/dashboard')
        .then(response => response.json())
        .then(response => {
            console.log(response)
            this.props.initBoxes(response)
        });
    }

    render() {
        if (!this.props.boxes) {
            return (<div>Loading ...</div>);
        } else {
            return (
                <div className="concorde-dashboard-grid">
                    {
                        mapLeaves (this.props.boxes[0], id => this.props.boxes[id], (box) => {
                            return (
                                <DashboardCell 
                                    key={box.id} 
                                    boxId={box.id}
                                />
                                );
                            })
                    }
                </div>
            );
        }
    }
}

function mapStateToProps(state) {
    return {
      boxes: state.boxes.boxes
    };
  }
export default connect(
    mapStateToProps,
    { initBoxes }
)(Dashboard)
