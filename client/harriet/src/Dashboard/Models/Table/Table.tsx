import React from 'react';
import { connect } from 'react-redux'

import { userService } from '../../../_services';
import { DashboardItem, DashboardItemProps } from '../../DashboardItem';
import { TableState, initialTableState } from '../../../redux/clients/table';

import '../../../Concorde.css';

interface TableProps extends DashboardItemProps {
}

class Table extends React.Component<TableProps,TableState> {

    constructor(props : TableProps) {
        super(props);

        this.state = initialTableState;
        this.onConnected = this.onConnected.bind(this);
        this.onColumnHeaderClick = this.onColumnHeaderClick.bind(this);
        this.getData = this.getData.bind(this);
    }

    onConnected(clientId : number) {
        this.getData(clientId, this.state.sortColumn, this.state.sortAscending);
    }

    onColumnHeaderClick(index : number) {

    }

    getData(clientId : number, sortColumn : number, sortAscending : boolean) {

        userService.postRequest('client/' + clientId, {data: 'get', sort: sortColumn + 1, ascending: sortAscending})
            .then((result) => result.json())   
            .then((resp) => {
                this.setState(state => {
                    return {
                        ...state,
                        clientId: clientId,
                        headings: resp.table.headings,
                        data: resp.table.data,
                        sortColumn: sortColumn,
                        sortAscending: sortAscending,
                    }                    
                });
            });

    }

    // onConnected(clientId) {
    //     this.getData(clientId, -1, true);
    // }
    
    // onColumnHeaderClick(index) {
    //     this.getData(this.state.clientId, index, this.state.sortColumn === index ? !this.state.sortAscending : false);
    // }

    render() {
        return (
            <DashboardItem 
                modelName={this.props.modelName} 
                modelArg={this.props.modelArg} 
                boxId={this.props.boxId}
                onConnected={this.onConnected} 
                onDashboardCommand={this.props.onDashboardCommand}
            >
                <table className="table-sm concorde-sortable-table">
                    <thead>
                        <tr>
                            {this.state.headings.map((heading,index) => {
                                return (
                                    <th
                                            onClick={(e) => this.onColumnHeaderClick(index)}
                                            data-column-index={index}
                                        >
                                        {heading.label} {index === this.state.sortColumn && (this.state.sortAscending ? (<i className="fas fa-sort-up"></i>) : (<i className="fas fa-sort-down"></i>))}</th>
                                    );
                                })
                            }
                        </tr>
                    </thead>
                    <tbody>
                        {this.state.tableData.map((row) => {
                            return (<tr>
                                {this.state.headings.map((heading) => {
                                    return (<td>{row[heading.id]}</td>);
                                })}
                            </tr>);
                        })}
                    </tbody>
                </table>
            </DashboardItem>
        );
    }
}

export default connect(
    null,
    null
)(Table)
