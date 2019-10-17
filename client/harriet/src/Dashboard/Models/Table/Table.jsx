import React from 'react';
import { userService } from '../../../_services';
import DashboardItem from '../../DashboardItem';

import '../../../Concorde.css';

class Table extends React.Component {

    constructor(props) {
        super(props);

        this.state = {
            tableData: [],
            headings: [],
            data: [],
            clientId: 0,
            sortColumn: -1,
            sortAscending: true,
        }

        this.onConnected = this.onConnected.bind(this);
        this.onColumnHeaderClick = this.onColumnHeaderClick.bind(this);
    }

    getData(clientId, sortColumn, sortAscending) {

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

    onConnected(clientId) {
        this.getData(clientId, -1, true);
    }
    
    onColumnHeaderClick(index) {
        this.getData(this.state.clientId, index, this.state.sortColumn === index ? !this.state.sortAscending : false);
    }

    render() {
        return (
            <DashboardItem title={this.props.title} model={this.props.model} modelArg={this.props.modelArgs} onConnected={this.onConnected} onDashboardCommand={this.props.onDashboardCommand}>
                <table className="table-sm concorde-sortable-table">
                    <thead>
                        <tr>
                            {this.state.headings.map((heading,index) => {
                                return (
                                    <th
                                        onClick={(e) => this.onColumnHeaderClick(index)}
                                        columnIndex={index}
                                    >
                                        {heading.label} {index === this.state.sortColumn && (this.state.sortAscending ? (<i class="fas fa-sort-up"></i>) : (<i class="fas fa-sort-down"></i>))}</th>
                                    );
                                })
                            }
                        </tr>
                    </thead>
                    <tbody>
                        {this.state.data.map((row) => {
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

export { Table };
