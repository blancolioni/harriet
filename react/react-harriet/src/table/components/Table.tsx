import React from 'react';

import { State } from '../model';

import { ClientDispatch } from '../../clients/model';

interface TableDispatch extends ClientDispatch {
    sort : (columnIndex : number, ascending : boolean) => void
}

interface Props {
    clientState : State,
    clientDispatch : TableDispatch
}

export default class Table extends React.Component<Props,State> {
    
    render() {
        const headings = this.props.clientState.headings;
        const body = this.props.clientState.tableData;
        return (
            <table>
                <thead>
                    <tr>
                        {headings.map((heading) => {
                            return (<th key={heading.id}>{heading.label}</th>)
                        })}
                    </tr>
                </thead>
                <tbody>
                    {body.map((row,index) => {
                        return (
                            <tr key={index}>
                                {headings.map((heading) => {
                                    return (
                                        <td
                                          key={heading.id}
                                        >
                                            {row[heading.id].display}
                                        </td>)
                                })}
                            </tr>
                    )})}
                </tbody>
            </table>
        );
    }
}
