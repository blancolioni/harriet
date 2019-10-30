import React from 'react';
import { connect } from 'react-redux'

import { execute } from '../actions';

interface Props {
    lines: string[],
}

interface State {
}

export default class ConsoleText extends React.Component<Props,State> {

    render() {
        return (
            <div className="concorde-shell-output">
                <table className="concorde-shell-table">
                    <tbody>
                        {this.props.lines.map((line,index) => {
                            return (
                                <tr key={index}>
                                    <td>
                                        {line}
                                    </td>
                                </tr>
                            );
                        })}
                    </tbody>
                </table>
            </div>
        );
    }
}
