import React from 'react';

interface Props {
    lines: string[],
}

const ConsoleText = (props : Props) => {
    console.log('ConsoleText', props.lines)
    return (
        <div className="concorde-shell-output">
            <table className="concorde-shell-table">
                <tbody>
                    {props.lines.map((line,index) => {
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
};

export default ConsoleText;