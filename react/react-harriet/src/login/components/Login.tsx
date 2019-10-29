import React from 'react';
import { connect } from 'react-redux'

import { State } from '../model';
import { login } from '../actions';

const allowEmptyPassword = true;

interface LoginState {
    username : string;
    password : string;
    submitted : boolean;
    loading : boolean;
    error?: string;
    errorMessage?: string;
}

const initialState : LoginState = {
    username: '',
    password: '',
    submitted: false,
    loading: false,
};

interface LoginProps {
    login: (id : string, username : string) => void
}

class Login extends React.Component<LoginProps,LoginState> {
    constructor(props : LoginProps) {
        super(props);

        this.state = initialState;

        this.handleChange = this.handleChange.bind(this);
        this.handleSubmit = this.handleSubmit.bind(this);
    }

    handleChange(e : React.ChangeEvent<HTMLInputElement>) {
        const { name, value } = e.target;
        this.setState(state => {
           return { ...state,
                     [name]: value 
                  }
           });
    }

    handleSubmit(e : React.FormEvent<HTMLFormElement>) {
        e.preventDefault();

        const { username, password } = this.state;

        // stop here if form is invalid
        if (!(username && (password || allowEmptyPassword))) {
            return;
        }

        this.setState({ submitted: true, loading: true });

        this.props.login(username, password);

    }

    render() {
        const { username, password, submitted, loading, error, errorMessage } = this.state;

        return (
            <div className="jumbotron">
                <div className="container">
                    <div className="col-md-6 col-md-offset-3">
                        <h2>Harriet Login</h2>
                        <form name="form" onSubmit={this.handleSubmit}>
                            <div className={'form-group' + (submitted && !username ? ' has-error' : '')}>
                                <label htmlFor="username">Username</label>
                                <input type="text" className="form-control" name="username" value={username} onChange={this.handleChange} />
                                {submitted && !username &&
                                    <div className="help-block">Username is required</div>
                                }
                            </div>
                            <div className={'form-group' + (submitted && !password ? ' has-error' : '')}>
                                <label htmlFor="password">Password</label>
                                <input type="password" className="form-control" name="password" value={password} onChange={this.handleChange} />
                                {submitted && !password && !allowEmptyPassword &&
                                    <div className="help-block">Password is required</div>
                                }
                            </div>
                            <div className="form-group">
                                <button className="btn btn-primary" disabled={loading}>Login</button>
                                {loading &&
                                    <img alt="" src="data:image/gif;base64,R0lGODlhEAAQAPIAAP///wAAAMLCwkJCQgAAAGJiYoKCgpKSkiH/C05FVFNDQVBFMi4wAwEAAAAh/hpDcmVhdGVkIHdpdGggYWpheGxvYWQuaW5mbwAh+QQJCgAAACwAAAAAEAAQAAADMwi63P4wyklrE2MIOggZnAdOmGYJRbExwroUmcG2LmDEwnHQLVsYOd2mBzkYDAdKa+dIAAAh+QQJCgAAACwAAAAAEAAQAAADNAi63P5OjCEgG4QMu7DmikRxQlFUYDEZIGBMRVsaqHwctXXf7WEYB4Ag1xjihkMZsiUkKhIAIfkECQoAAAAsAAAAABAAEAAAAzYIujIjK8pByJDMlFYvBoVjHA70GU7xSUJhmKtwHPAKzLO9HMaoKwJZ7Rf8AYPDDzKpZBqfvwQAIfkECQoAAAAsAAAAABAAEAAAAzMIumIlK8oyhpHsnFZfhYumCYUhDAQxRIdhHBGqRoKw0R8DYlJd8z0fMDgsGo/IpHI5TAAAIfkECQoAAAAsAAAAABAAEAAAAzIIunInK0rnZBTwGPNMgQwmdsNgXGJUlIWEuR5oWUIpz8pAEAMe6TwfwyYsGo/IpFKSAAAh+QQJCgAAACwAAAAAEAAQAAADMwi6IMKQORfjdOe82p4wGccc4CEuQradylesojEMBgsUc2G7sDX3lQGBMLAJibufbSlKAAAh+QQJCgAAACwAAAAAEAAQAAADMgi63P7wCRHZnFVdmgHu2nFwlWCI3WGc3TSWhUFGxTAUkGCbtgENBMJAEJsxgMLWzpEAACH5BAkKAAAALAAAAAAQABAAAAMyCLrc/jDKSatlQtScKdceCAjDII7HcQ4EMTCpyrCuUBjCYRgHVtqlAiB1YhiCnlsRkAAAOwAAAAAAAAAAAA==" />
                                }
                            </div>
                            {error &&
                                <div className={'alert alert-danger'}>{error} {errorMessage} </div>
                            }
                        </form>
                    </div>
                </div>
            </div>
        );
    }
}

export default connect(
    null,
    { login }
)(Login)
