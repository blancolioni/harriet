import { takeEvery, put, call } from 'redux-saga/effects'
import * as t from './actionTypes';
import { LoginAction, authorize, loginFailed } from './actions';
import { State } from './model';
import { userService } from '../_services/user.service';

function* login(action : any)  {
    try {
        const token = yield call(userService.login, action.userName, action.password)
        yield put(authorize(action.userName, token));
    } catch(error) {
        yield put(loginFailed(error));
    }
}

function *loginSaga() {
    yield takeEvery(t.LOGIN,login);
}

export default loginSaga
