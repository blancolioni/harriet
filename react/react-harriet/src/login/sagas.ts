import { takeEvery, put, call } from 'redux-saga/effects'
import * as t from './actionTypes';
import { authorize, loginFailed } from './actions';
import { userService } from '../_services/user.service';
import { SagaParams } from '../sagas';
import setupSocket from '../_sockets';

function* login(action : any, params : SagaParams)  {
    try {
        const { id, faction } = yield call(userService.login, action.userName, action.password)
        if (params.socket) { params.socket.close() }
        params.socket = setupSocket(params.dispatch, id);
        yield put(authorize(action.userName, faction, id));
    } catch(error) {
        yield put(loginFailed(error));
    }
}

function* logout(params : SagaParams) {
    if (params.socket) {
        params.socket.close();
        params.socket = null;
    }
}

function *loginSaga(params : SagaParams) {
    yield takeEvery(t.LOGIN, (action) => login(action, params));
    yield takeEvery(t.LOGOUT, () => logout(params));
}

export default loginSaga
