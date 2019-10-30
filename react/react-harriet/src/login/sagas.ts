import { takeEvery, put, call } from 'redux-saga/effects'
import * as t from './actionTypes';
import { authorize, loginFailed } from './actions';
import { newClient } from '../clients/actions';
import { setLayout } from '../dashboard/actions'
import { userService } from '../_services/user.service';
import { SagaParams } from '../sagas';
import setupSocket from '../_sockets';

function* login(action : any, params : SagaParams)  {
    try {
        const { id, faction } = yield call(userService.login, action.userName, action.password)
        if (params.socket) { params.socket.close() }
        params.socket = setupSocket(params.dispatch, id);
        yield put(authorize(action.userName, faction, id));
        const layoutResp = yield call(userService.getRequest, 'environment/DASHBOARD');
        const layoutText = yield layoutResp.text();
        const layout = JSON.parse(layoutText);
        for (const client of layout.clients) {
            yield put(newClient(client.viewName, client.title, client.modelName, client.modelArgs));
        }
        yield put(setLayout(layout.boxes));
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
