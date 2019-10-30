import { takeEvery, put, call } from 'redux-saga/effects'
import * as t from './actionTypes';
import { updateClient } from './actions';
import { userService } from '../_services/user.service';
import { SagaParams } from '../sagas';


function* onNewClient(action : any, params : SagaParams)  {
    const update = yield call(userService.getRequest, 'client/' + action.clientId);
    yield put(updateClient(action.clientId, update));
}

function *clientSaga(params : SagaParams) {
    yield takeEvery(t.NEW_CLIENT, (action) => onNewClient(action, params));
}

export default clientSaga
