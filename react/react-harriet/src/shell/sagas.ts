import { takeEvery, call, put } from 'redux-saga/effects'
import * as t from './actionTypes';
import { addToOutput, CommandAction } from './actions';
import { userService } from '../_services/user.service';
import { SagaParams } from '../sagas';

function* executeCommand(action : CommandAction, params : SagaParams)  {
    try {
        const response = yield call(userService.postRequest, 'client/' + action.clientId, {data: action.command})
        const result = yield response.json();
        console.log('executeCommand', action.clientId, result);
        yield put(addToOutput(action.clientId, result.standardOutput.concat (result.standardError)))
    } catch(error) {
    }
}

function *shellSaga(params : SagaParams) {
    yield takeEvery(t.COMMAND, (action : CommandAction) => executeCommand(action, params));
}

export default shellSaga
