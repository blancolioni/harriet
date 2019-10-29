import { takeEvery, call } from 'redux-saga/effects'
import * as t from './actionTypes';
import { SetSpeedAction } from './actions';
import { userService } from '../_services/user.service';
import { SagaParams } from '../sagas';

function* setSpeed(action : SetSpeedAction, params : SagaParams)  {
    try {
        yield call(userService.postRequest, 'status/updateSpeed/' + action.newSpeed)
    } catch(error) {
    }
}

function *toolbarSaga(params : SagaParams) {
    yield takeEvery(t.SET_SPEED, (action : SetSpeedAction) => setSpeed(action, params));
}

export default toolbarSaga
