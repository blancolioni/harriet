import { combineReducers } from 'redux';
import login from './login';
import toolbar from './toolbar';

const rootReducer = combineReducers({
  [login.constants.NAME]: login.reducer,
  [toolbar.constants.NAME]: toolbar.reducer
});

export default rootReducer
export type AppState = ReturnType<typeof rootReducer>