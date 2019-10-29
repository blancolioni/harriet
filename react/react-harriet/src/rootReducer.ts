import { combineReducers } from 'redux';
import login from './login';
import toolbar from './toolbar';
import dashboard from './dashboard';

const rootReducer = combineReducers({
  [login.constants.NAME]: login.reducer,
  [toolbar.constants.NAME]: toolbar.reducer,
  [dashboard.constants.NAME]: dashboard.reducer,
});

export default rootReducer
export type AppState = ReturnType<typeof rootReducer>