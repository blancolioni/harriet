import { combineReducers } from 'redux';
import login from './login';
import toolbar from './toolbar';
import dashboard from './dashboard';
import clients from './clients';
import shell from './shell';
import table from './table';

const rootReducer = combineReducers({
  [login.constants.NAME]: login.reducer,
  [toolbar.constants.NAME]: toolbar.reducer,
  [dashboard.constants.NAME]: dashboard.reducer,
  [clients.constants.NAME]: clients.reducer([shell.reducer, table.reducer]),
});

export default rootReducer
export type AppState = ReturnType<typeof rootReducer>