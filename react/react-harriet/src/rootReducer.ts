import { combineReducers } from 'redux';
import login from './login';
import toolbar from './toolbar';
import dashboard from './dashboard';
import clients from './clients';
import shell from './shell';

const clientReducer = (rs : any[]) => {
  return function(state : any, action : any) {
    let s = state;
    for (const r of rs) {
      s = r(s, action);
    }
    return s;
  }
}

const rootReducer = combineReducers({
  [login.constants.NAME]: login.reducer,
  [toolbar.constants.NAME]: toolbar.reducer,
  [dashboard.constants.NAME]: dashboard.reducer,
  [clients.constants.NAME]: clients.reducer([shell.reducer]),
});

export default rootReducer
export type AppState = ReturnType<typeof rootReducer>