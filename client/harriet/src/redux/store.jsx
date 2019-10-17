import { createStore, applyMiddleware } from "redux";
import reduxThunk from 'redux-thunk';

import rootReducer from "./reducers/index";
import wsMiddleware from './middleware/websockets';

const middleware = [reduxThunk, wsMiddleware];

export default createStore(rootReducer, applyMiddleware(...middleware));
