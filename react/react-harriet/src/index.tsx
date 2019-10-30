import React from 'react'
import ReactDOM from 'react-dom'
import { Provider } from 'react-redux'
import { createStore, applyMiddleware } from 'redux'
import createSagaMiddleware from 'redux-saga'

import './index.css'
import App from './App'
import { register } from './serviceWorker'
import reducers from './rootReducer'
import loginSaga from './login/sagas'
import shellSaga from './shell/sagas'
import toolbarSaga from './toolbar/sagas'
import clientSaga from './clients/sagas'


const sagaMiddleware = createSagaMiddleware()

const store = createStore(
  reducers,
  applyMiddleware(sagaMiddleware)
)

// const socket = setupSocket(store.dispatch)  

const sagaParams = { socket: null, dispatch: store.dispatch };
sagaMiddleware.run(loginSaga,  sagaParams);
sagaMiddleware.run(clientSaga, sagaParams);
sagaMiddleware.run(shellSaga, sagaParams);
sagaMiddleware.run(toolbarSaga, sagaParams);

ReactDOM.render(
  <Provider store={store}>
    <App />
  </Provider>,
  document.getElementById('root')
)
register();

