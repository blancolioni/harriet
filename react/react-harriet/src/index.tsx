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
import toolbarSaga from './toolbar/sagas'
import setupSocket from './_sockets'


const sagaMiddleware = createSagaMiddleware()

const store = createStore(
  reducers,
  applyMiddleware(sagaMiddleware)
)

// const socket = setupSocket(store.dispatch)  

const sagaParams = { socket: null, dispatch: store.dispatch };
sagaMiddleware.run(loginSaga,  sagaParams);
sagaMiddleware.run(toolbarSaga, sagaParams);

ReactDOM.render(
  <Provider store={store}>
    <App />
  </Provider>,
  document.getElementById('root')
)
register();

