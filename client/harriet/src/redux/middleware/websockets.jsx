import * as actions from '../actions/websocket';
import { setTime } from '../actions/status';
import { logout } from '../actions/login';

import { WS_CONNECT, WS_DISCONNECT, WS_DISCONNECTED } from '../actionTypes';

const serverName = 'localhost';
const port = '8080';

const serverUrl = 'http://' + serverName + ':' + port + '/';
const webSocketUrl = 'ws://' + serverName + ':' + port + '/';

const socketMiddleware = () => {
  let socket = null;

  const onOpen = store => (event) => {
    const id = store.getState().login.id;
    console.log('websocket open', id, event.target.url);
    socket.send(JSON.stringify({id}));
    store.dispatch(actions.wsConnected(event.target.url));
  };

  const onClose = store => () => {
    store.dispatch(actions.wsDisconnected());
    store.dispatch(logout());
  };

  const onMessage = store => (event) => {
    const payload = JSON.parse(event.data);
    console.log('receiving server message', payload, store.getState());

    switch (payload.type) {
      case 'update-state':
        store.dispatch(setTime({
          time: payload.currentTime,
          timeImage: payload.currentTimeImage,
        }));
        break;

      default:
        break;
    }
  };

  const onError = store => (event) => {
    console.log('error', store.getState(), event);
  };

  // the middleware part of this function
  return store => next => action => {
    switch (action.type) {
      case WS_CONNECT:
        if (socket !== null) {
          socket.close();
        }

        // connect to the remote host
        socket = new WebSocket(webSocketUrl + 'socket');

        // websocket handlers
        socket.onmessage = onMessage(store);
        socket.onclose = onClose(store);
        socket.onopen = onOpen(store);
        socket.onerror = onError(store);
        break;

      case WS_DISCONNECT:
        if (socket !== null) {
          socket.close();
        }
        socket = null;
        console.log('websocket closed');
        break;

      case WS_DISCONNECTED:
        console.log('websocket disconnected');
        break;

      // case 'NEW_MESSAGE':
      //   console.log('sending a message', action.msg);
      //   socket.send(JSON.stringify({ command: 'NEW_MESSAGE', message: action.msg }));
      //   break;

      default:
        console.log('the next action:', action);
        return next(action);
    }
  };
};

export default socketMiddleware();