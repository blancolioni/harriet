import * as actions from '../actions/websocket';
import { setTime } from '../actions/status';
import { logout } from '../login/login.tsx';
import { updateFaction } from '../actions/faction';

import { WS_CONNECT, WS_DISCONNECT, WS_DISCONNECTED } from '../actionTypes';

const serverName = 'localhost';
const port = '8080';

const serverUrl = 'http://' + serverName + ':' + port + '/';
const webSocketUrl = 'ws://' + serverName + ':' + port + '/';

const socketMiddleware = () => {
  let socket = null;

  const onOpen = store => (event) => {
    const id = store.getState().login.id;
    socket.send(JSON.stringify({id}));
    store.dispatch(actions.wsConnected(event.target.url));
  };

  const onClose = store => event => {
    store.dispatch(actions.wsDisconnected());
    store.dispatch(logout());
  };

  const onMessage = store => (event) => {
    const payload = JSON.parse(event.data);

    switch (payload.type) {
      case 'update-state':
        store.dispatch(setTime({
          time: payload.currentTime,
          timeImage: payload.currentTimeImage,
        }));
        break;

      case 'update-faction':
        store.dispatch(updateFaction({
          cash: payload.cash,
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
        break;

      case WS_DISCONNECTED:
        break;

      // case 'NEW_MESSAGE':
      //   console.log('sending a message', action.msg);
      //   socket.send(JSON.stringify({ command: 'NEW_MESSAGE', message: action.msg }));
      //   break;

      default:
        return next(action);
    }
  };
};

export default socketMiddleware();