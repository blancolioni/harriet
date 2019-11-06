import { Dispatch, AnyAction } from 'redux'

import { logout } from '../login/actions';
import { updateToolbar } from '../toolbar/actions';
import { updateClient } from '../clients/actions';
import { updateOutline } from '../outline/actions';

const serverUrl = 'localhost:8080/socket';

const setupSocket = (dispatch : Dispatch<AnyAction>, token : string) => {

  console.log("connecting", serverUrl);

  const socket = new WebSocket('ws://' + serverUrl)

  socket.onopen = () => {
    socket.send(JSON.stringify({ id: token }));
  }

  socket.onclose = () => {
    dispatch(logout());
  }

  socket.onerror = () => {
    dispatch(logout());
  }


  socket.onmessage = (event) => {
    const data = JSON.parse(event.data)
    if (data.clients) {
      for (const client of data.clients) {
        dispatch(updateClient(client.clientId, client.update));
      }
    }
    console.log('onmessage', data)
    switch (data.payload.type) {
      case 'update-state':
        dispatch(updateToolbar(data.payload.currentTimeImage));
        break;
      case 'update-faction':
          dispatch(updateToolbar(undefined, data.payload.cash));
          break;
      case 'outline':
        dispatch(updateOutline(data.payload.outline))
      default:
        break
    }
  }

  return socket
}

export default setupSocket;