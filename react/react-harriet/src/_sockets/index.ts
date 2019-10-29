import { Dispatch, AnyAction } from 'redux'

import { logout } from '../login/actions';
import { updateToolbar } from '../toolbar/actions';

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
    console.log(data);
    switch (data.payload.type) {
      case 'update-state':
        dispatch(updateToolbar(data.payload.currentTimeImage));
      // case types.ADD_MESSAGE:
      //   dispatch(messageReceived(data.message, data.author))
      //   break
      // case types.USERS_LIST:
      //   dispatch(populateUsersList(data.users))
      //   break
      default:
        break
    }
  }

  return socket
}

export default setupSocket;