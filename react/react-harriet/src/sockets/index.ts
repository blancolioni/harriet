import { Dispatch, AnyAction } from 'redux'

// import * as types from '../constants/ActionTypes'
// import { messageReceived, populateUsersList } from '../actions'

const serverUrl = 'localhost:8080/socket';

const setupSocket = (dispatch : Dispatch<AnyAction>) => {
  const socket = new WebSocket('ws://' + serverUrl)

  socket.onopen = () => {
    // socket.send(JSON.stringify({
    //   type: types.ADD_USER,
    //   name: username
    // }))
  }
  socket.onmessage = (event) => {
    const data = JSON.parse(event.data)
    switch (data.type) {
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

export default setupSocket
