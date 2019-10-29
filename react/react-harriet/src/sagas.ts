import { Dispatch, AnyAction } from 'redux'

export interface SagaParams {
    socket   : WebSocket | null
    dispatch : Dispatch<AnyAction>
}
