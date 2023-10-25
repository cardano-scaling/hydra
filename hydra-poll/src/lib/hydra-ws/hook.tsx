import { useCallback, useContext, useEffect } from "react"
import { HydraSocketContext } from "./context"
import {
    ClientConnected,
    ClientDisconnected,
    HydraEvent,
    HydraEventType,
    ServerOutput,
    Update
} from "./model/events"

export const useHydraEvent = (emitEvent: (evt: HydraEvent) => void) => {
    // get the socket instance
    const { socket } = useContext(HydraSocketContext)

    // Memoize emitEvent with useCallback
    const memoizedEmitEvent = useCallback(emitEvent, [])

    // when the component, *which uses this hook* mounts, add listeners.
    useEffect(() => {
        // Define the listener functions
        const handleOpen = (msg: Event) => {
            // console.log('[HydraEvent] connected', msg)
            memoizedEmitEvent({ tag: HydraEventType.ClientConnected } as ClientConnected)
        }

        const handleClose = (msg: CloseEvent) => {
            // console.log('[HydraEvent] disconnected', msg)
            memoizedEmitEvent({ tag: HydraEventType.ClientDisconnected } as ClientDisconnected)
        }

        const handleMessage = (event: MessageEvent) => {
            const output = JSON.parse(event.data) as ServerOutput
            console.log("[HydraEvent] ServerOutput", output)
            memoizedEmitEvent({ tag: HydraEventType.Update, output } as Update)
        }

        const handleError = (event: Event) => {
            console.error('[HydraEvent] WebSocket Error:', event)
        }

        // Add listeners when the component mounts
        socket.addEventListener('open', handleOpen)
        socket.addEventListener('close', handleClose)
        socket.addEventListener('message', handleMessage)
        socket.addEventListener('error', handleError)

        // remove all the listeners and close the socket when it unmounts
        return () => {
            socket.removeEventListener('open', handleOpen)
            socket.removeEventListener('close', handleClose)
            socket.removeEventListener('message', handleMessage)
            socket.removeEventListener('error', handleError)

            if (socket.readyState === WebSocket.OPEN) {
                socket.close()
            }
        }
    }, [socket, memoizedEmitEvent])
}
