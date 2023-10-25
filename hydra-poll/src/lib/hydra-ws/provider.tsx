import React, { PropsWithChildren, useMemo, useRef } from "react"
import { HydraSocketContext } from './context'

export type UserHydraSocketOptions = { url: string }

const HydraSocketProvider: React.FC<PropsWithChildren<UserHydraSocketOptions>> = ({
    children,
    url,
}) => {
    const options = useMemo(() => ({ url }), [url])

    // we use a ref to store the socket as it won't be updated frequently
    const socketRef = useRef<WebSocket | null>(null)

    const initializeWebSocket: () => WebSocket = () => {
        const url = new URL(options.url)
        return new WebSocket(url)
    }

    if (!socketRef.current) {
        // Create and initialize your WebSocket connection here
        socketRef.current = initializeWebSocket()
    }

    return (
        <HydraSocketContext.Provider value={{ socket: socketRef.current! }}>
            {children}
        </HydraSocketContext.Provider>
    )
}

export default HydraSocketProvider
