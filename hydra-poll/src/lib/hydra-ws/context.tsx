import React from 'react'

export interface HydraSocketService {
    socket: WebSocket
}

const HydraSocketContext: React.Context<HydraSocketService> =
    React.createContext({} as HydraSocketService)

export default HydraSocketContext