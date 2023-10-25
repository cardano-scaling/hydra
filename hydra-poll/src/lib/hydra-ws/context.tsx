import React from 'react'

export interface HydraSocketService { socket: WebSocket }

export const HydraSocketContext: React.Context<HydraSocketService> =
    React.createContext({} as HydraSocketService)
