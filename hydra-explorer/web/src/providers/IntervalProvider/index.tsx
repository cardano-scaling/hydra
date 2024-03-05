"use client" // This is a client component 👈🏽

import React, { useContext, useState } from 'react'

export interface IntervalSettingService {
    isAutoUpdate: boolean
    intervalTime: number
    toggleAutoUpdate: () => void
    updateIntervalTime: (seconds: number) => void
}

const IntervalContext: React.Context<IntervalSettingService> =
    React.createContext({} as IntervalSettingService)

export const useIntervalContext = () => useContext(IntervalContext)

const IntervalSettingProvider: React.FC<any> = ({
    children
}) => {
    const [intervalTime, setIntervalTime] = useState(1000)
    const [isAutoUpdateOn, setAutoUpdate] = useState(false)

    const handleToggleAutoUpdate = () => {
        setAutoUpdate((prevAutoUpdate) => !prevAutoUpdate)
    }

    const handleIntervalTimeChange = (seconds: number) => {
        setIntervalTime(seconds)
    }

    return (
        <IntervalContext.Provider value={{
            isAutoUpdate: isAutoUpdateOn,
            intervalTime: intervalTime,
            toggleAutoUpdate: handleToggleAutoUpdate,
            updateIntervalTime: handleIntervalTimeChange
        }}>
            {children}
        </IntervalContext.Provider>
    )
}

export default IntervalSettingProvider
