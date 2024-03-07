"use client" // This is a client component 👈🏽

import { useHeadsData } from '@/providers/HeadsDataProvider'
import { totalLovelaceValueLocked } from '@/utils'
import React from 'react'

const HeadsDashboard = () => {
    const { heads, error } = useHeadsData()

    const totalHeads = heads.length

    const totalLockedMoney = heads.reduce((total, head) => total + totalLovelaceValueLocked(head), 0)

    return (
        <div style={{ maxWidth: 'fit-content' }}>
            {error ? (
                <p className="text-red-500">{error}</p>
            ) : (
                <div className="bg-gray-800 shadow-md rounded-lg p-6">
                    <div className="grid grid-cols-2 gap-4">
                        <div className="border p-4">
                            <h3 className="text-center text-lg font-semibold mb-2">Total Heads</h3>
                            <p className="text-center text-xl">{totalHeads}</p>
                        </div>
                        <div className="border p-4">
                            <h3 className="text-center text-lg font-semibold mb-2">Total Value Locked</h3>
                            <p className="text-center text-xl">{totalLockedMoney / 1000000} ₳</p>
                        </div>
                    </div>
                </div>
            )}
        </div>
    )
}

export default HeadsDashboard
