"use client" // This is a client component 👈🏽

import React, { useState } from 'react'
import { HeadState } from "@/app/model"
import { useHeadsData } from "@/providers/HeadsDataProvider"
import HeadDetails from "../HeadDetails"

const HeadsTable: React.FC = () => {
    const { heads, error } = useHeadsData()
    const [selectedHead, setSelectedHead] = useState<HeadState | null>(null)

    const handleRowClick = (head: HeadState) => {
        setSelectedHead(head)
    }

    const totalValueLocked = (head: HeadState) => {
        let totalLockedMoney = 0
        head.members?.forEach((member) => {
            if (member.commits && Object.keys(member.commits).length > 0) {
                Object.values(member.commits).forEach((commit) => {
                    totalLockedMoney += (commit.value.lovelace) 
                })
            }
        })
        return totalLockedMoney;
    }

    return (
        <div className="container mx-auto mt-12">
            {error ? (
                <p className="text-red-500">{error}</p>
            ) : (
                <div className="w-full">
                    <table className="table-fixed w-full bg-gray-800 text-white rounded-lg">
                        <thead>
                            <tr>
                                <th className="text-center px-4 py-2">Head ID</th>
                                <th className="text-center px-4 py-2">Status</th>
                                <th className="text-center px-4 py-2">Slot Number</th>
                                <th className="text-center px-4 py-2">Block Number</th>
                                <th className="text-center px-4 py-2">Block Hash</th>
                                <th className="text-center px-4 py-2">Value Locked</th>
                                <th className="text-center px-4 py-2">Actions</th>
                            </tr>
                        </thead>
                        <tbody>
                            {heads?.map((entry, index) => (
                                <tr key={index} className={`${index % 2 === 0 ? 'bg-gray-700' : 'bg-gray-600'}`}>
                                    <td className="truncate text-center border px-4 py-2">{entry.headId}</td>
                                    <td className="truncate text-center border px-4 py-2">{entry.status}</td>
                                    <td className="truncate text-center border px-4 py-2">{entry.point.slot}</td>
                                    <td className="truncate text-center border px-4 py-2">{entry.blockNo}</td>
                                    <td className="truncate text-center border px-4 py-2">{entry.point.blockHash}</td>
                                    <td className="truncate text-center border px-4 py-2">{totalValueLocked(entry) / 1000000} ₳</td>
                                    <td className="text-center border px-4 py-2">
                                        <button
                                            className="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
                                            onClick={() => handleRowClick(entry)}
                                        >
                                            Details
                                        </button>
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            )}
            {selectedHead && <HeadDetails head={selectedHead} onClose={() => setSelectedHead(null)} />}
        </div>
    )
}

export default HeadsTable