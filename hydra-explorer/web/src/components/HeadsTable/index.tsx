"use client" // This is a client component 👈🏽

import { HeadState } from "@/app/model"
import useDataFetcher from "@/hooks/DataFetcher"
import { useState } from "react"

const HeadsTable = () => {

    const [heads, setHeads] = useState<HeadState[]>([])
    const [error, setError] = useState<string | null>(null)

    useDataFetcher<HeadState[]>({
        url: '/heads',
        setFetchedData: setHeads,
        setError,
    })

    return (
        <div className="container mx-auto mt-8">
            {error ? (
                <p className="text-red-500">{error}</p>
            ) : (
                <div className="w-full">
                    <table className="table-fixed w-full bg-gray-800 text-white rounded-lg">
                        <thead>
                            <tr>
                                <th className="px-4 py-2">Head ID</th>
                                <th className="px-4 py-2">Status</th>
                                <th className="px-4 py-2">Last Updated At SlotNo</th>
                                <th className="px-4 py-2">Last Updated At BlockNo</th>
                                <th className="px-4 py-2">Last Updated At BlockHash</th>
                            </tr>
                        </thead>
                        <tbody>
                            {heads?.map((entry, index) => (
                                <tr key={index} className={`${index % 2 === 0 ? 'bg-gray-700' : 'bg-gray-600'}`}>
                                    <td className="truncate border px-4 py-2">{entry.headId}</td>
                                    <td className="truncate border px-4 py-2">{entry.status}</td>
                                    <td className="truncate border px-4 py-2">{entry.lastUpdatedAtPoint.slot}</td>
                                    <td className="truncate border px-4 py-2">{entry.lastUpdatedAtBlockNo}</td>
                                    <td className="truncate border px-4 py-2">{entry.lastUpdatedAtPoint.blockHash}</td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            )}
        </div>
    )
}

export default HeadsTable
