"use client" // This is a client component 👈🏽

import { TickState } from "@/app/model"
import useDataFetcher from "@/hooks/DataFetcher"
import { useState } from "react"

const TickBox = () => {
    const [tick, setTick] = useState<TickState | null>(null)
    const [error, setError] = useState<string | null>(null)

    useDataFetcher<TickState>({
        url: '/tick',
        setFetchedData: setTick,
        setError,
    })

    return (
        <div className="container mx-auto mt-8">
            {error ? (
                <p className="text-red-500">{error}</p>
            ) : (
                <div className="w-full">
                    <table className="table-fixed bg-gray-800 text-white rounded-lg">
                        <thead>
                            <tr>
                                <th className="text-center px-4 py-2">Block Number</th>
                                <th className="text-center px-4 py-2">Block Hash</th>
                                <th className="text-center px-4 py-2">Slot Number</th>
                            </tr>
                        </thead>
                        <tbody>
                            {tick ? (
                                <tr>
                                    <td className="truncate text-center border px-4 py-2">{tick?.blockNo}</td>
                                    <td className="truncate text-center border px-4 py-2">
                                        <a href={`https://sancho.cexplorer.io/block/${tick?.point.blockHash}`} target="_blank" className="text-blue-300 hover:text-blue-500">
                                            {tick?.point.blockHash}
                                        </a>
                                    </td>
                                    <td className="truncate text-center border px-4 py-2">{tick?.point.slot}</td>
                                </tr>
                            ) : null}
                        </tbody>
                    </table>
                </div>
            )}
        </div>
    )
}

export default TickBox
