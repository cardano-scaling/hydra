"use client" // This is a client component 👈🏽

import { useHeadsData } from "@/providers/HeadsDataProvider"

const HeadsTable = () => {
    const {heads, error} = useHeadsData()

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
