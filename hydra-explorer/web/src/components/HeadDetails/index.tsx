"use client" // This is a client component 👈🏽

import React, { useState, useEffect } from 'react'
import { HeadState, HeadMember } from '@/app/model'
import MemberCommitDetails from '../MemberCommitDetails'

interface HeadDetailsProps {
    head: HeadState
    onClose: () => void
}

const HeadDetails: React.FC<HeadDetailsProps> = ({ head, onClose }) => {
    const [selectedMember, setSelectedMember] = useState<HeadMember | null>(null)
    const [showMemberCommitDetails, setShowMemberCommitDetails] = useState(false)

    const handleMemberClick = (member: HeadMember) => {
        setSelectedMember(member)
        setShowMemberCommitDetails(true)
    }

    useEffect(() => {
        const handleKeyPress = (event: KeyboardEvent) => {
            if (event.key === 'Escape') {
                if (showMemberCommitDetails) {
                    setShowMemberCommitDetails(false)
                } else {
                    onClose()
                }
            }
        }

        window.addEventListener('keydown', handleKeyPress)

        return () => {
            window.removeEventListener('keydown', handleKeyPress)
        }
    }, [onClose, showMemberCommitDetails])

    return (
        <div className="fixed inset-0 flex items-center justify-center bg-gray-900 bg-opacity-50 z-50">
            <div className="bg-gray-800 p-6 rounded-lg shadow-xl relative">
                <button
                    className="absolute top-4 right-4 bg-red-500 hover:bg-red-700 text-white font-bold py-2 px-4 rounded"
                    onClick={onClose}
                >
                    Close
                </button>
                <h2 className="text-2xl font-bold mb-4">Head Details</h2>
                <div className="grid grid-cols-2 gap-4">
                    <div className="border p-4">
                        <h3 className="text-lg font-semibold mb-2">Head ID</h3>
                        <p>{head.headId}</p>
                    </div>
                    <div className="border p-4">
                        <h3 className="text-lg font-semibold mb-2">Seed Tx In</h3>
                        <p>{head.seedTxIn}</p>
                    </div>
                    <div className="border p-4">
                        <h3 className="text-lg font-semibold mb-2">Status</h3>
                        <p>{head.status}</p>
                    </div>
                    <div className="border p-4">
                        <h3 className="text-lg font-semibold mb-2">Contestation Period</h3>
                        <p>{head.contestationPeriod}</p>
                    </div>
                    <div className="border p-4">
                        <h3 className="text-lg font-semibold mb-2">Contestations</h3>
                        <p>{head.contestations}</p>
                    </div>
                    <div className="border p-4">
                        <h3 className="text-lg font-semibold mb-2">Snapshot Number</h3>
                        <p>{head.snapshotNumber}</p>
                    </div>
                    <div className="border p-4">
                        <h3 className="text-lg font-semibold mb-2">Contestation Deadline</h3>
                        <p>{head.contestationDeadline}</p>
                    </div>
                    <div className="border p-4">
                        <h3 className="text-lg font-semibold mb-2">Point</h3>
                        <p>
                            Block Hash: {head.point.blockHash} <br />
                            Slot: {head.point.slot}
                        </p>
                    </div>
                    <div className="border p-4 col-span-2">
                        <h3 className="text-lg font-semibold mb-2">Members</h3>
                        <table className="w-full">
                            <thead>
                                <tr>
                                    <th className="text-center px-4 py-2">On Chain ID</th>
                                    <th className="text-center px-4 py-2">Party VKey</th>
                                    <th className="text-center px-4 py-2">Total Committed Lovelace</th>
                                    <th className="text-center px-4 py-2">View UTxO Details</th>
                                </tr>
                            </thead>
                            <tbody>
                                {head.members?.map((member, index) => (
                                    <tr key={index} className={`${index % 2 === 0 ? 'bg-gray-700' : 'bg-gray-600'}`}>
                                        <td className="truncate text-center border px-4 py-2">{member.onChainId}</td>
                                        <td className="truncate text-center border px-4 py-2">{member.party.vkey}</td>
                                        <td className="truncate text-center border px-4 py-2">{getTotalCommittedLovelace(member)}</td>
                                        <td className="text-center border px-4 py-2">
                                            <button
                                                className="bg-blue-500 hover:bg-blue-700 text-white font-bold py-1 px-2 rounded"
                                                onClick={() => handleMemberClick(member)}
                                            >
                                                View
                                            </button>
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    </div>
                </div>
            </div>
            {selectedMember && showMemberCommitDetails && <MemberCommitDetails member={selectedMember} onClose={() => setShowMemberCommitDetails(false)} />}
        </div>
    )
}

function getTotalCommittedLovelace(member: HeadMember): number {
    if (!member.commits) return 0
    return Object.values(member.commits).reduce((total, commit) => total + commit.value.lovelace, 0)
}

export default HeadDetails
