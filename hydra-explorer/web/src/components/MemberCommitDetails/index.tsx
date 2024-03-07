"use client" // This is a client component 👈🏽

import React, { useEffect } from 'react'
import { HeadMember } from '@/app/model'

interface MemberCommitDetailsProps {
  member: HeadMember
  onClose: () => void
}

const MemberCommitDetails: React.FC<MemberCommitDetailsProps> = ({ member, onClose }) => {
  useEffect(() => {
    const handleKeyPress = (event: KeyboardEvent) => {
      if (event.key === 'Escape') {
        onClose()
      }
    }

    window.addEventListener('keydown', handleKeyPress)

    return () => {
      window.removeEventListener('keydown', handleKeyPress)
    }
  }, [onClose])

  return (
    <div className="fixed inset-0 flex items-center justify-center bg-gray-900 bg-opacity-50 z-50">
      <div className="bg-gray-800 p-6 rounded-lg shadow-xl relative">
        <button
          className="absolute top-4 right-4 bg-red-500 hover:bg-red-700 text-white font-bold py-2 px-4 rounded"
          onClick={onClose}
        >
          Close
        </button>
        <h2 className="text-2xl font-bold mb-4">Member Commits</h2>
        <div className="border p-4 col-span-2">
          <table className="w-full">
            <thead>
              <tr>
                <th className="text-center px-4 py-2">Tx In</th>
                <th className="text-center px-4 py-2">Address</th>
                <th className="text-center px-4 py-2">Value</th>
              </tr>
            </thead>
            <tbody>
              {member.commits &&
                Object.entries(member.commits).map(([commitId, commit], index) => (
                  <tr key={index} className={`${index % 2 === 0 ? 'bg-gray-700' : 'bg-gray-600'}`}>
                    <td className="truncate text-center border px-4 py-2">{commitId}</td>
                    <td className="truncate text-center border px-4 py-2">{commit.address}</td>
                    <td className="truncate text-center border px-4 py-2">{commit.value.lovelace / 1000000} ₳</td>
                  </tr>
                ))}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  )
}

export default MemberCommitDetails
