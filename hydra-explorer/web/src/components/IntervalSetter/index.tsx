"use client" // This is a client component 👈🏽

import { useIntervalContext } from "@/providers/IntervalProvider"
import { ChangeEvent } from "react"

interface IntervalSetterProps {
  className?: string
}

export default function IntervalSetter(props: IntervalSetterProps) {
  const { isAutoUpdate
    , intervalTime
    , toggleAutoUpdate
    , updateIntervalTime } = useIntervalContext()

  return (
    <div className={`col ${props.className}`}>
      <label className="block text-sm font-medium text-gray-200">Update Interval:</label>
      <div className="mt-1 flex items-center">
        <button
          type="button"
          onClick={toggleAutoUpdate}
          className={`px-4 py-2 rounded ${isAutoUpdate ? "bg-blue-500 hover:bg-blue-600" : "bg-green-500 hover:bg-green-600"
            } text-white`
          }
        >
          {isAutoUpdate ? "Pause ⏸" : "Resume ▶"}
        </button>
        <select
          value={intervalTime}
          onChange={(e: ChangeEvent<HTMLSelectElement>) => {
            const intervalValue = parseInt(e.target.value, 10)
            updateIntervalTime(intervalValue)
          }
          }
          className="ml-4 py-2 px-3 bg-gray-800 text-gray-200 rounded-md"
        >
          <option value={1000}>1 second</option>
          <option value={5000}>5 seconds</option>
          <option value={10000}>10 seconds</option>
        </select>
      </div>
    </div>
  )
}

