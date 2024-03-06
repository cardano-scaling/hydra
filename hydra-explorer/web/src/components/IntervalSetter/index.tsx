"use client" // This is a client component 👈🏽

import { useIntervalContext } from "@/providers/IntervalProvider"
import { ChangeEvent } from "react"

const IntervalSetter = () => {
  const { isAutoUpdate
    , intervalTime
    , toggleAutoUpdate
    , updateIntervalTime } = useIntervalContext()

  return (
    <div className="flex">
      <div className="mt-16">
        <button
          type="button"
          onClick={toggleAutoUpdate}
          className={`px-4 py-2 rounded ${isAutoUpdate ? "bg-blue-500 hover:bg-blue-600" : "bg-green-500 hover:bg-green-600"
            } text-white`
          }
        >
          {isAutoUpdate ? "Pause ⏸" : "Resume ▶"}
        </button>
      </div>

      <div className="mt-9">
        <label className="px-4 text-sm font-medium text-gray-200">Update Interval:</label>
        <div className="ml-4 py-2">
          <select
            value={intervalTime}
            onChange={(e: ChangeEvent<HTMLSelectElement>) => {
              const intervalValue = parseInt(e.target.value, 10)
              updateIntervalTime(intervalValue)
            }
            }
            className="py-2 px-3 bg-gray-800 text-gray-200 rounded-md"
          >
            <option value={1000}>1 second</option>
            <option value={5000}>5 seconds</option>
            <option value={10000}>10 seconds</option>
          </select>
        </div>
      </div>

    </div>
  )
}

export default IntervalSetter