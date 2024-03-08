"use client" // This is a client component 👈🏽

import { useIntervalContext } from '@/providers/IntervalProvider'
import { useEffect, useRef } from 'react'

export interface FetchDataOptions<T> {
    url: string
    setFetchedData: (data: T) => void
    setError: (error: string) => void
}

function useDataFetcher<T>({
    url,
    setFetchedData,
    setError,
}: FetchDataOptions<T>) {
    const { intervalTime, isAutoUpdate } = useIntervalContext()
    const fetchDataRef = useRef<() => void>()

    useEffect(() => {
        const fetchData = async () => {
            try {
                console.log(`Fetching Data from ${url}`)
                const response = await fetch(url)
                // The return value is *not* serialized
                // You can return Date, Map, Set, etc.
                if (!response.ok) {
                    // This will activate the closest `error.js` Error Boundary
                    throw new Error('Failed to fetch data')
                }
                const data: T = await response.json()
                setFetchedData(data)
            } catch (error) {
                setError('Error fetching data. Please try again later.')
            }
        }

        // Assign the fetchData function to the ref
        fetchDataRef.current = fetchData

        // Fetch data immediately and then set up interval if auto update is enabled
        if (isAutoUpdate) {
            fetchData()
        }

        // Set up interval to fetch data periodically
        const intervalId = setInterval(() => {
            if (isAutoUpdate && fetchDataRef.current) {
                fetchDataRef.current()
            }
        }, intervalTime)

        // Cleanup function to clear the interval
        return () => {
            clearInterval(intervalId)
        }
    }, [intervalTime, isAutoUpdate])

    return {}
}

export default useDataFetcher
