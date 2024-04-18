"use client" // This is a client component 👈🏽

import { HeadState } from '@/app/model'
import useDataFetcher from '@/hooks/DataFetcher'
import React, { useContext, useState } from 'react'

export interface HeadsDataService {
  heads: HeadState[],
  error: string | null
}

const HeadsDataContext: React.Context<HeadsDataService> =
  React.createContext({} as HeadsDataService)

export const useHeadsDataContext = () => {
  const context = useContext(HeadsDataContext)
  if (!context) {
      throw new Error("useHeadsDataContext must be used within a HeadsDataProvider")
  }
  return context
}

export const HeadsDataProvider: React.FC<any> = ({
    children
}) => {
  const [heads, setHeads] = useState<HeadState[]>([])
  const [error, setError] = useState<string | null>(null)

  useDataFetcher<HeadState[]>({
      url: '/heads',
      setFetchedData: setHeads,
      setError,
  })

  return (
    <HeadsDataContext.Provider value={{heads: heads, error: error}}>
      {children}
    </HeadsDataContext.Provider>
  )
}

export const useHeadsData = () => useContext(HeadsDataContext)
