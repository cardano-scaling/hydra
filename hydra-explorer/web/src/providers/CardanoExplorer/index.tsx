"use client" // This is a client component 👈🏽

import React, { PropsWithChildren, useContext } from "react"

export interface CardanoExplorer {
    mintPolicy: (policyId: string) => string
    tx: (txIn: string) => string
    block: (blockHash: string) => string
    address: (addr: string) => string
}

const CardanoExplorerContext: React.Context<CardanoExplorer> =
    React.createContext({} as CardanoExplorer)

export const useCardanoExplorer = () => {
    const context = useContext(CardanoExplorerContext)
    if (!context) {
        throw new Error("useCardanoExplorer must be used within a CardanoExplorerProvider")
    }
    return context
}

export type CardanoExplorerProps = {
    network: string
}

export const CardanoExplorerProvider: React.FC<PropsWithChildren<CardanoExplorerProps>> =
    ({ network, children }) => {
        const cexplorer: CardanoExplorer = {
            mintPolicy: (policyId: string) => `https://${network}.cexplorer.io/policy/${policyId}/mint`,
            tx: (txIn: string) => `https://${network}.cexplorer.io/tx/${txIn}`,
            block: (blockHash: string) => `https://${network}.cexplorer.io/block/${blockHash}`,
            address: (addr: string) => `https://${network}.cexplorer.io/address/${addr}`
        }

        return (
            <CardanoExplorerContext.Provider value={cexplorer}>
                {children}
            </CardanoExplorerContext.Provider>
        )
    }
