export interface NodeId { nodeId: string }

export interface Party { vkey: string }

export interface UTCTime { time: number }

export interface UTxOValueType { lovelace: number }

export interface UTxOType {
    address: string,
    datum?: string,
    datumhash?: string,
    inlineDatum?: string,
    referenceScript?: string,
    value: UTxOValueType
}

export type UTxO = Record<string, UTxOType>
