export interface ChainPoint {
    blockHash: string
    slot: string
}

export interface HeadState {
    headId: string
    status: string
    lastUpdatedAtPoint: ChainPoint
    lastUpdatedAtBlockNo: number
}

export interface TickState {
    point: ChainPoint
    blockNo: number
}
