export interface ChainPoint {
    blockHash: string
    slot: string
}

export interface HeadState {
    headId: string
    status: string
    point: ChainPoint
    blockNo: number
}

export interface TickState {
    point: ChainPoint
    blockNo: number
}
