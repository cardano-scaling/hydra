import { NodeId, Party, UTCTime, UTxO } from "./types"

export interface Greetings {
    tag: string
    me: Party
    // seq: number
    // timestamp: Date
}

export interface PeerConnected {
    tag: string
    peer: NodeId
    // seq: number
    // timestamp: Date
}

export interface PeerDisconnected {
    tag: string
    peer: NodeId
    // seq: number
    // timestamp: Date
}

export interface ReadyToCommit {
    tag: string
    parties: Party[]
    // seq: number
    // timestamp: Date
}

export interface Committed {
    tag: string
    party: Party
    utxo: UTxO
    // seq: number
    // timestamp: Date
}

export interface HeadIsAborted {
    tag: string
    utxo: UTxO
    // seq: number
    // timestamp: Date
}

export interface RolledBack {
    tag: string
    // seq: number
    // timestamp: Date
}

export interface TxValid {
    tag: string
    // headId: string
    transaction: any
    // seq: number
    // timestamp: Date
}

export type ServerOutput =
    Greetings
    | PeerConnected
    | PeerDisconnected
    | ReadyToCommit
    | Committed
    | HeadIsAborted
    | RolledBack
    | TxValid

export interface ClientConnected { tag: HydraEventType.ClientConnected }
export interface ClientDisconnected { tag: HydraEventType.ClientDisconnected }
export interface Update { tag: HydraEventType.Update, output: ServerOutput }
export interface Tick { tag: HydraEventType.Tick, tick: UTCTime }

export enum HydraEventType {
    ClientConnected,
    ClientDisconnected,
    Update,
    Tick
}

export type HydraEvent =
    ClientConnected
    | ClientDisconnected
    | Update
    | Tick
