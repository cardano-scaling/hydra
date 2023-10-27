import { NodeId, Party, UTCTime, UTxO } from "./types"

export type HeadId = string

export type SnapshotNumber = number

export enum HeadStatus {
    Idle,
    Initializing,
    Open,
    Closed,
    FanoutPossible,
    Final
}

export interface Greetings {
    me: Party
    headStatus: HeadStatus
    snapshotUtxo?: UTxO
    hydraNodeVersion: string
    tag: ServerOutputTag.Greetings
    seq: number
    timestamp: Date
}

export interface PeerConnected {
    peer: NodeId
    tag: ServerOutputTag.PeerConnected
    seq: number
    timestamp: Date
}

export interface PeerDisconnected {
    peer: NodeId
    tag: ServerOutputTag.PeerDisconnected
    seq: number
    timestamp: Date
}

export interface ReadyToCommit {
    parties: Party[]
    tag: ServerOutputTag.ReadyToCommit
    seq: number
    timestamp: Date
}

export interface Committed {
    headId: HeadId
    party: Party
    utxo: UTxO
    tag: ServerOutputTag.Committed
    seq: number
    timestamp: Date
}

export interface HeadIsOpen {
    headId: HeadId
    utxo: UTxO
    tag: ServerOutputTag.HeadIsOpen
    seq: number
    timestamp: Date
}

export interface HeadIsAborted {
    headId: HeadId
    utxo: UTxO
    tag: ServerOutputTag.HeadIsAborted
    seq: number
    timestamp: Date
}

export interface HeadIsClosed {
    headId: HeadId
    utxo: UTxO
    snapshotNumber: SnapshotNumber
    // contestationDeadline :: UTCTime
    tag: ServerOutputTag.HeadIsClosed
    seq: number
    timestamp: Date
}

export interface HeadIsContested {
    headId: HeadId
    utxo: UTxO
    snapshotNumber: SnapshotNumber
    tag: ServerOutputTag.HeadIsContested
    seq: number
    timestamp: Date
}

export interface HeadIsFinalized {
    headId: HeadId
    utxo: UTxO
    tag: ServerOutputTag.HeadIsFinalized
    seq: number
    timestamp: Date
}

export interface ReadyToFanout {
    headId: HeadId
    tag: ServerOutputTag.ReadyToFanout
    seq: number
    timestamp: Date
}

export interface RolledBack {
    tag: ServerOutputTag.RolledBack
    seq: number
    timestamp: Date
}

export interface TxValid {
    headId: HeadId
    transaction: any
    tag: ServerOutputTag.TxValid
    seq: number
    timestamp: Date
}

export interface TxInvalid {
    headId: HeadId
    utxo: UTxO
    transaction: any
    // validationError :: ValidationError
    tag: ServerOutputTag.TxInvalid
    seq: number
    timestamp: Date
}

export interface GetUTxOResponse {
    headId: HeadId
    transaction: any
    tag: ServerOutputTag.GetUTxOResponse
    seq: number
    timestamp: Date
}

export interface InvalidInput {
    reason: string
    input: string
    tag: ServerOutputTag.InvalidInput
    seq: number
    timestamp: Date
}

export type ServerOutput =
    Greetings
    | PeerConnected
    | PeerDisconnected
    | ReadyToCommit
    | Committed
    | HeadIsOpen
    | HeadIsAborted
    | HeadIsClosed
    | HeadIsContested
    | HeadIsFinalized
    | ReadyToFanout
    | RolledBack
    | TxValid
    | TxInvalid
    // | SnapshotConfirmed
    | GetUTxOResponse
    | InvalidInput
// | PostTxOnChainFailed

export enum ServerOutputTag {
    Greetings = "Greetings",
    PeerConnected = "PeerConnected",
    PeerDisconnected = "PeerDisconnected",
    ReadyToCommit = "ReadyToCommit",
    Committed = "Committed",
    HeadIsOpen = "HeadIsOpen",
    HeadIsAborted = "HeadIsAborted",
    HeadIsClosed = "HeadIsClosed",
    HeadIsContested = "HeadIsContested",
    HeadIsFinalized = "HeadIsFinalized",
    ReadyToFanout = "ReadyToFanout",
    RolledBack = "RolledBack",
    TxValid = "TxValid",
    TxInvalid = "TxInvalid",
    // SnapshotConfirmed
    GetUTxOResponse = "GetUTxOResponse",
    InvalidInput = "InvalidInput"
    // PostTxOnChainFailed
}

export interface ClientConnected { tag: HydraEventType.ClientConnected }

export interface ClientDisconnected { tag: HydraEventType.ClientDisconnected }

export interface Update { tag: HydraEventType.Update, output: ServerOutput }

export enum HydraEventType {
    ClientConnected,
    ClientDisconnected,
    Update,
}

export type HydraEvent =
    ClientConnected
    | ClientDisconnected
    | Update
