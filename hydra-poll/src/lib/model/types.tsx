export interface Host { hostname: string, port: number }

export interface Party { vkey: string }

export interface NodeId { nodeId: string }

export interface UTCTime { time: number }

export interface UTxOValueType {
    lovelace: number
}

export interface UTxOType {
    address: string,
    datum?: string,
    datumhash?: string,
    inlineDatum?: string,
    referenceScript?: string,
    value: UTxOValueType
}

export type UTxO = object // Map String UTxOType

export enum DialogState {
    NoDialog,
    Dialog
}

export enum Pending {
    Pending,
    NotPending
}

export enum FeedbackState {
    Short,
    Full
}

export enum Severity {
    Success,
    Info,
    Error
}

export interface UserFeedback {
    severity: Severity
    message: string
    time: UTCTime
}

export enum HeadStateType {
    Idle = "Idle",
    Initializing = "Initializing",
    Open = "Open",
    Closed = "Closed",
    FanoutPossible = "FanoutPossible",
    Final = "Final"
}

export interface Idle { tag: HeadStateType.Idle }
export interface Initializing { tag: HeadStateType.Initializing, parties: Party[], remainingParties: Party[], utxo: UTxO }
export interface Open { tag: HeadStateType.Open, parties: Party[], utxo: UTxO }
export interface Closed { tag: HeadStateType.Closed, contestationDeadline: UTCTime }
export interface FanoutPossible { tag: HeadStateType.FanoutPossible }
export interface Final { tag: HeadStateType.Final, utxo: UTxO }

export type HeadState =
    Idle
    | Initializing
    | Open
    | Closed
    | FanoutPossible
    | Final

export interface Disconnected { nodeHost: Host, now: UTCTime }

export interface Connected {
    me?: Party
    nodeHost: Host
    peers: NodeId[]
    headState: HeadState
    dialogState: DialogState
    feedbackState: FeedbackState
    feedback: UserFeedback[]
    now: UTCTime
    pending: Pending
}

export type State = Disconnected | Connected