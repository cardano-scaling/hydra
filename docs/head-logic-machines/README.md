# Head state machines

Generated from crem's executable definitions with:

```sh
cabal run hydra-node:head-logic-lab -- machines docs/head-logic-machines
```

These are allowed replay transitions, including resets and events outside their usual phase. Edges to Idle mean observed completion (HeadFannedOut), including at parties that did not initiate fanout. Identity transitions are implicit. They do not show admission guards or prove that every edge is reachable from network input. See [the implementation guide](../head-logic-crem.md) for event meanings and limitations.

## composition

```mermaid
stateDiagram-v2
state Lifecycle {
Lifecycle_Idle
Lifecycle_Open
Lifecycle_Closed
Lifecycle_ReadyToFanout
Lifecycle_PendingFanout
Lifecycle_FanoutProgress
Lifecycle_Idle --> Lifecycle_Open: HeadOpened (Init observed)
Lifecycle_Open --> Lifecycle_Closed: HeadClosed
Lifecycle_Closed --> Lifecycle_Open: HeadOpened (replay reset)
Lifecycle_Closed --> Lifecycle_ReadyToFanout: HeadIsReadyToFanout
Lifecycle_Closed --> Lifecycle_PendingFanout: fanout starts or empty partial observation
Lifecycle_Closed --> Lifecycle_FanoutProgress: HeadPartialFannedOut
Lifecycle_Closed --> Lifecycle_Idle: HeadFannedOut (observed on chain)
Lifecycle_ReadyToFanout --> Lifecycle_Open: HeadOpened (replay reset)
Lifecycle_ReadyToFanout --> Lifecycle_PendingFanout: fanout starts or empty partial observation
Lifecycle_ReadyToFanout --> Lifecycle_FanoutProgress: HeadPartialFannedOut
Lifecycle_ReadyToFanout --> Lifecycle_Idle: HeadFannedOut (observed on chain)
Lifecycle_PendingFanout --> Lifecycle_Open: HeadOpened (replay reset)
Lifecycle_PendingFanout --> Lifecycle_ReadyToFanout: HeadFanoutReverted
Lifecycle_PendingFanout --> Lifecycle_FanoutProgress: HeadPartialFannedOut
Lifecycle_PendingFanout --> Lifecycle_Idle: HeadFannedOut (observed on chain)
Lifecycle_FanoutProgress --> Lifecycle_Open: HeadOpened (replay reset)
Lifecycle_FanoutProgress --> Lifecycle_ReadyToFanout: HeadFanoutReverted
Lifecycle_FanoutProgress --> Lifecycle_PendingFanout: HeadPartialFannedOut (empty total)
Lifecycle_FanoutProgress --> Lifecycle_Idle: HeadFannedOut (observed on chain)
}
state Incremental {
Incremental_OutsideHead
Incremental_NoIncrement
Incremental_DepositSelected
Incremental_DecommitPending
Incremental_ConflictingSelection
Incremental_OutsideHead --> Incremental_NoIncrement
Incremental_NoIncrement --> Incremental_OutsideHead
Incremental_NoIncrement --> Incremental_DepositSelected
Incremental_NoIncrement --> Incremental_DecommitPending
Incremental_NoIncrement --> Incremental_ConflictingSelection
Incremental_DepositSelected --> Incremental_OutsideHead
Incremental_DepositSelected --> Incremental_NoIncrement
Incremental_DepositSelected --> Incremental_DecommitPending
Incremental_DepositSelected --> Incremental_ConflictingSelection
Incremental_DecommitPending --> Incremental_OutsideHead
Incremental_DecommitPending --> Incremental_NoIncrement
Incremental_DecommitPending --> Incremental_DepositSelected
Incremental_DecommitPending --> Incremental_ConflictingSelection
Incremental_ConflictingSelection --> Incremental_OutsideHead
Incremental_ConflictingSelection --> Incremental_NoIncrement
Incremental_ConflictingSelection --> Incremental_DepositSelected
Incremental_ConflictingSelection --> Incremental_DecommitPending
}
Lifecycle --> Incremental
state OffChain {
OffChain_OutsideHead
OffChain_NoRequestSeen
OffChain_SnapshotIdle
OffChain_RequestSent
OffChain_CollectingSignatures
OffChain_OutsideHead --> OffChain_NoRequestSeen
OffChain_NoRequestSeen --> OffChain_OutsideHead
OffChain_NoRequestSeen --> OffChain_SnapshotIdle
OffChain_NoRequestSeen --> OffChain_RequestSent
OffChain_NoRequestSeen --> OffChain_CollectingSignatures
OffChain_SnapshotIdle --> OffChain_OutsideHead
OffChain_SnapshotIdle --> OffChain_NoRequestSeen
OffChain_SnapshotIdle --> OffChain_RequestSent
OffChain_SnapshotIdle --> OffChain_CollectingSignatures
OffChain_RequestSent --> OffChain_OutsideHead
OffChain_RequestSent --> OffChain_NoRequestSeen
OffChain_RequestSent --> OffChain_SnapshotIdle
OffChain_RequestSent --> OffChain_CollectingSignatures
OffChain_CollectingSignatures --> OffChain_OutsideHead
OffChain_CollectingSignatures --> OffChain_NoRequestSeen
OffChain_CollectingSignatures --> OffChain_SnapshotIdle
OffChain_CollectingSignatures --> OffChain_RequestSent
}
Incremental --> OffChain
state NodeMetadata {
NodeMetadata_()
}
OffChain --> NodeMetadata

```

## lifecycle

```mermaid
stateDiagram-v2

Idle
Open
Closed
ReadyToFanout
PendingFanout
FanoutProgress
Idle --> Open: HeadOpened (Init observed)
Open --> Closed: HeadClosed
Closed --> Open: HeadOpened (replay reset)
Closed --> ReadyToFanout: HeadIsReadyToFanout
Closed --> PendingFanout: fanout starts or empty partial observation
Closed --> FanoutProgress: HeadPartialFannedOut
Closed --> Idle: HeadFannedOut (observed on chain)
ReadyToFanout --> Open: HeadOpened (replay reset)
ReadyToFanout --> PendingFanout: fanout starts or empty partial observation
ReadyToFanout --> FanoutProgress: HeadPartialFannedOut
ReadyToFanout --> Idle: HeadFannedOut (observed on chain)
PendingFanout --> Open: HeadOpened (replay reset)
PendingFanout --> ReadyToFanout: HeadFanoutReverted
PendingFanout --> FanoutProgress: HeadPartialFannedOut
PendingFanout --> Idle: HeadFannedOut (observed on chain)
FanoutProgress --> Open: HeadOpened (replay reset)
FanoutProgress --> ReadyToFanout: HeadFanoutReverted
FanoutProgress --> PendingFanout: HeadPartialFannedOut (empty total)
FanoutProgress --> Idle: HeadFannedOut (observed on chain)

```

## deposits

```mermaid
stateDiagram-v2

MissingDeposit
InactiveDeposit
ActiveDeposit
ExpiredDeposit
ConsumedDeposit
MissingDeposit --> InactiveDeposit
MissingDeposit --> ActiveDeposit
MissingDeposit --> ExpiredDeposit
InactiveDeposit --> MissingDeposit
InactiveDeposit --> ActiveDeposit
InactiveDeposit --> ExpiredDeposit
InactiveDeposit --> ConsumedDeposit
ActiveDeposit --> MissingDeposit
ActiveDeposit --> InactiveDeposit
ActiveDeposit --> ExpiredDeposit
ActiveDeposit --> ConsumedDeposit
ExpiredDeposit --> MissingDeposit
ExpiredDeposit --> InactiveDeposit
ExpiredDeposit --> ActiveDeposit
ExpiredDeposit --> ConsumedDeposit
ConsumedDeposit --> MissingDeposit
ConsumedDeposit --> InactiveDeposit
ConsumedDeposit --> ActiveDeposit
ConsumedDeposit --> ExpiredDeposit
```

## incremental

```mermaid
stateDiagram-v2

OutsideHead
NoIncrement
DepositSelected
DecommitPending
ConflictingSelection
OutsideHead --> NoIncrement
NoIncrement --> OutsideHead
NoIncrement --> DepositSelected
NoIncrement --> DecommitPending
NoIncrement --> ConflictingSelection
DepositSelected --> OutsideHead
DepositSelected --> NoIncrement
DepositSelected --> DecommitPending
DepositSelected --> ConflictingSelection
DecommitPending --> OutsideHead
DecommitPending --> NoIncrement
DecommitPending --> DepositSelected
DecommitPending --> ConflictingSelection
ConflictingSelection --> OutsideHead
ConflictingSelection --> NoIncrement
ConflictingSelection --> DepositSelected
ConflictingSelection --> DecommitPending
```

## off-chain

```mermaid
stateDiagram-v2

OutsideHead
NoRequestSeen
SnapshotIdle
RequestSent
CollectingSignatures
OutsideHead --> NoRequestSeen
NoRequestSeen --> OutsideHead
NoRequestSeen --> SnapshotIdle
NoRequestSeen --> RequestSent
NoRequestSeen --> CollectingSignatures
SnapshotIdle --> OutsideHead
SnapshotIdle --> NoRequestSeen
SnapshotIdle --> RequestSent
SnapshotIdle --> CollectingSignatures
RequestSent --> OutsideHead
RequestSent --> NoRequestSeen
RequestSent --> SnapshotIdle
RequestSent --> CollectingSignatures
CollectingSignatures --> OutsideHead
CollectingSignatures --> NoRequestSeen
CollectingSignatures --> SnapshotIdle
CollectingSignatures --> RequestSent
```
