# Head state machines

Generated from crem's executable definitions with:

```sh
cabal run hydra-node:head-logic-lab -- machines docs/head-logic-machines
```

Coordinated contains the snapshot and transaction owners used by live staging and replay. Its Reserved phase records a pending decommit; it does not prove that rebuilding preserved the reservation. Composition shows DepositRegistry → Lifecycle → Clock. The open lifecycle state holds the Coordinated continuation; its nested composition is shown separately. These are allowed replay transitions, including resets and events outside their usual phase. Edges to Idle mean observed completion (HeadFannedOut), including at parties that did not initiate fanout. Identity transitions are implicit. They do not show admission guards or prove that every edge is reachable from network input. See [the implementation guide](../head-logic-crem.md) for event meanings and limitations.

## coordinated

```mermaid
stateDiagram-v2
state Snapshots {
Snapshots_AwaitingRequest
Snapshots_RequestSent
Snapshots_CollectingSignatures
Snapshots_AwaitingRequest --> Snapshots_RequestSent
Snapshots_AwaitingRequest --> Snapshots_CollectingSignatures
Snapshots_RequestSent --> Snapshots_AwaitingRequest
Snapshots_RequestSent --> Snapshots_CollectingSignatures
Snapshots_CollectingSignatures --> Snapshots_AwaitingRequest
Snapshots_CollectingSignatures --> Snapshots_RequestSent
}
state Transactions {
Transactions_Unreserved
Transactions_Reserved
Transactions_Unreserved --> Transactions_Reserved
Transactions_Reserved --> Transactions_Unreserved
}
Snapshots --> Transactions
```

## composition

```mermaid
stateDiagram-v2
state DepositRegistry {
DepositRegistry_TrackedDeposits
}
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
DepositRegistry --> Lifecycle
state Clock {
Clock_ObservedTime
}
Lifecycle --> Clock

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
