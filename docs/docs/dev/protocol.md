# Protocol

Additional implementation-specific documentation for the Hydra Head protocol and extensions like incremental decommits.

### Incremental Commits

#### Deposit flow

```mermaid
sequenceDiagram
    Alice->>+Node A: POST /commit (UTxO, UTCTime)
    Node A-->>-Alice: depositTx

    Alice ->> Alice: sign depositTx
    Alice ->> Alice: submit depositTx

    Chain ->>+ Node A: OnDepositTx utxo
    Chain ->>+ Node B: OnDepositTx utxo

    Node A -->> Alice: DepositDetected

    Node A -->> Alice: CommitRequested

    par Alice isLeader
        Node A->>Node A: ReqSn utxo
    and
        Node A->>Node B: ReqSn utxo
    end

    Node A->>Node A: sig = sign snapshot incl. inputs(commitTx)

    par broadcast
        Node A->>Node A: AckSn sig
    and
        Node A->>Node B: AckSn sig
    end
    Node B->>Node A: AckSn sig
    Node A -->> Alice: SnapshotConfirmed

    Node A -->> Alice: CommitApproved

    Node A ->> Chain: IncrementTx snapshot sig
    Chain ->> Node A: OnIncrementTx
    Node A -->> Alice: CommitFinalized
```

#### Recover flow

```mermaid
sequenceDiagram
    Alice->>+Node A: DELETE /commit/tx-in (UTxO, UTCTime, SlotNo)
    Node A-->>-Alice: recoverTx

    Alice ->> Alice: sign recoverTx
    Alice ->> Alice: submit recoverTx
```

### Incremental Decommits

```mermaid
sequenceDiagram
    Alice->>+Node A: POST /decommit (decTx)
    Node A-->>-Alice: OK

    Node A->>Node A: canApply decTx

    par broadcast
        Node A->>Node A: ReqDec decTx
    and
        Node A->>Node B: ReqDec decTx
    end

    Node A -->> Alice: DecommitRequested

    par Alice isLeader
        Node A->>Node A: ReqSn decTx
    and
        Node A->>Node B: ReqSn decTx
    end

    Node A->>Node A: canApply decTx, decUTxO =  outputs(decTx)
    Node A->>Node A: sig = sign snapshot incl. decUTxO

    par broadcast
        Node A->>Node A: AckSn sig
    and
        Node A->>Node B: AckSn sig
    end

    Node B->>Node A: AckSn sig

    Node A -->> Alice: SnapshotConfirmed
    Node A -->> Alice: DecommitApproved

    Node A ->> Chain: DecrementTx snapshot sig
    Chain ->> Node A: OnDecrementTx
    Node A -->> Alice: DecommitFinalized
```
