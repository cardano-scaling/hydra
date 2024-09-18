# Protocol

Additional implementation-specific documentation for the Hydra Head protocol and extensions like incremental decommits.

### Incremental Commits

#### Deposit flow

```mermaid
sequenceDiagram
    Alice->>+Node A: POST /commit UTxO
    Node A-->>-Alice: depositTx

    Alice ->> Alice: sign depositTx
    Alice ->> Chain: submit depositTx

    Chain ->>+ Node A: OnDepositTx utxo
    Chain ->>+ Node B: OnDepositTx utxo

    Node A -->> Alice: DepositDetected

    par Alice isLeader
        Node A->>Node A: ReqSn utxoToCommit
    and
        Node A->>Node B: ReqSn utxoToCommit
    end

    Node A -->> Alice: CommitRequested

    Node A->>Node A: sig = sign snapshot incl. utxoToCommit

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
    Alice->>+Node A: DELETE /commits/<tx-in>
    Node A->>Chain: recoverTx
    Chain ->>+ Node A: OnRecoverTx utxo
    Chain ->>+ Node B: OnRecoverTx utxo
    Node A -->>- Alice: CommitRecovered
    Node B -->>- Bob: CommitRecovered
    Node A-->>-Alice: OK

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
