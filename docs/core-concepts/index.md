---
sidebar_position: 1
---

# Learn

```mdx-code-block
import DocCardList from '@theme/DocCardList';
import {useDocsSidebar} from '@docusaurus/theme-common/internal';

<DocCardList items={useDocsSidebar().items.filter(({ docId }) => docId != "index")}/>
```

### Incremental Decommits

```mermaid
sequenceDiagram
    Alice->>HeadLogic: Decommit (decTx)
    HeadLogic->>HeadLogic: canApply decTx

    par broadcast
        HeadLogic->>HeadLogic: ReqDec decTx
    and
        HeadLogic->>Node B: ReqDec decTx
    end

    HeadLogic -->> Alice: DecommitRequested

    par Alice isLeader
        HeadLogic->>HeadLogic: ReqSn decTx
    and
        HeadLogic->>Node B: ReqSn decTx
    end

    HeadLogic->>HeadLogic: canApply decTx, decUTxO =  outputs(decTx)
    HeadLogic->>HeadLogic: sig = sign snapshot incl. decUTxO

    par broadcast
        HeadLogic->>HeadLogic: AckSn sig
    and
        HeadLogic->>Node B: AckSn sig
    end

    Node B->>HeadLogic: AckSn sig

    HeadLogic -->> Alice: SnapshotConfirmed
    HeadLogic -->> Alice: DecommitApproved

    HeadLogic ->> Chain: DecrementTx snapshot sig
    Chain ->> HeadLogic: OnDecrementTx
    HeadLogic -->> Alice: DecommitFinalized
```

### Incremental Commits

Scenario: Alice wants to commit some `UTxO` owned by her private key to the
Open Head. Similarly to "external" commit this means that the node can only
prepare balance and sign the transaction but Alice still needs to add her
signature and submit the transaction to the chain.

```mermaid
sequenceDiagram
    Alice->>+API: POST /commit (UTxO)
    API->>HeadLogic: Commit UTxO

    par broadcast
        HeadLogic->>HeadLogic: ReqInc incUTxO
    and
        HeadLogic->>Node B: ReqInc incUTxO
    end

    HeadLogic -->> Alice: WS CommitRequested

    par Alice isLeader
        HeadLogic->>HeadLogic: ReqSn incUTxO
    and
        HeadLogic->>Node B: ReqSn incUTxO
    end
    
    Note over HeadLogic,Chain: PROBLEM: Need to verify incUTxO on L1 as we authorize the TxIns to use (because of on-chain scripts).

    HeadLogic->>HeadLogic: sig = sign snapshot incl. inputs(incUTxO)

    par broadcast
        HeadLogic->>HeadLogic: AckSn sig
    and
        HeadLogic->>Node B: AckSn sig
    end

    Node B->>HeadLogic: AckSn sig

    HeadLogic -->> Alice: WS SnapshotConfirmed
    HeadLogic -->> Alice: WS CommitApproved

    HeadLogic -->> API: SnapshotConfirmed
    API->>API: draftIncrementTx vk snapshot sig >>= finalizeTx >>= signTx sk

    API-->>-Alice: IncrementTx
    Alice->>Alice: sign IncrementTx
    Alice->>Chain: submit IncrementTx
    
    Chain->>HeadLogic: OnIncrementTx
    HeadLogic-->>Alice: CommitFinalized
```
