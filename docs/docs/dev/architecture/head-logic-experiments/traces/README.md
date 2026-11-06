# Head logic: concrete ReqTx executions

These diagrams come from interpreting the actual `onOpenNetworkReqTxProgram`
with the simple test ledger. Each diagram covers one concrete input; unvisited
branches are outside its scope. Every generated run checks its complete outcome
against the frozen migration baseline.

Read each sequence diagram from top to bottom. Lanes separate head logic,
ledger replies, staged changes, and buffered effects within one node.
The last note says whether changes and effects are committed or discarded.

Regenerate this page and its `.mmd` files with:

```sh
cabal bench hydra-node:head-logic-program --benchmark-options='--trace docs/docs/dev/architecture/head-logic-experiments/traces'
```

## leader success

```mermaid
%%{init: {"sequence": {"wrap": true}}}%%
sequenceDiagram
  participant logic as Head logic
  participant ledger as Ledger
  participant changes as Staged changes
  participant effects as Buffered effects
  rect rgb(245, 247, 250)
    Note over logic,effects: ReqTx: leader-success
    Note over logic: transaction is already applied locally: no
    logic->>changes: stage: remember transaction
    Note over logic: outputs do not collide with the local UTxO: yes
    ledger-->>logic: ledger application: valid
    Note over logic: outputs do not collide with pending deposits: yes
    logic->>changes: stage: apply transaction to local state
    Note over logic: no snapshot is in flight: yes
    Note over logic: this party leads the next snapshot: yes
    Note over logic: snapshot request has useful work: yes
    Note over logic: proposal matches the current snapshot context: yes
    logic->>changes: stage: record snapshot request
    logic->>effects: buffer: broadcast ReqSn
    Note over logic,effects: finish: commit changes and release effects
  end
```

## follower

```mermaid
%%{init: {"sequence": {"wrap": true}}}%%
sequenceDiagram
  participant logic as Head logic
  participant ledger as Ledger
  participant changes as Staged changes
  rect rgb(245, 247, 250)
    Note over logic,changes: ReqTx: follower
    Note over logic: transaction is already applied locally: no
    logic->>changes: stage: remember transaction
    Note over logic: outputs do not collide with the local UTxO: yes
    ledger-->>logic: ledger application: valid
    Note over logic: outputs do not collide with pending deposits: yes
    logic->>changes: stage: apply transaction to local state
    Note over logic: no snapshot is in flight: yes
    Note over logic: this party leads the next snapshot: no
    Note over logic,changes: finish: commit changes and release effects
  end
```

## wait

```mermaid
%%{init: {"sequence": {"wrap": true}}}%%
sequenceDiagram
  participant logic as Head logic
  participant ledger as Ledger
  participant changes as Staged changes
  rect rgb(245, 247, 250)
    Note over logic,changes: ReqTx: wait
    Note over logic: transaction is already applied locally: no
    logic->>changes: stage: remember transaction
    Note over logic: outputs do not collide with the local UTxO: yes
    ledger-->>logic: ledger application: invalid
    Note over logic: retry budget remains: yes
    Note over logic,changes: wait: commit changes and discard buffered effects
  end
```

## timeout

```mermaid
%%{init: {"sequence": {"wrap": true}}}%%
sequenceDiagram
  participant logic as Head logic
  participant ledger as Ledger
  participant changes as Staged changes
  rect rgb(245, 247, 250)
    Note over logic,changes: ReqTx: timeout
    Note over logic: transaction is already applied locally: no
    logic->>changes: stage: remember transaction
    Note over logic: outputs do not collide with the local UTxO: yes
    ledger-->>logic: ledger application: invalid
    Note over logic: retry budget remains: no
    logic->>changes: stage: record invalid transaction
    Note over logic,changes: finish: commit changes and release effects
  end
```

## output collision

```mermaid
%%{init: {"sequence": {"wrap": true}}}%%
sequenceDiagram
  participant logic as Head logic
  participant changes as Staged changes
  rect rgb(245, 247, 250)
    Note over logic,changes: ReqTx: output-collision
    Note over logic: transaction is already applied locally: no
    logic->>changes: stage: remember transaction
    Note over logic: outputs do not collide with the local UTxO: no
    Note over logic: retry budget remains: yes
    Note over logic,changes: wait: commit changes and discard buffered effects
  end
```

## snapshot in flight

```mermaid
%%{init: {"sequence": {"wrap": true}}}%%
sequenceDiagram
  participant logic as Head logic
  participant ledger as Ledger
  participant changes as Staged changes
  rect rgb(245, 247, 250)
    Note over logic,changes: ReqTx: snapshot-in-flight
    Note over logic: transaction is already applied locally: no
    logic->>changes: stage: remember transaction
    Note over logic: outputs do not collide with the local UTxO: yes
    ledger-->>logic: ledger application: valid
    Note over logic: outputs do not collide with pending deposits: yes
    logic->>changes: stage: apply transaction to local state
    Note over logic: no snapshot is in flight: no
    Note over logic,changes: finish: commit changes and release effects
  end
```
