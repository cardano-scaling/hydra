# Head logic: concrete ReqTx executions

These diagrams come from interpreting the actual `onOpenNetworkReqTxProgram`
with the simple test ledger. Each diagram covers one concrete input; unvisited
branches are outside its scope. Every generated run checks its complete outcome
against the frozen migration baseline.

Regenerate this page and its `.mmd` files with:

```sh
cabal bench hydra-node:head-logic-program --benchmark-options='--trace docs/docs/dev/architecture/head-logic-traces'
```

## leader success

```mermaid
flowchart TD
  n0["transaction is already applied locally: no"]
  n1["stage: remember transaction"]
  n2["outputs do not collide with the local UTxO: yes"]
  n3["ledger application: valid"]
  n4["outputs do not collide with pending deposits: yes"]
  n5["stage: apply transaction to local state"]
  n6["snapshot request has useful work: yes"]
  n7["no snapshot is in flight: yes"]
  n8["this party leads the next snapshot: yes"]
  n9["stage: record snapshot request"]
  n10["buffer: broadcast ReqSn"]
  n11["finish: commit changes and release effects"]
  n0 --> n1
  n1 --> n2
  n2 --> n3
  n3 --> n4
  n4 --> n5
  n5 --> n6
  n6 --> n7
  n7 --> n8
  n8 --> n9
  n9 --> n10
  n10 --> n11
```

## follower

```mermaid
flowchart TD
  n0["transaction is already applied locally: no"]
  n1["stage: remember transaction"]
  n2["outputs do not collide with the local UTxO: yes"]
  n3["ledger application: valid"]
  n4["outputs do not collide with pending deposits: yes"]
  n5["stage: apply transaction to local state"]
  n6["snapshot request has useful work: yes"]
  n7["no snapshot is in flight: yes"]
  n8["this party leads the next snapshot: no"]
  n9["finish: commit changes and release effects"]
  n0 --> n1
  n1 --> n2
  n2 --> n3
  n3 --> n4
  n4 --> n5
  n5 --> n6
  n6 --> n7
  n7 --> n8
  n8 --> n9
```

## wait

```mermaid
flowchart TD
  n0["transaction is already applied locally: no"]
  n1["stage: remember transaction"]
  n2["outputs do not collide with the local UTxO: yes"]
  n3["ledger application: invalid"]
  n4["retry budget remains: yes"]
  n5["wait: commit changes and discard buffered effects"]
  n0 --> n1
  n1 --> n2
  n2 --> n3
  n3 --> n4
  n4 --> n5
```

## timeout

```mermaid
flowchart TD
  n0["transaction is already applied locally: no"]
  n1["stage: remember transaction"]
  n2["outputs do not collide with the local UTxO: yes"]
  n3["ledger application: invalid"]
  n4["retry budget remains: no"]
  n5["stage: record invalid transaction"]
  n6["finish: commit changes and release effects"]
  n0 --> n1
  n1 --> n2
  n2 --> n3
  n3 --> n4
  n4 --> n5
  n5 --> n6
```

## output collision

```mermaid
flowchart TD
  n0["transaction is already applied locally: no"]
  n1["stage: remember transaction"]
  n2["outputs do not collide with the local UTxO: no"]
  n3["retry budget remains: yes"]
  n4["wait: commit changes and discard buffered effects"]
  n0 --> n1
  n1 --> n2
  n2 --> n3
  n3 --> n4
```

## snapshot in flight

```mermaid
flowchart TD
  n0["transaction is already applied locally: no"]
  n1["stage: remember transaction"]
  n2["outputs do not collide with the local UTxO: yes"]
  n3["ledger application: valid"]
  n4["outputs do not collide with pending deposits: yes"]
  n5["stage: apply transaction to local state"]
  n6["snapshot request has useful work: yes"]
  n7["no snapshot is in flight: no"]
  n8["finish: commit changes and release effects"]
  n0 --> n1
  n1 --> n2
  n2 --> n3
  n3 --> n4
  n4 --> n5
  n5 --> n6
  n6 --> n7
  n7 --> n8
```
