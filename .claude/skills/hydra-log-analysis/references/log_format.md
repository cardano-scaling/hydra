# hydra-node Log Format Reference

Hydra-node emits **JSON Lines** (one JSON object per line). Every line is independently parseable.

## Top-level object shape

```jsonc
{
  "timestamp": "2026-03-12T14:23:45.123Z",     // ISO 8601 UTC
  "threadId": "ThreadId 42",
  "namespace": "...",
  "message": {
    "node": {                                   // present on every protocol log line
      "input":  { ... },                        // what triggered processing (optional)
      "outcome": {                              // what came out of HeadLogic.update
        "stateChanges": [ ... ],                // array of StateChanged events
        "effects":      [ ... ],                // array of Effects (network/chain/client)
        "error":        { ... }                 // present only on Left outcomes
      }
    }
  }
}
```

Other top-level message shapes exist (chain follower, network layer, etc.) but the **HeadLogic** lines under `message.node` are the load-bearing ones for protocol diagnosis.

## Key JSON paths

| Purpose | Path |
|---|---|
| Wall-clock timestamp | `.timestamp` |
| Triggering input tag | `.message.node.input.tag` |
| State change list | `.message.node.outcome.stateChanges[]` |
| State change tag | `.message.node.outcome.stateChanges[].tag` |
| Effect list | `.message.node.outcome.effects[]` |
| Network message tag inside effect | `.message.node.outcome.effects[].message.tag` |
| Outcome error | `.message.node.outcome.error` |
| Snapshot number (state) | `.snapshot.number` (newer) or `.snapshotNumber` (older) |
| Snapshot number (effect) | `.message.snapshotNumber` |
| Party identity | `.party.vkey` (hex string) |

Recursive descent (`.. | objects | select(.tag? == "X")`) is the safest way to find a tag if its exact path varies between hydra-node versions or codepaths.

## State-change tags (significant subset)

Found at `.message.node.outcome.stateChanges[].tag`.

### Snapshot lifecycle
- `SnapshotRequested` — leader emitted ReqSn; carries `snapshot.number`, optional `decommitTx`, optional `depositTxId`
- `SnapshotConfirmed` — quorum of AckSns gathered; carries `snapshot` and `signatures` map keyed by party
- `SnapshotRequestAborted` — leader's own ReqSn echo failed validation; cleanly resets to NoSeenSnapshot (added recently — see project memory `project_deposit_bug.md`)

### Transactions
- `TransactionAppliedToLocalUTxO` — tx applied locally; carries `newLocalUTxO`
- `TransactionReceived` — tx received over network from a peer

### Commits / decommits / deposits
- `PartyCommitted` — collectCom UTxO contribution observed on chain
- `CommitFinalized` — chain confirmed an incremental commit; clears pending deposit
- `DecommitFinalized` — chain confirmed an incremental decommit; clears `decommitTx`
- `DepositRecorded` — chain saw a deposit tx; records `depositTxId` and `chainTime`
- `DepositActivated` — deposit deadline reached and tx is in scope; sets `currentDepositTxId`
- `DepositExpired` — deposit deadline passed without inclusion

### Head lifecycle
- `HeadInitialized` / `HeadOpened` / `HeadClosed` / `HeadIsContested` / `HeadIsFinalized`

### Chain
- `TickObserved` — chain follower advanced to a new slot/time

## Effect tags (network)

Found at `.message.node.outcome.effects[].message.tag` (network effects wrap a `Message` ADT).

- `ReqTx` — broadcast a new transaction to peers
- `ReqSn` — leader requesting a snapshot; carries `snapshotNumber`, `transactionIds`, optional `decommitTx`, `depositTxId`
- `AckSn` — non-leader acknowledging a snapshot; carries `snapshotNumber`, `signed` signature, `party`
- `ConfTx` — confirm transaction (rare)
- `ReqDec` — request a decommit (legacy)

There is also a `ToParty` effect-shape variant when sending point-to-point — look for `.toParty.vkey`.

## Input tags

Found at `.message.node.input.tag`.

- `NetworkInput` — wraps a `Message` from a peer; inner tag at `.message.tag`
- `ChainInput` — observed chain event (commit, deposit, tick, close)
- `ClientInput` — request from the API client (`NewTx`, `Decommit`, `Recover`, `Close`...)

## Error / failure shapes

When HeadLogic rejects an input, `outcome` contains:

```jsonc
{
  "tag": "Error",                  // sometimes
  "error": {
    "tag": "RequireFailed",        // typical wrapper
    "reason": {
      "tag": "ReqSvNumberInvalid", // or similar — see HeadLogic/Error.hs
      ...
    }
  }
}
```

Grep heuristics:
- `"RequireFailed"` — HeadLogic precondition violation
- `"Plutus validation failed"` — on-chain script rejected a tx (Aiken `expect` failure produces a `DebugFailure` array)
- `"Invalid"` — generic validation error

## Time fields

- `.timestamp` — wall clock, ISO 8601, **always UTC**
- `.chainTime` (inside chain events / DepositRecorded) — POSIX time **in milliseconds**
- Tx validity bounds — POSIX **milliseconds** since epoch

## Multi-node correlation

Each node's log is independent. To correlate:
- Match on `snapshotNumber` for snapshot-related events
- Match on `txId` (inside `transaction.txId` or as bytes in `transactionIds[]`) for tx events
- Match on `depositTxId` for deposit lifecycle
- Match on `chainTime` to align on the chain timeline (timestamps will differ slightly per node)
