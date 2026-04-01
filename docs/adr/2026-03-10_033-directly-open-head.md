---
slug: 33
title: |
  33. Directly open head: removal of initialization phase
authors: [ch1bo]
tags: [Accepted]
---

## Status

Accepted

## Context

- The Hydra Head protocol, as described in the [original paper](https://eprint.iacr.org/2020/299.pdf), specifies an initialization phase before a head can be opened. This phase consists of multiple on-chain transactions:

  1. **Init** - Creates the head on-chain in an `Initial` state, minting head tokens (one state thread token + one participation token per party).
  2. **Commit** - Each participant locks UTxO they want to bring into the head (one transaction per party).
  3. **CollectCom** - Once all parties have committed, collects all committed UTxO and transitions the head to `Open`.
  4. **Abort** - An alternative path allowing any party to cancel initialization and reimburse all committed UTxO.

- This initialization phase introduced significant complexity:
  - Two additional on-chain validators (`vInitial`, `vCommit`) beyond the head validator itself.
  - An `Initial` off-chain state tracking pending commits and committed UTxO per party.
  - Multiple API events (`HeadIsInitializing`, `Committed`, `HeadIsAborted`) and a client command (`Abort`).

- The **"unabortable heads" problem**: If a participant committed a large UTxO set, the resulting Abort transaction could exceed Cardano transaction size limits, making it impossible to abort the head. This effectively locked funds with no recourse.

- The Commit mechanism was limited in how much UTxO could be committed per party due to on-chain transaction size constraints, while the later-added deposit/increment mechanism does not have these per-party limitations.

- The overall lifecycle cost of opening a head was high: Init + N Commit transactions + CollectCom, each requiring on-chain fees.

## Decision

Remove the initialization phase entirely. The Init transaction directly opens the head:

- **Init creates an `Open` head**: The Init transaction mints the head tokens and creates the head output in `Open` state with an empty UTxO set (`utxoHash = hash(∅)`).
- **Funds are added post-opening**: Participants use the existing deposit/increment mechanism to add funds to the head after it is opened.
- **No Commit, CollectCom, or Abort transactions**: These transaction types and their associated on-chain validators are removed.

The head lifecycle simplifies from:

```
Idle → Init → Initial → Commit* → CollectCom → Open → ... → Final
                 ↓
               Abort → Idle
```

to:

```
Idle → Init → Open → ... → Final
```

## Consequences

- The `vInitial` and `vCommit` on-chain validators are removed, reducing the on-chain script surface and audit scope.

- The head validator state machine simplifies from `Initial → Open → Closed → Final` to `Open → Closed → Final`.

- Off-chain complexity is reduced: the `InitialState` and its associated event handlers (commit tracking, abort logic) are removed from `HeadLogic`.

- The API surface shrinks: `HeadIsInitializing`, `Committed`, `HeadIsAborted` server outputs and the `Abort` client input are removed. `HeadIsOpen` no longer carries a `utxo` field since heads always open empty.

- Opening a head requires fewer on-chain transactions (single Init vs. Init + N Commits + CollectCom), reducing costs for most use cases.

- The unabortable heads problem is eliminated since there is no abort transaction.

- Adding funds to a head is unified under a single mechanism (deposit/increment) regardless of whether it happens at the start or later during the head's lifetime.

- The overloaded "commit" term is dropped in favor of "deposit". Previously, the `/commit` HTTP endpoint served double duty (committing during initialization and depositing into an open head). With initialization removed, this endpoint can be replaced by a more REST-like `/deposits` resource, and server outputs renamed from `CommitXXX` to `DepositXXX`, resulting in a cleaner and more consistent API.

- A future enhancement could allow the initiator to include initial funds directly in the Init transaction, avoiding the need for a separate deposit.
