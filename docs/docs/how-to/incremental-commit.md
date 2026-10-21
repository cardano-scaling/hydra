---
sidebar_position: 5
---

# Deposit funds to an open Head

Deposits move funds from Cardano L1 into an already open head. The
`POST /commit` endpoint drafts the transaction; the client adds the required
signatures and submits it to L1.

## Deposit request forms

| Request | Deposited outputs | L1 change |
| --- | --- | --- |
| UTxO only | The supplied UTxO | None from these inputs |
| UTxO and a blueprint without outputs | The supplied UTxO, with additional transaction context | None from these inputs |
| UTxO and a blueprint with outputs | The blueprint outputs | Provide a balanced blueprint |
| UTxO, a blueprint with outputs, and `changeAddress` | The blueprint outputs | Remaining input value returned to the address |

:::info
Do not include stake-pool or DRep registration or delegation certificates in
a blueprint. Staking and governance are outside the head ledger.
:::

:::warning Deposit restrictions
The `/commit` endpoint enforces the following restrictions:
- **Byron addresses are rejected.** UTxOs held at Byron-era addresses cannot be deposited.
- **Minimum ADA.** The deposited outputs are locked in a single deposit output whose inline datum embeds a copy of each of them, so that output needs more ADA than the deposited outputs would on their own. A UTxO holding a native token at its own minimum ADA is a typical case that falls short. The deposited value must cover the minimum ADA of this deposit output; otherwise the request fails with a `DepositTooLow` error (HTTP 400) reporting `providedValue`, the lovelace you deposit, and `minimumValue`, the lovelace the deposit output requires. To retry, deposit the same assets with at least the difference more lovelace: either create a new UTxO holding the tokens plus the extra ADA, or use a blueprint transaction whose deposited output carries the extra ADA taken from your other inputs. Adding further UTxOs to the same deposit also works, but each one enlarges the datum and so raises the required minimum a little beyond the reported difference.
- **Balance the requested value.** Explicit blueprint outputs determine what is deposited. With `changeAddress`, the node returns the remaining input value on L1; negative change components are clamped to zero. This does not reduce oversized blueprint outputs. Supply enough input value for the outputs you request and inspect the drafted transaction before signing.
- **Deposits too large to claim are rejected.** A deposit is only accepted if the increment transaction claiming it would stay within layer 1 ledger limits: the merged head output value must fit within the maximum serialized value size (5000 bytes on mainnet) and the increment transaction within the maximum transaction size. Deposits carrying tokens under many distinct policy ids hit the value size limit first; such a request fails with a `DepositTooLarge` error (HTTP 400). Without this check the deposit transaction itself would succeed, but the funds could never be claimed into the head and would remain locked until recovered after the deadline.
:::

For a reproducible partial deposit, follow the [blueprint tutorial](./commit-blueprint.md).
It deposits 5 ADA from an input of at least 8 ADA, builds the request from the
actual transaction and UTxO files, and returns change on L1. For native assets,
preserve exact integer quantities throughout request construction; do not
round values through JavaScript `Number` or other floating-point conversions.


## Settlement

Observation, activation, approval, and settlement are separate steps. See
[deposit timing](../configuration.md#deposit-activation).

A deposit remains pending while any of its output references (transaction id and output index) collide with an output the head holds or a transaction in the proposed snapshot creates. It can be selected after the conflicting output has been spent in a confirmed snapshot; if it remains unclaimed, it can be recovered after its deadline. Layer 2 transactions, including decommits, are also rejected if they create an output reference already held by the head.

:::info Deposited funds are spendable only after the increment settles
A deposit is not part of the head's layer 2 ledger the moment its snapshot is
confirmed. It is carried as a *pending commit* until the `IncrementTx` claiming
it is observed on-chain, and only then does it enter the spendable UTxO set —
signalled by `CommitFinalized`. Submitting a transaction that spends the
deposited outputs before that point leaves it pending until it either becomes
applicable or times out as invalid; a snapshot request listing such a
transaction is rejected outright and never signed. Wait for `CommitFinalized`
before building transactions that spend the deposited outputs.
:::

:::warning Reference scripts
Do not include reference scripts in the outputs to deposit. The serialized
Plutus output can record a script hash, but the node cannot reconstruct the
script from that hash. It rejects deposits whose outputs do not round-trip
through its UTxO representation. If an input carries a reference script, use
explicit blueprint outputs without it, and publish the script separately on
L2 if needed. This restriction does not prevent using reference inputs to
witness the L1 deposit transaction.
:::

## Recover a deposit

Query the pending deposit transaction IDs:

```shell
curl --fail-with-body -sS http://localhost:4001/commits
```

Choose the deposit to recover and wait until its deadline has passed. Replace
`DEPOSIT_TX_ID` below with the actual ID:

```shell
curl --fail-with-body -X DELETE http://localhost:4001/commits/DEPOSIT_TX_ID
```

Wait for `CommitRecovered`, then query the recorded output addresses on L1.
Recovery reproduces those outputs, which may differ from the original inputs
when a blueprint changes them. The transaction must recover one deposit at a
time, after its deadline.

Unclaimed deposits remain recoverable when the head closes, after fanout,
and while a new head is running. The node only claims deposits targeting the
current head's ID.

There is one important restriction: while the head is open, the node rejects
recovery of a deposit whose finalized increment rolled back
(`RecoverBlockedByFinalizedCommit`). Its funds are already accounted for in
signed L2 state, so the node re-posts that increment instead. After closure,
recovery and selective fanout provide an escape path. See
[rollback handling](../dev/rollbacks/index.md).
