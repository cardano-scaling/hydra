# Deposit funds into a head

Deposits move public-key or script outputs from Cardano L1 into an open Hydra
head. `POST /commit` drafts a deposit transaction and adds the node's wallet
witness. The client adds any required owner signatures and submits the
transaction to L1 without changing its body or output order.

## Request forms

| Request | Outputs deposited on L2 | Change on L1 |
| --- | --- | --- |
| UTxO without a blueprint | The supplied UTxO | None from the supplied inputs |
| Blueprint without outputs | The supplied UTxO | None from the supplied inputs |
| Blueprint with outputs | The blueprint outputs | Supply a balanced blueprint |
| Blueprint with outputs and `changeAddress` | The blueprint outputs | Remaining input value goes to `changeAddress` |

A blueprint supplies transaction context such as script witnesses, redeemers,
reference inputs, and required signers. Include the UTxO context needed to
resolve its inputs. When that context includes reference inputs, use explicit
blueprint outputs: a blueprint without outputs deposits the entire supplied
UTxO map. The node replaces the blueprint's outputs on L1 with the
deposit output and any change; the deposit datum records the outputs destined
for L2. Output references for explicit blueprint outputs use the blueprint
transaction ID.

Depositing funds from a script does not automatically preserve that script's
control. To preserve it, the recorded L2 outputs must retain the required
script address and datum. See [dApp validator guidance](../how-to/best-practise-dapp.md).

## Completion

Submission and `CommitRecorded` do not make funds spendable on L2. The deposit
must mature, be approved in a signed snapshot, and settle through an increment
transaction. Wait for `CommitFinalized`. See the
[protocol flow](./protocol.md#deposit-flow),
[deposit how-to](../how-to/incremental-commit.md), and
[blueprint example](../how-to/commit-blueprint.md).
