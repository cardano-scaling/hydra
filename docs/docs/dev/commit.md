# Deposit funds into a Head

In order to transact inside of a Head on L2, users need to have some funds available to spend. Users can deposit public key outputs or script outputs into a Head to accommodate different use cases.

All deposits use the same `POST /commit` API endpoint, which returns a signed deposit transaction (`depositTx`) to be submitted to L1. See [protocol](./protocol.md) or [specification](./specification.md) for more details.

Depending on what is to be deposited, a user needs to decide between a _simple_ or _blueprint_ deposit request:

- **Simple** request to deposit a public key output into a Head, in which case they only need to show the JSON representation of the `UTxO` they want to deposit. The returned transaction then needs to be signed by the correct private key to prove ownership.

- **Blueprint** requests allow providing a _blueprint_ transaction together with the `UTxO` it spends to have more fine-grained control over the created deposit transaction. This is necessary when depositing script `UTxO` where more control over transaction attributes is needed (redeemers, reference inputs, validity bounds, etc.).

:::info
Blueprint transaction outputs are ignored — all value is locked into the Hydra deposit validator on L1.
:::

:::danger
TODO: Add section about _why_ the design is this way here, about how to make scripts spendable in a Hydra head deposit or link a document in the user manual, or consolidate this document further into [protocol](./protocol.md) and user manual. See https://github.com/cardano-scaling/hydra/issues/1655 
:::

For concrete examples on how to use the deposit API see the [simple](./../how-to/incremental-commit) and [blueprint](./../how-to/commit-blueprint) how-tos in the user manual.
