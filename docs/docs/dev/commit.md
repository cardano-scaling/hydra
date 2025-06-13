# Commit funds to a Head

In order to transact inside of a Head on L2, users need to have some funds available to spend. In addition to public key outputs users can also directly commit script outputs to a Head in order to accommodate different use cases.

Depending on when a commit should happens we can distinguish two types of commits:

- Before the head is open: **Commit**
- When the head is already open: **Incremental commit**

Both types of commits share the same API of requesting a transaction via `POST /commit` where the only difference is the returned transaction being either a `commitTx` or `depositTx` in the L1 protocol. See [protocol](./protocol.md) or [specification](./specification.md) for more details.

Depending on what is to be committed, a user needs to decide between a _simple_ or _blueprint_ commit request:

- **Simple** request to commit a public key output to a Head in which case they only need to show the JSON representation of the `UTxO` they want to spend. The returned transaction then needs to be signed by the correct private key to prove ownership.

- **Blueprint** requests allow to provide a _blueprint_ transaction together with the `UTxO` it spends to be able to have more fine grained control over the created commit/deposit transaction. This is crucial when committing script `UTxO` where more control over some of the transaction attributes is needed (redeemers, reference inputs, validity bounds, etc.).

:::info
Note that blueprint transaction outputs are ignored as all the value will be locked into the Hydra L1 scripts (commit or deposit validators).
:::

:::danger
TODO: Add section about _why_ the design is this way here, about how to make scripts spendable in a a Hydra head commit/deposit or link a document in the user manual, or consolidate this document further into [protocol](./protocol.md) and user manual. See https://github.com/cardano-scaling/hydra/issues/1655 
:::

For concrete examples on how to use the commit API see the [simple](./../how-to/incremental-commit) and [blueprint](./../how-to/commit-blueprint) commit how-tos in the user manual.
