# Commit funds to a Head

In order to transact inside of a Head on L2, users need to have some funds available to spend. In addition to public key outputs users can also directly commit script outputs to a Head in order to accomodate different use cases.

Depending on when a commit should happens we can distinguish two types of commits:

- Before the head is open: **Commit**
- When the head is already open: **Incremental commit**

Both types of commits share the same API of requesting a transaction via `POST /commit` where the only difference is the returned transaction being either a `commitTx` or `depositTx` in the L1 protocol. See [protocol](./protocol.md) or [specification](./specification.md) for more details.

Depending on what is to be committed, a user needs to decide between a _simple_ or _blueprint_ commit request:

- **Simple** request to commit a public key output to a Head in which case they only need to show the JSON representation of the `UTxO` they want to spend. The returned transaction then needs to be signed by the correct private key to prove ownership.

- **Blueprint** requests allow to provide a _blueprint_ transaction together with the `UTxO` it spends to be able to have more fine grained control over the created commit/deposit transaction. This is crucial when comitting script `UTxO` where more control over some of the transaction attributes is needed (redeemers, reference inputs, validity bounds, etc.).

:::info
Note that blueprint transaction outputs are ignored as all the value will be locked into the Hydra L1 scripts (commit or deposit validators).
:::

:::danger
TODO: Add section about how to make scripts spendable in a a Hydra head commit/deposit or link a document form the user manual.

Also, should add more rationale to _why_ the design is this way here.
:::

For concrete examples on how to use the commit API see the [simple](./../how-to/incremental-commit) and [blueprint](./../how-to/commit-blueprint) commit how-tos in the user manual.

TODO: section below feels a bit out-of place?

### Commit process

When the Head is already opened we are talking about _incremental commit_ and
what happens _under the hood_ is a bit different.

To be able to commit to a running Head `hydra-node` would lock the provided
`UTxO` in the **deposit** script output controlled by the `hydra-node`.

It would also set the deadline by which this locked `UTxO` needs to be picked
up and made available inside of L2 by posting a **increment** transaction. This
also happens automatically but here we want to describe this process and bring
it closer to Hydra users.

::::info
Deposit deadline is specified as argument to `hydra-node` executable eg.
`--deposit-deadline "100s"`. Deadline information is present in the
`CommitRecorded` API server output.
::::


### Recovering the deposited funds

If for some reason `hydra-node` failed to observe the deposit there is a
**recover** process in place so that users can always have a way to unlock
their funds.

If the deadline is reached and the requested commit didn't show up on L2
users can request a recover by providing a `TxId` of the deposit transaction
which initially locked the funds.

::::info
Users can also request to see pending deposits. See our api [documentation](/api-reference).
::::

Any Head participant can request to recover the deposit not only the one which initially deposited the funds.









