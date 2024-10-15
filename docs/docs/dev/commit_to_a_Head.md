# Commit funds to a Head


In order to transact inside of a Head on L2 users need to have some funds
available before staring the Head protocol.

In addition to public key outputs users can also commit script outputs to a
Head in order to accomodate different usecases for dApps and add custom validator logic.

We can distinguish two types of commits:

- **Commit before the Head is open**

- **Commit when the Head is already opened (incremental commits)**

Both types of commits are pretty similar from the user perspective but the latter one
is more useful since it depreciates the old behavior where the users needed to
have available funds before the Head protocol can actually be started.

In order to commit a `UTxO` user owns on L1 they can choose to do what we call
a _simple_ or a _blueprint_ commit.

**_Simple_** here means the user wants to commit a public key output to a Head in
which case they only need to show the JSON representation of the `UTxO` they
own.

The **_blueprint_** option requires providing the _blueprint_ transaction
together with the `UTxO` it spends to be able to have more fine grained control
over the commit process. Note that in this case `hydra-node` ignores any
outputs of a user transaction and is only concerned with the **inputs** it spends.

::::info
For example when committing a script `UTxO` and when more control of some of
the transaction attributes is needed (lower/upper validity bounds, custom
redeemers, reference inputs etc.)
::::

### Commit process

Both commit options mentioned should not be different from the users
perspective and the only difference is _when_ commit is made in the Head
[lifecycle](./../protocol-overview#hydra-head-lifecycle) - before or after the Head is opened.

In any case user needs to have the `hydra-node` running and be able to issue a
request which is already described in more detailed
[simple](./../how-to/incremental-commit) and
[blueprint](./../how-to/commit-blueprint) tutorials.

Upon succesfull request `hydra-node` returns a transaction to the user which
then they need to sign and submit to the L1 network.

`hydra-node` constantly observes interesting L1 transactions and in case the
Head is not yet opened it would wait for all other Head participants to commit
before the Head protocol starts.

When the Head is already opened we are talking about _incremental commit_ and
what happens _under the hood_ is a bit different.

To be able to commit to a running Head `hydra-node` would lock the provided
`UTxO` in the **deposit** script output controlled by the `hydra-node`.

It would also set the deadline by which this locked `UTxO` needs to be picked
up and made available inside of L2 by posting a **increment** transaction. This
also happens automatically but here we want to describe this process and bring
it closer to Hydra users.

It is worthwhile mentioning that deposit deadline is double the value used for
contestation period. This gives the users control by specifying the
contestation period it in the arguments to `hydra-node` executable.

::::info
Deadline information is present in the `CommitRecorded` API server output.
::::


### Recovering the deposited funds

If for some reason `hydra-node` failed to observe the deposit there is a
**recover** process in place so that users can always have a way to unlock
their funds.

If the deadline is reached and the requested commit didn't show up on L2
users can request a recover by providing a `TxId` of the deposit transaction
which initially locked the funds.

::::info
Users can also request to see pending deposits. See our api [documentation](/api-reference/#operation-publish-/commits).
::::

Any Head participant can request to recover the deposit not only the one which initially deposited the funds.









