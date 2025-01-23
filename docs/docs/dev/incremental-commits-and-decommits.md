# Incremental commits and decommits

These two new addons to the initial Hydra Head protocol deserve more
explanation so our users are aware of how they work _under the hood_ to bring
more clarity to these processes.

For now these two new additions run sequentially so we are doing one thing at a
time, at least for now, while we will think about batching certain actions in
the future if the need for that arises.

It is only possible to either commit or decommit - we don't allow snapshots with both
fields specified for simplicity. This restriction might be lifted later on - once we
are sure this simpler version works nicely.

## Incremental Commits

Incremental Commits allow us to take some `UTxO` from L1 and make it available
on L2 for transacting inside of a running Hydra Head.

The process for incremental commits is pretty much the same as when
_committing_ before the Head is in the `Open` state. In fact we can open a Head
without committing some funds and then _top-up_ our L2 funds by doing incremental
commits.

The process of incrementally committing a `UTxO` starts by sending a `HTTP` request to
the hydra-node API endpoint:

```bash

curl -X POST <IP>:<PORT>/commit --data @commit.json
```

:::info

Note that the commit transaction, which is sent to the hydra-node API, only needs
to specify the transaction inputs present in L1 that we want to make available
on L2. It will ignore any specified outputs and instead the owner of
incremented `UTxO` on L2 is the same one that owned the funds on L1.

:::

Hydra node will accept a plain `UTxO` encoded as JSON in the `POST` request
body or a _blueprint_ transaction together with the `UTxO` used to resolve it's
inputs.

_Blueprint_ transaction is just like a recipe that describes which transaction
inputs should be made available on L2 network ignoring any specified outputs.
It goes together with a `UTxO` used to resolve the transaction inputs. It's
purpose is to prove that one can spend specified transaction inputs.

Successfull API response includes a _deposit_ transaction that needs to be
signed and submitted by the user in order to kick-off the deposit process.

This process just locks the specified `UTxO` at a deposit script address which
will then, later on, after confirmed snapshot, be unlocked by the _increment_
transaction which will actually make this `UTxO` available on L2.

The deposit transaction contains a deadline - time window in which we expect
the hydra-node to be able to observe this deposit and issue a _increment_
transaction that will do the heavy lifting and bring the specified input on L2.

Deposit deadline value is specified as the `hydra-node` option eg.
`--deposit-deadline "100s"`

Once a hydra-node observes a deposit transaction it will record the deposit as
pending into the local state. There can be many pending deposits but the new
Snapshot will include them one by one.

When this new Snapshot is acknowledged by all parties _increment_ transaction
will be posted by the leader.

:::info
Note that any node that posts increment transaction will also pay the fees even if
the deposit will not be owned by them on L2.
:::

Upon observing increment transaction we remove deposit from the local pending deposits
and the process can start again.

:::note

Since we can potentially request many deposits, the leader will increment only
one of them. While others are stuck in the pending state any new transaction on
L2 will take next pending deposit and try to include it in a snapshot.

:::

## Incremental Decommits

Incremental decommits allow us to take some L2 `UTxO` and bring it to the L1
while the Head protocol is running.

Head participant (or any other user that can send requests to the hydra-node
API endpoint) requests inclusion of some UTxO from L1 by sending a `POST`
`HTTP` request which contains in the request body a decommit transaction
encoded as _TextEnvelope_ JSON value.

```bash
curl -X POST <IP>:<PORT>/decommit --data @decommit-tx.json
```

This transaction needs to be signed by the owner of the funds on L2.

:::info

What we call a decommit transaction is the one that user supplies in the API
endpoint. The decrement transaction is the transaction that hydra-node posts
after it checks that decommit transaction applies and the one that actually
makes some UTxO available on L1.

:::

Hydra node accepts this transaction and checks if it can be cleanly applied to
the local `UTxO` set. After this check hydra-node will issue a `ReqDec` message
signalling to other parties that we want to produce a new `Snapshot` that
contains the same  `UTxO` to decommit. Once a snapshot is signed, hydra-node
posts a _decrement_ transaction that will take specified output and make it
available on L1.


