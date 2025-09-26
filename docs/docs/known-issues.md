# Known issues and limitations

Before running a Hydra node on the Cardano mainnet, it is important to be aware of several known issues and limitations. Operating a Hydra node requires a deep understanding of the underlying infrastructure, and you may risk your funds if you are unfamiliar with the implementation and usage processes.

### Head protocol limits

Due to the limitations on transaction sizes and execution budgets on Cardano, the Hydra protocol has the following constraints:

- The protocol can only handle a maximum number of participants in a head (see [the cost of CollectCom transaction](https://hydra.family/head-protocol/benchmarks/transaction-cost#collectcom-transaction-costs)). When attempting to configure too many peers, the Hydra node will inform you of the current configured maximum.

Currently, participants may be denied access to their funds by other protocol participants at different stages within a Hydra head because of the complexity or size of the UTXO being committed or created while the head is open:

- The Hydra head cannot be _finalized_ if it holds more than approximately 80 assets (see [the cost of FanOut transaction](https://hydra.family/head-protocol/benchmarks/transaction-cost#fanout-transaction-costs) for latest numbers), although it can be _closed_
- Tokens that are minted and not burned within an open Hydra head will prevent the head from being _finalized_
- If one or more participants commit UTXOs that are too large to be processed together in a `CollectCom` or  `Abort` transaction, the Hydra head will remain stuck in the _initialising_ stage.

See these resources for additional information about reducing the risk of locking up funds in a Hydra head:

* [Directly open heads](https://github.com/cardano-scaling/hydra/issues/1329)
* [Always abortable head](https://github.com/cardano-scaling/hydra/issues/699)
* [Limit size/complexity of UTXOs in the head](https://github.com/cardano-scaling/hydra/issues/698)
* [Only sign closable snapshots](https://github.com/cardano-scaling/hydra/issues/370).

### Static topology

The network topology needs to be statically configured and match across all `hydra-node` instances. Currently this means that the `--peer` command line options need to match between nodes. Otherwise the `etcd` node of the networking later will report errors in the logs.

Known errors are:

 - `cluster ID mismatch` - the cluster was initiated with a different list of `--peer`s
   - check configuration with other participants. There should be a corresponding log entry `NetworkClusterIDMismatch` with the information on:
      - `clusterPeers` - loaded peers info received from `etcd` cluster.
      - `configuredPeers` - peers info coming from `hydra-node` arguments.

 - `member ... has already been bootstrapped` - missing information in `<persistence-dir>/etcd`
   - restart your hydra-node with the `ETCD_INITIAL_CLUSTER_STATE` environment variable set to `existing` (`new` is the default), see also https://etcd.io/docs/v3.3/op-guide/configuration/

We should be able to work around these UX issues using [etcd discovery](https://etcd.io/docs/v3.5/op-guide/clustering/#etcd-discovery) eventually.

#### Auto-compaction (amount of time a peer can be offline)

Because we do not want the hydra-node to take up unbounded disk space, we set
a conservative amount of history that the internal `etcd` process will store.

This can be controlled via environment variables that you can read more about
here: [Etcd Configuration](configuration#networking-configuring-the-limits-of-etcd-networking-recovery)

#### Adapting to new breaking changes

If the hydra-node has breaking changes in regards to reading the files it stores in the `persistence` folder, it used to be recommended to just delete the entire folder.

Now, because of etcd, it is important to only delete the `hydra-node` specific files; not the files associated with `etcd`. In particular you may like to delete the following files:

- `persistence/state*`

Note that, as with any adjustments of this kind, it is good practice to make a backup first!

### Training wheels

There is a hard-coded limit in hydra-node when used on **mainnet**: only up to 100 ada can be committed into the Hydra head. This is a safety precaution and will be increased as more experience is gained in running Hydra heads on the mainnet.

### Deposit periods

The `--deposit-period` allows an individual `hydra-node` operator to decide how long they want a deposit to have settled at least. However, differences bigger than [`defaultTTL * waitDelay`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Node.html#v:waitDelay) (currently 10 minutes) result in non-approved snapshots. This is due to the way the `HeadLogic` is implemented and snapshot requests are not retried currently. See [hydra#1999](https://github.com/cardano-scaling/hydra/issues/1999) for more context.

### Known bugs

Refer to the project repository issue tracker for [known issues](https://github.com/cardano-scaling/hydra/issues?q=is%3Aissue+is%3Aopen+label%3A%22bug+%3Abug%3A%22). If you discover any security-relevant problems, please follow our [security policy](https://github.com/cardano-scaling/hydra?tab=security-ov-file#readme).
