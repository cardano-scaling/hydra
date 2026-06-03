# Known issues and limitations

Before running a Hydra node on the Cardano mainnet, it is important to be aware of several known issues and limitations. Operating a Hydra node requires a deep understanding of the underlying infrastructure, and you may risk your funds if you are unfamiliar with the implementation and usage processes.

### Head protocol limits

Due to the limitations on transaction sizes and execution budgets on Cardano, the Hydra protocol has the following constraints:

- The protocol can only handle a maximum number of participants in a head. When attempting to configure too many peers, the Hydra node will inform you of the current configured maximum.

Currently, participants may be denied access to their funds by other protocol participants at different stages within a Hydra head because of the complexity or size of the UTXO being deposited or created while the head is open:

- Tokens that are minted and not burned within an open Hydra head will prevent the head from being _finalized_. Partial fanout can still distribute ADA-only outputs, but any UTxOs carrying unburned native tokens will remain stuck. See [#2334](https://github.com/cardano-scaling/hydra/issues/2334) for ongoing investigation into whether these funds can be recovered.
- A single UTxO that is intrinsically too large to include in any Cardano transaction. For example, one carrying an unusually large inline datum cannot be fanned out regardless of how many steps are used. This is a Cardano-level transaction size constraint, not a Hydra limitation. If such a UTxO is within a Hydra head when the head is closed, you will be unable to fan it out.

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
here: [Etcd Configuration](configuration#auto-compaction-of-networking-buffers)

#### Adapting to new breaking changes

If the hydra-node has breaking changes in regards to reading the files it stores in the `persistence` folder, it used to be recommended to just delete the entire folder.

Now, because of etcd, it is important to only delete the `hydra-node` specific files; not the files associated with `etcd`. In particular you may like to delete the following files:

- `persistence/state*`

Note that, as with any adjustments of this kind, it is good practice to make a backup first!

### Training wheels

The following restrictions apply when **depositing** funds into a Hydra head (via `POST /commit`):

- **Byron addresses are not supported.** Any UTxO held at a Byron-era address will be rejected with an error. Only Shelley-era (and later) addresses are accepted.

### Deposit periods

The `--deposit-period` allows an individual `hydra-node` operator to decide how long they want a deposit to have settled at least. However, differences bigger than [`defaultTTL * waitDelay`](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Node.html#v:waitDelay) (currently 10 minutes) result in non-approved snapshots. This is due to the way the `HeadLogic` is implemented and snapshot requests are not retried currently. See [hydra#1999](https://github.com/cardano-scaling/hydra/issues/1999) for more context.

### Known bugs

Refer to the project repository issue tracker for [known issues](https://github.com/cardano-scaling/hydra/issues?q=is%3Aissue%20is%3Aopen%20(label%3A%22bug%20%3Abug%3A%22%20OR%20type%3ABug)). If you discover any security-relevant problems, please follow our [security policy](https://github.com/cardano-scaling/hydra?tab=security-ov-file#readme).
