# Known issues and limitations

Before running a Hydra node on the Cardano mainnet, it is important to be aware of several known issues and limitations. Operating a Hydra node requires a deep understanding of the underlying infrastructure, and you may risk your funds if you are unfamiliar with the implementation and usage processes.

### Head protocol limits

Due to the limitations on transaction sizes and execution budgets on Cardano, the Hydra protocol has the following constraints:

- A head supports at most **29 parties**. Mirrors do not add parties.
- The accumulator supports at most **4,095 outputs**, counting pending deposits or decommits alongside the head UTxO. See [head size limits](./dev/architecture/partial-fanout.md#head-size-limit).
- Fanout can use multiple transactions, but every individual output must still fit L1 size and minimum-ADA constraints. Use the [recommended ledger parameters](./configuration.md#ledger-parameters).

Currently, participants may be denied access to their funds by other protocol participants at different stages within a Hydra head because of the complexity or size of the UTXO being deposited or created while the head is open:

- Tokens that are minted and not burned within an open Hydra head will prevent the head from being _finalized_. [Partial fanout](dev/architecture/partial-fanout.md#unburned-tokens) can still distribute the unaffected outputs, but any UTxO carrying such a token cannot be fanned out, and neither can the last transaction, which has to distribute everything that is left and burn the head tokens. The same applies to a token the head output holds but cannot burn, for instance one under a foreign policy. See [#2334](https://github.com/cardano-scaling/hydra/issues/2334) for ongoing investigation into whether these funds can be recovered.
- A single UTxO that is intrinsically too large to include in any Cardano transaction. For example, one carrying an unusually large inline datum cannot be fanned out regardless of how many steps are used. This is a Cardano-level transaction size constraint, not a Hydra limitation. If such a UTxO is within a Hydra head when the head is closed, you will be unable to fan it out.

### Static topology

The network topology needs to be statically configured and match across all `hydra-node` instances. Currently this means that the `--peer` command line options need to match between nodes. Otherwise the `etcd` node of the networking later will report errors in the logs.

Known errors are:

 - `cluster ID mismatch` - the cluster was initiated with a different list of `--peer`s
   - check configuration with other participants. There should be a corresponding log entry `NetworkClusterIDMismatch` with the information on:
      - `clusterPeers` - loaded peers info received from `etcd` cluster.
      - `configuredPeers` - peers info coming from `hydra-node` arguments.

 - `member ... has already been bootstrapped` - missing information in `<persistence-dir>/etcd`
   - restart your hydra-node with the `ETCD_INITIAL_CLUSTER_STATE` environment variable set to `existing` (`new` is the default), see also https://etcd.io/docs/v3.6/op-guide/configuration/

We should be able to work around these UX issues using [etcd discovery](https://etcd.io/docs/v3.6/op-guide/clustering/#etcd-discovery) eventually.

#### Auto-compaction (amount of time a peer can be offline)

Because we do not want the hydra-node to take up unbounded disk space, we set
a conservative amount of history that the internal `etcd` process will store.

This can be controlled via environment variables that you can read more about
here: [Etcd Configuration](configuration#auto-compaction-of-networking-buffers)

#### Adapting to new breaking changes

Plan upgrades with every party in the head. Before stopping nodes, read the
release notes for changes to scripts, snapshot signatures, network encoding,
and persistence. Settle existing heads before switching to an incompatible
script or snapshot format; a database migration does not migrate an on-chain
head. In particular, the 2.4.0 snapshot-signing changes require existing heads
to be closed and fanned out before upgrading.

After a clean shutdown, back up each node's entire persistence directory and
keys. Keep SQLite files together, including any WAL files, along with etcd
state and `pending-broadcast/`. Do not delete state to make an upgrade start.

The node automatically migrates legacy state files to `hydra.db` and older
SQLite payloads to CBOR. After a successful CBOR migration, older releases
cannot read that database. Keep the pre-upgrade backup; do not resume an old
backup after the head has made new progress. See
[persistence migrations](./dev/architecture/event-sourcing.md#schema-versioning).

Upgrade all peers to a compatible [network protocol version](./configuration.md#network-protocol-version-and-upgrades).
The embedded etcd version also has its own data-format compatibility rules;
replacing the Hydra executable does not make an etcd downgrade safe.

#### Pending outbound messages

`<persistence-dir>/pending-broadcast/` holds messages this node has broadcast
that the cluster has not accepted yet. It fills while the node cannot reach a
majority of the `etcd` cluster, and drains once it can.

Do not delete it as part of a routine upgrade cleanup. Deleting it discards messages the other participants
never saw, which breaks the reliable-broadcast guarantee the off-chain
protocol relies on and can leave the head unable to make progress. The
recovery for a growing `pending-broadcast/` is to restore quorum by bringing
peers back, not to remove files.

The metrics to watch are `hydra_head_pending_broadcasts`,
`hydra_head_broadcast_no_progress_seconds` and `hydra_head_broadcast_stalled`
on the node's `--monitoring-port`; see
<a href="../benchmarks/metrics#diagnosing-a-stalled-broadcast">runtime metrics</a>.

While the queue is not draining, the node reports `NetworkBroadcastStalled`
(and later `NetworkBroadcastResumed`) to its clients, and refuses new `NewTx`
and `Decommit` submissions with `RejectedInputBecauseBroadcastStalled`
(HTTP 503) to keep the backlog from growing. Closing, contesting and fanning
out the head are never refused.

### Training wheels

The following restrictions apply when **depositing** funds into a Hydra head (via `POST /commit`):

- **Byron addresses are not supported.** Any UTxO held at a Byron-era address will be rejected with an error. Only Shelley-era (and later) addresses are accepted.

### Known bugs

Refer to the project repository issue tracker for [known issues](https://github.com/cardano-scaling/hydra/issues?q=is%3Aissue%20is%3Aopen%20(label%3A%22bug%20%3Abug%3A%22%20OR%20type%3ABug)). If you discover any security-relevant problems, please follow our [security policy](https://github.com/cardano-scaling/hydra?tab=security-ov-file#readme).
