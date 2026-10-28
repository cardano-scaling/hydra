---
sidebar_position: 7
---

# Operate a Hydra node

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```

This page guides Hydra users on troubleshooting issues when running their instances of `hydra-node` and participating in a Hydra head.

## Example setup

We offer sample node configurations that will help you get started with hosting a Hydra node on virtual machines in the cloud. These configurations are available in the [`sample-node-config/` directory](https://github.com/cardano-scaling/hydra/tree/master/sample-node-config/).


## Logs

Following the principles outlined in [ADR-9](/adr/9), the `hydra-node` emits [JSON](https://json.org) formatted logs to the `stdout` stream, with one log item per line.

## Monitoring

When the `--monitoring-port PORT` argument is provided, the `hydra-node` executable will expose a [Prometheus](https://prometheus.io) compatible HTTP `/metrics` endpoint on the specified port to enable metrics scraping.

For instance, if a `hydra-node` is initiated with `--monitoring-port 6001`, the following command:


```mdx-code-block
<TerminalWindow>
{`curl http://localhost:6001/metrics`}
</TerminalWindow>
```

will return Prometheus text with `HELP` and `TYPE` metadata. For example:

```text
# HELP hydra_head_confirmed_tx_total Transactions in newly confirmed or adopted snapshot numbers observed in this process; repeated adoption excluded.
# TYPE hydra_head_confirmed_tx_total counter
hydra_head_confirmed_tx_total  0
# HELP hydra_node_synced One when the current node state is in sync.
# TYPE hydra_node_synced gauge
hydra_node_synced  0.0
```

The endpoint is available during initialization. A successful scrape means the
process is reachable. Assess synchronization with `hydra_node_synced`, network
connectivity with `hydra_network_connected`, and actual work through the progress
and backlog metrics. An occupied monitoring port fails startup instead of
silently disabling metrics.

See the [metrics reference](/benchmarks/metrics#runtime-metrics-prometheus) for
all families, units, bounded labels, timing boundaries and compatibility aliases.
The demo Grafana dashboards and Prometheus alert rules use canonical counters
ending in `_total` and latency histograms in seconds. Existing names continue to
work, including the millisecond histograms used by older consumers.

Peer count is based on distinct processed connectivity reports and becomes zero
when network disconnection is processed. Zero peers can be healthy in a
single-party deployment. `hydra_network_connected` and the broadcast health
gauges read live producers independently of the processing queue.

To distinguish a blocked main loop from a silent chain backend, compare
`hydra_chain_last_block_timestamp_seconds` (last processed tick) with
`hydra_chain_backend_last_block_timestamp_seconds` (last raw roll-forward).
Ignore their initial zero values when computing heartbeat ages.

## Common issues

### No head is observed from the chain

* Ensure the `hydra-node` is connected to a `cardano-node` operating on the correct network. Verify the `--network` command-line argument and the `cardano-node` configuration.
* Remember, the `hydra-node` cannot start if it cannot connect to the `cardano-node`, which might require time as the `cardano-node` must revalidate its database and potentially reconstruct its ledger state upon startup. Its connections are not open until it is fully prepared. If running as a service or a container, ensure that the orchestrator restarts the process when it crashes.
* Check that the _Scripts_ transaction identifier is valid. This identifier is provided on the [release](https://github.com/cardano-scaling/hydra/releases/latest) page for the three major networks (`preview`, `pre-production`, `mainnet`).
* Verify that the `hydra-node`'s _Cardano signing key_ is consistent with the _Verification key_ from the `Init` transaction. Ensure the `--cardano-signing-key` parameter points to the correct key, and that peers have the accurate `--cardano-verification-key` for your node.
* Confirm that peers' _Cardano verification keys_ are accurate. This mirrors the above issue; check parameters on all peers.

### Head does not make progress

* Confirm peers are properly connected to each other. Verify the `--peer` arguments point to the correct `host:port` for each peer. The `PeerConnected` message should be observed by the client or appear in the logs and be consistent across all peers involved in a head.
* Ensure the _Hydra signing key_ for your node or the _Hydra verification keys_ for peers match each node's expectations. Verify that `AckSn` messages are received by all parties and that the `LogicOutcome` log contains no errors.

### Head Stuck: Peer Out of Sync

Processing transactions in a Hydra head requires each node to agree on transactions, which occurs when they sign a snapshot during the AckSn phase. The protocol validates transactions (on the NewTx command) against its local view of the ledger state, using the provided --ledger-protocol-parameters. Since transaction validity depends on configuration (and, to some extent, the exact build versions of hydra-node), one node may accept a transaction while its peers reject it.

When this happens, the accepting node's local state diverges from the rest, potentially leading to attempts to spend outputs that other nodes do not recognize as available.

This issue can also arise if a peer goes offline while a transaction is submitted, potentially causing the Hydra head to become stuck and preventing further snapshots from being signed ([see test case](https://github.com/cardano-scaling/hydra/pull/1780)).

As a result of this divergence, the local ledger state of each Hydra node essentially becomes forked, resulting in inconsistent states across the network. While it is technically still possible to submit transactions to the Hydra nodes, doing so is ineffective because snapshots do not update unless all nodes sign them. Each node starts accepting transactions based on entirely different states, leading to disagreement on which UTxOs have been spent.

To recover from this issue, we introduced side-loading of snapshots to synchronize the local ledger state of the Hydra nodes. With this mechanism, every peer reverts to the latest confirmed snapshot. This can be done by using latest snapshot confirmed from GET /snapshot to call the POST /snapshot endpoint, which clears the peer’s local pending transactions and restores its state to match the last agreed snapshot, allowing the node to rejoin the consensus with the rest of the network.

Newer confirmed snapshots can also be adopted if all party members use the POST /snapshot endpoint with the same ConfirmedSnapshot as the JSON body.

It is important to note that this recovery process is a coordinated effort among peers to ensure the consistency and availability of the Hydra head.

### Availability with mirror nodes

A mirror runs another instance of an existing party using the same Cardano
and Hydra signing keys. Keep separate persistence directories, unique node
IDs and advertised addresses, and a consistent etcd peer list. List each
party's verification keys only once. See the
[mirror configuration](../configuration.md#mirror-nodes).

Treat a mirror as a warm standby. Synchronized instances can produce duplicate
acknowledgments, but a lagging instance can miss snapshot progress and
simultaneous L1 actions compete for the same inputs. Monitor synchronization
before relying on a mirror to take over. Replicating signing keys also expands
the set of machines that must be trusted.

Two conditions are needed for off-chain progress:

- Every Hydra party must have a synchronized instance available to sign.
- The etcd cluster must retain a connected majority: `floor(n / 2) + 1` of its
  `n` members.

A cluster can tolerate at most `floor((n - 1) / 2)` unavailable etcd members,
provided the remaining members can communicate. This is a failure limit,
not a limit on how many mirrors to configure.

| etcd members | Required majority | Maximum unavailable members |
| --- | --- | --- |
| 3 | 2 | 1 |
| 4 | 3 | 1 |
| 5 | 3 | 2 |
| 6 | 4 | 2 |

For example, three Alice instances and one Bob instance need three etcd
members online. Losing two Alice instances breaks quorum even though one
instance of each party remains. Losing Bob prevents snapshot signing even
if the three Alice instances retain quorum. Back up persistence and keys;
mirrors do not replace backups.
