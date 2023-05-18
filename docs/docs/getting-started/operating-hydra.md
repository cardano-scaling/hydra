---
sidebar_position: 6
---

# Operating a Hydra Node

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```

This page aims at helping Hydra users troubleshoot issues when running their own instances of `hydra-node` and participate in a Hydra Head.

## Logs

Following [ADR-9](/adr/9) design principles, the `hydra-node` provides [JSON](https://json.org) formatted logs on the `stdout` stream, one line per log item. The log items follow a [JSON schema](https://github.com/input-output-hk/hydra/blob/master/hydra-node/json-schemas/logs.yaml). This logging capability is kept voluntarily simple and non configurable in order to ease integration of Hydra logging into more general log analysis infrastructure, whether a custom ELK stack, third-party services, docker sidecars...

:::info

There is an unpublished [log-filter](https://github.com/input-output-hk/hydra/blob/master/hydra-cluster/exe/log-filter/Main.hs) executable that one can attach to a hydra-node in order to trim down the volume of information in the log stream. This filter provides _some_ filtering features, namely removing transactions bodies and replacing them with transaction ids, but it's not general enough to warrant publication. Similar capabilites can be easily provided with tools like [jq](https://stedolan.github.io/jq/).

:::

## Monitoring

When given `--monitoring-port PORT` argument, the hydra-node executable will expose a [Prometheus](https://prometheus.io) compatible HTTP `/metrics` endpoint on the given port to enable _scraping_ of exposed metrics.

For example, assuming a hydra-node was started with `--monitoring-port 6001`, this command

```mdx-code-block
<TerminalWindow>
curl http://localhost:6001/metrics
</TerminalWindow>
```

will output

```
# TYPE hydra_head_confirmed_tx counter
hydra_head_confirmed_tx  0
# TYPE hydra_head_events counter
hydra_head_events  50467
# TYPE hydra_head_requested_tx counter
hydra_head_requested_tx  0
# TYPE hydra_head_tx_confirmation_time_ms histogram
hydra_head_tx_confirmation_time_ms_bucket{le="5.0"} 0.0
hydra_head_tx_confirmation_time_ms_bucket{le="10.0"} 0.0
hydra_head_tx_confirmation_time_ms_bucket{le="50.0"} 0.0
hydra_head_tx_confirmation_time_ms_bucket{le="100.0"} 0.0
hydra_head_tx_confirmation_time_ms_bucket{le="1000.0"} 0.0
hydra_head_tx_confirmation_time_ms_bucket{le="+Inf"} 0.0
hydra_head_tx_confirmation_time_ms_sum  0.0
hydra_head_tx_confirmation_time_ms_count  0
```

## Common Issues

### No Head is observed from the chain

* `hydra-node` is connected to a `cardano-node` that's on the wrong
  network. Check the `--network` command-line argument and the
  `cardano-node` configuration
* Note that the `hydra-node` cannot start if it cannot connect to the
  `cardano-node`, which might require some time as the `cardano-node`
  needs to revalidate its database and possibly even reconstruct its
  ledger state when it starts and its connections are not open until
  it's ready. If running as a service or a container, make sure the orchestrator restarts the process when it crashes
* The _Scripts_ transaction identifier is invalid. This transaction id
  is available in the
  [release](https://github.com/input-output-hk/hydra/releases/tag/0.10.0)
  page for the 3 major networks (`preview`, `preprod`, `mainnet`)
* The `hydra-node`'s _Cardano signing key_ is inconsistent with the
  _Verification key_ from the `Init` transaction. Check the
  `--cardano-signing-key` parameter points to the right key, and that
  peers have the correct `--cardano-verification-key` for your host.
* The peers' _Cardano verification keys_ are incorrect. This is
  mirroring the above issue, check parameters on all peers.

### Head does not make progress

* Peers are not correctly connected to each others'. Check the
  `--peer` arguments point to the right `host:port` for each
  peer. `PeerConnected` message should be sent to the client (or
  appears in the logs) and be consistent for all peers involved in a
  Head.
* The _Hydra signing key_ for our node or the _Hydra verification
  keys_ for peers do not match what's expected by each node. Check
  that `AckSn` messages are received by all parties and that the
  `LogicOutcome` log does not contain any `Error`
