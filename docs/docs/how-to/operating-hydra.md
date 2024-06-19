---
sidebar_position: 3
---

# Operate a Hydra Node

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```

This page guides Hydra users on troubleshooting issues when running their instances of `hydra-node` and participating in a Hydra head.

## Example setup

We offer sample node configurations that will help you get started with hosting a Hydra node on virtual machines in the cloud. These configurations are available in the [`sample-node-config/` directory](https://github.com/input-output-hk/hydra/tree/master/sample-node-config/).


### Google Cloud with Terraform

This setup includes a [docker-compose.yaml](https://github.com/input-output-hk/hydra/blob/master/sample-node-config/gcp/docker-compose.yaml) file, which serves as a robust template for configuring `cardano-node` and `hydra-node` services. Additionally, various scripts are provided to assist with setting up your cluster.


## Logs

Following the principles outlined in [ADR-9](/adr/9), the `hydra-node` emits [JSON](https://json.org) formatted logs to the `stdout` stream, with one log item per line. These log entries conform to a specific [JSON schema](https://github.com/input-output-hk/hydra/blob/master/hydra-node/json-schemas/logs.yaml). We deliberately maintain the logging mechanism simple and non-configurable to facilitate the integration of Hydra logs into broader log analysis infrastructures, including custom ELK stacks, third-party services, or Docker sidecars.


## Monitoring

When the `--monitoring-port PORT` argument is provided, the `hydra-node` executable will expose a [Prometheus](https://prometheus.io) compatible HTTP `/metrics` endpoint on the specified port to enable metrics scraping.


For instance, if a `hydra-node` is initiated with `--monitoring-port 6001`, the following command:


```mdx-code-block
<TerminalWindow>
curl http://localhost:6001/metrics
</TerminalWindow>
```

will output:

```
# TYPE hydra_head_confirmed_tx counter
hydra_head_confirmed_tx  0
# TYPE hydra_head_inputs counter
hydra_head_inputs  50467
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

### No head is observed from the chain

* Ensure the `hydra-node` is connected to a `cardano-node` that's operating on the correct network. Verify the `--network` command-line argument and the `cardano-node` configuration.
* Remember, the `hydra-node` cannot start if it is unable to connect to the `cardano-node`, which might require time as the `cardano-node` must revalidate its database and potentially reconstruct its ledger state upon startup. Its connections are not open until it is fully prepared. If running as a service or a container, ensure that the orchestrator restarts the process when it crashes.
* Check that the _Scripts_ transaction identifier is valid. This identifier is provided on the [release](https://github.com/input-output-hk/hydra/releases/tag/0.10.0) page for the three major networks (`preview`, `pre-production`, `mainnet`).
* Verify that the `hydra-node`'s _Cardano signing key_ is consistent with the _Verification key_ from the `Init` transaction. Ensure the `--cardano-signing-key` parameter points to the correct key, and that peers have the accurate `--cardano-verification-key` for your node.
* Confirm that peers' _Cardano verification keys_ are accurate. This mirrors the above issue; check parameters on all peers.

### Head does not make progress

* Confirm peers are properly connected to each other. Verify the `--peer` arguments point to the correct `host:port` for each peer. The `PeerConnected` message should be observed by the client or appear in the logs and be consistent across all peers involved in a head.
* Ensure the _Hydra signing key_ for your node or the _Hydra verification keys_ for peers match each node's expectations. Verify that `AckSn` messages are received by all parties and that the `LogicOutcome` log contains no errors.
