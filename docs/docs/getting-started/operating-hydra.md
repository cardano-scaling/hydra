---
sidebar_position: 3
---

# Troubleshooting

This page aims at helping Hydra users troubleshoot issues when running their own instances of `hydra-node` and participate in a Hydra Head.

## Logs

Following [ADR-9](/adr/9) design principles, the `hydra-node` provides [JSON](https://json.org) formatted logs on the `stdout` stream, one line per log item. The log items follow a [JSON schema](https://github.com/input-output-hk/hydra/blob/8a8157d8cba4907e1653e2fbb87551cf7ddd59d8/hydra-node/json-schemas/logs.yaml).


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
