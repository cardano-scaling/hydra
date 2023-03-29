# Troubleshooting

## Known issues

### hydra-node

- The current transaction size has a limit of ~16KB. This causes the following inconveniences:

  - The protocol can only handle a maximum number of participants by Head. See [cost of collectcom transaction](https://hydra.family/head-protocol/benchmarks/transaction-cost/#cost-of-collectcom-transaction) or the `hydra-node` will inform you of the current configured maximum when trying to configure too many peers.

  - Only one or no utxo can be committed by each party to a Head.

  - The head cannot be finalized if holding more than ~100 assets. See [cost of fanout transaction](https://hydra.family/head-protocol/benchmarks/transaction-cost/#cost-of-fanout-transaction) for latest numbers.

- There are couple of ideas around reducing the risk of loosing funds in a Hydra Head:
  - [Always abortable Head](https://github.com/input-output-hk/hydra/issues/699)
  - [Limit size/complexity of UTxOs in the Head](https://github.com/input-output-hk/hydra/issues/698)
  - [Only sign closable snapshots](https://github.com/input-output-hk/hydra/issues/370)

* Not an issue, but a workaround: The internal wallet of `hydra-node` requires a UTXO to be marked as "fuel" to drive the Hydra protocol transactions. See [user manual](https://hydra.family/head-protocol/docs/getting-started/demo/with-docker/#seeding-the-network).

### hydra-tui

- TUI crashes when user tries to post a new transaction wihout any UTXO remaining.

- Recipient addresses to send money to in the TUI are inferred from the current UTXO set. If a party does not commit a UTXO or consumes all its UTXO in a Head, it won't be able to send or receive anything anymore.
