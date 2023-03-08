# Troubleshooting

## Known issues

### hydra-node

- Hydra node crashes after a fork

    + It ocurs during a fork and expects the operator to restart its hydra-node.

    + Reference: [issue #560](https://github.com/input-output-hk/hydra/issues/560) Hydra node crashed after a fork.
    
    + Workaround: Restarting your node should be enough to come back to live.

- The current transaction size has a limit of ~16KB. This causes the following inconveniences:

    + The protocol can only handle a maximum number of participants by Head. See [cost of collectcom transaction](https://hydra.family/head-protocol/benchmarks/transaction-cost/#cost-of-collectcom-transaction) or the `hydra-node` will inform you of the current configured maximum when trying to configure too many peers.

    + Only one or no utxo can be committed by each party to a Head.

    + The head cannot be finalized if holding more than ~100 assets. See [cost of fanout transaction](https://hydra.family/head-protocol/benchmarks/transaction-cost/#cost-of-fanout-transaction) for latest numbers.

- Comitting a `UTxO` with a `ReferenceScript` will make the head not finalizable.

   + The `hydra-node` does prevent this from happening, but transactions in the
     Head protocol done by other implementations may result in the Head be not
     finalizable by `hydra-node`.

   + Creating output with reference scripts on the layer 2 ledger is
     non-problematic.

   + If you run into this, file an issue and the Hydra team will be able to help
     you finalize your head in a manual workaround.

- Not an issue, but a workaround: The internal wallet of `hydra-node` requires a UTXO to be marked as "fuel" to drive the Hydra protocol transactions. See [user manual](https://hydra.family/head-protocol/docs/getting-started/demo/with-docker/#seeding-the-network).

### hydra-tui

- TUI crashes when user tries to post a new transaction wihout any UTXO remaining.

- Recipient addresses to send money to in the TUI are inferred from the current UTXO set. If a party does not commit a UTXO or consumes all its UTXO in a Head, it won't be able to send or receive anything anymore.
