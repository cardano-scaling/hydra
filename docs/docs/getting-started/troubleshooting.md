# Troubleshooting

## Known issues

### hydra-node

- AcquireFailurePointNotOnChain

    + It ocurs when you attempt to start a head using a point in the past too old (exceeding **k** limit).

    + Reference: [issue #439](https://github.com/input-output-hk/hydra/issues/439) AcquireFailurePointTooOld when `--start-chain-from` point is past **k**.

    + Workaround: Restart your node fresh, without state. For that you need to remove your persistance dir and restart the hydra-node.

- Hydra node crashes after a fork

    + It ocurs during a fork and expects the operator to restart its hydra-node.

    + Reference: [issue #560](https://github.com/input-output-hk/hydra/issues/560) Hydra node crashed after a fork.
    
    + Workaround: Restarting your node should be enough to come back to live. Beware, if you wait to long to restart it then you may fall under `QueryAcquireException AcquireFailurePointTooOld` and will require you to restart without state.

- Not an issue, but a workaround: The internal wallet of `hydra-node` requires a UTXO to be marked as "fuel" to drive the Hydra protocol transactions. See [user manual](https://hydra.family/head-protocol/docs/getting-started/demo/with-docker/#seeding-the-network).

- The current standard tx size has a limit of ~16KB. This causes the following inconveniences:
    + The protocol can only handle a maximum number of participants by Head. See [cost of collectcom transaction](https://hydra.family/head-protocol/benchmarks/transaction-cost/#cost-of-collectcom-transaction). Only one or empty utxo can be committed by a party to a Head. For this reason, the Head can collect at most 4 commits and each party can commit either 1 or 0 UTXO to a Head.

    + The head cannot be finalized if holding more than ~100 assets (or ~50 ada-only UTxO entries). See [cost of fanout transaction](https://hydra.family/head-protocol/benchmarks/transaction-cost/#cost-of-fanout-transaction). This means `fanoutTx` cannot handle more than 100 UTxO (200 with the temporary increase for test purpose).

### hydra-tui

- TUI crashes when user tries to post a new transaction wihout any UTXO remaining.

- Recipient addresses to send money to in the TUI are inferred from the current UTXO set. If a party does not commit a UTXO or consumes all its UTXO in a Head, it won't be able to send or receive anything anymore.
