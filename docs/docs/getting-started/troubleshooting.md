# Troubleshooting

## Known issues

- QueryException "AcquireFailurePointNotOnChain"

    + Description: It ocurs when you attempt to start a head using a point in the past too old (exceeding **k** limit).

    + Reference: [439](https://github.com/input-output-hk/hydra/issues/439) AcquireFailurePointTooOld when `--start-chain-from` point is past **k**

    + Workaround: Restart your node fresh, without state. For that you need to remove your persistance dir and restart the hydra-node.

- Hydra node crashes after a fork

    + Description: It ocurs during a fork and expects the operator to restart its hydra-node.

    + Reference: [560](https://github.com/input-output-hk/hydra/issues/560) Hydra node crashed after a fork
    
    + Workaround: Restarting your node should be enough to come back to live. Beware, if you wait to long to restart it then you may fall under `QueryAcquireException AcquireFailurePointTooOld` and will require you to restart without state.

- Head can collect at most 3 commits and each party can commit either 1 or 0 UTXO to a Head. Only no or one utxo can be committed to a Head.

- Recipient addresses to send money to in the TUI are inferred from the current UTXO set. If a party does not commit a UTXO or consumes all its UTXO in a Head, it won't be able to send or receive anything anymore.

- TUI crashes when user tries to post a new transaction wihout any UTXO remaining.

- Not an issue, but a workaround: The internal wallet of `hydra-node` requires a UTXO to be marked as "fuel" to drive the Hydra protocol transactions. See [user manual](https://hydra.family/head-protocol/docs/getting-started/demo/with-docker/#seeding-the-network).

- Aborting a head with more than 2 participants (i.e. > 2) requires increase in tx size limit over current mainchain parameters to ~20KB. `collectComTx` requires increase in tx size limit over current mainchain parameters to 32KB, which should be alleviated with Plutus optimisations and merging all contracts in one in future releases.

- The head cannot be finalized if holding more than ~100 assets (or ~50 ada-only UTxO entries) with the standard tx size of 16KB. `fanoutTx` cannot handle more than 100 UTxO with the standard tx size of 16KB (200 with the temporary increase for test purpose). 
