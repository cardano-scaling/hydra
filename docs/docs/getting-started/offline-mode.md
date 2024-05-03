Hydra supports an offline mode, which allows for disabling the Layer 1 interface (that is, the underlying Cardano blockchain which Hydra heads use to seed funds and ultimately funds are withdrawn to). Disabling Layer 1 interactions allows use-cases which would otherwise require running and configuring an entire Layer 1 private devnet.

In this offline mode, only the Layer 2 ledger is run. Therefore, ledger genesis parameters that normally influence things like time-based transaction validation, may be set to defaults that aren't reflective of mainnet. To set this, set --ledger-protocol-parameters to a non-zero file. TODO: show how to obtain this.

To initialize the Layer 2 ledger's UTXO state, offline mode takes an obligatory --initial-utxo parameter, which points to a JSON encoded UTXO file. This UTXO is independent of Event Source loaded events, and the latter are validated against this UTXO. The UTXO follows the following schema `{ txout : {address, value : {asset : quantity}, datum, datumhash, inlinedatum, referenceScript }`

An example UTXO:
```json
{"1541287c2598ffc682742c961a96343ac64e9b9030e6b03a476bb18c8c50134d#0":{"address":"addr_test1vqg9ywrpx6e50uam03nlu0ewunh3yrscxmjayurmkp52lfskgkq5k","datum":null,"datumhash":null,"inlineDatum":null,"referenceScript":null,"value":{"lovelace":100000000}},"39786f186d94d8dd0b4fcf05d1458b18cd5fd8c6823364612f4a3c11b77e7cc7#0":{"address":"addr_test1vru2drx33ev6dt8gfq245r5k0tmy7ngqe79va69de9dxkrg09c7d3","datum":null,"datumhash":null,"inlineDatum":null,"referenceScript":null,"value":{"lovelace":100000000}}}```