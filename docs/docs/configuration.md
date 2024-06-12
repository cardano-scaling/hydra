# Configuration

Running a Hydra head involves operating a Hydra node connected to other Hydra nodes and a Cardano node. The entire configuration of the `hydra-node` is managed through command-line options. Use the `--help` option to see a full list of available commands:

```shell
hydra-node --help
```

Below, we document selected aspects of the configuration. For a comprehensive guide, refer to the [tutorial](./tutorial) or specific _How To_ articles. for more details.

### Cardano Keys

In a Hydra head, each participant is authenticated using two sets of keys. The first set identifies a participant on the Cardano Layer 1 (L1) and is used to hold ADA for paying fees. Each `hydra-node` requires a `--cardano-signing-key`, and you must provide the `--cardano-verification-key` for each participant.

Generate a Cardano key pair using the `cardano-cli`:

```shell
cardano-cli address key-gen \
  --verification-key-file cardano.vk \
  --signing-key-file cardano.sk
```

These keys authenticate on-chain transactions and ensure only authorized participants can control the head's lifecycle, preventing unauthorized actors from interfering (e.g., aborting an initialized head). While this will not put participants funds at risk, it is still an annoyance that is preventable.

### Hydra keys

The second set of keys are Hydra keys, used for multi-signing snapshots within a head. Although these keys may eventually support an aggregated multi-signature scheme, they currently use the Ed25519 format.

Generate new Hydra keys using the `hydra-node`:

```
hydra-node gen-hydra-key --output-file my-key
```

This command creates two files: `my-key.sk` and `my-key.vk` containing Hydra keys suitable for use inside a head.

For demonstration purposes, we also provide demo key pairs (`alice.{vk,sk}`, `bob.{vk,sk}`, and `carol.{vk,sk}`) in our [demo folder](https://github.com/input-output-hk/hydra/tree/master/demo). These demo keys should not be used in production.



### Contestation Period

The contestation period is a critical protocol parameter, defined in seconds, for example:

```
hydra-node --contestation-period 120s
```

Contestation period is used in:

- Constructing the upper validity bound for `Close` and `Contest` transactions,
- Computing the contestation deadline, which defines the lower validity
  bound for `FanOut` transaction.

The default contestation period is _60 seconds_, but it should be tailored to the network conditions, as different networks have varying slot lengths and block production rates.

:::info Consistent contestation period

All parties in the Hydra head protocol must configure the same `--contestation-period` value. Otherwise, the `Init` transaction will be ignored, preventing any participant from stalling or causing a denial-of-service (DoS) attack by setting an unreasonably large contestation period.

:::

:::warning Invalid Close and Contest Transactions

Depending on the contestation period value and the network conditions,
`Close` and `Contest` transactions could become invalid and be
silently rejected by the `cardano-node` to which they have been
submitted. This can happen, for example, if:
* The network is congested, with lot of transactions waiting to be
  included in a block,
* The node's connectivity to the network drops and the transaction is
  not propagated to block producers fast enough.

Currently, the `hydra-node` does not handle this situation. Each client application should implement a retry mechanism based on the expected time for transaction inclusion.

:::

### Reference Scripts

The `hydra-node` uses reference scripts to reduce transaction sizes driving the Head's lifecycle. Specify the `--hydra-scripts-tx-id` to reference the correct scripts. The `hydra-node` will verify the availability of these scripts on-chain.

Check the scripts against which a hydra-node was compiled using:

```shell
hydra-node --script-info
```

For public [(test) networks](https://book.world.dev.cardano.org/environments.html), we publish Hydra scripts with each new release, listing transaction IDs in the [release notes](https://github.com/input-output-hk/hydra/releases) and in [`networks.json`](https://github.com/input-output-hk/hydra/blob/master/networks.json).

To publish scripts yourself, use the `publish-scripts` command:

```shell
hydra-node publish-scripts \
  --testnet-magic 42 \
  --node-socket /path/to/node.socket \
  --cardano-signing-key cardano.sk
```

This command outputs a transaction ID upon success. The provided key should hold sufficient funds (> 50 ADA) to create multiple **UNSPENDABLE** UTXO entries on-chain, each carrying a script referenced by the Hydra node.

### Ledger Parameters

At the core of a Hydra head is a ledger. Currently, Hydra is integrated with Cardano and assumes a similar ledger configuration to Layer 1. This translates as a command-line option `--ledger-protocol-parameters`. This defines the updatable protocol parameters such as fees or transaction sizes. These parameters follow the same format as the output of `cardano-cli query protocol-parameters`.

We provide existing files in [hydra-cluster/config](https://github.com/input-output-hk/hydra/blob/master/hydra-cluster/config) which can be used as basis. In particular, the protocol parameters nullify costs inside a head. Apart from that, they are the direct copy the current mainnet parameters. An interesting point about the Hydra's ledger is that, while it re-uses the same rules and code as the layer 1 (isomorphic), some parameters can be altered.For example, fees can be adjusted, but not parameters controlling maximum value sizes or minimum ADA values, as altering these could make a head unclosable.
 A good rule thumb is that anything that applies strictly to transactions (fees, execution units, max tx size...) is safe to change. But anything that could be reflected in the UTXO is not.

:::info About Protocol Parameters
Many protocol parameters are irrelevant in the Hydra context (e.g., there is no treasury or stake pool within a head). Parameters related to reward incentives or delegation rules are therefore unused.
:::

### Fuel vs. Funds

Transactions driving the Head lifecycle (Init, Abort, Close, etc.) must be submitted to Layer 1 and hence incur costs. Any UTXO owned by the `--cardano-signing-key` provided to the `--hydra-node` can be used to pay fees or serve as collateral for these transactions. We refer to this as **fuel**.

Consequently, sending some ADA to the address of this "internal wallet" is required. To get the address for the cardano keys as generated above, one can use for example the cardano-cli:

```shell
cardano-cli address build --verification-key-file cardano.vk --mainnet
# addr1v92l229athdj05l20ggnqz24p4ltlj55e7n4xplt2mxw8tqsehqnt
```

<!-- TODO: this part below feels ab it odd here, rather move to API or how-to? -->

While the `hydra-node` needs to pay fees for protocol transactions, any wallet can be used to commit **funds** into an `initializing` Hydra head. The `hydra-node` provides an HTTP endpoint at `/commit`, allowing you to specify either:
 - A set of `UTXO` outputs to commit (belonging to public keys), or
 - A _blueprint_ transaction along with the `UTXO` that resolves it.

This endpoint returns a commit transaction, which is balanced, and all fees are paid by the `hydra-node`. The integrated wallet must sign and submit this transaction to the Cardano network. See the [API documentation](pathname:///api-reference/#operation-publish-/commit) for details.

If using your own UTxO to commit to a head, send the appropriate JSON representation of the said UTXO to the `/commit` API endpoint. 
Using a _blueprint_ transaction with `/commit` offers flexibility, as `hydra-node` adds necessary commit transaction data without removing additional information specified in the blueprint transaction (e.g., reference inputs, redeemers, validity ranges).


> Note: Outputs of a blueprint transaction are not considered—only inputs are used to commit funds to the head. The `hydra-node` will also **ignore** any minting or burning specified in the blueprint transaction.

For more details, refer to the [How To](./how-to/commit-blueprint) guide on committing to a head using a blueprint transaction.

## Connect to Cardano

The `hydra-node` must be connected to the Cardano network, unless running in [offline mode](./configuration.md#offline-mode).

A direct connection to a [`cardano-node`](https://github.com/input-output-hk/cardano-node/) is a pre-requisite. Please refer to existing documentation on starting a node, e.g. on [developers.cardano.org](https://developers.cardano.org/docs/get-started/running-cardano) or [use mithril](https://mithril.network/doc/manual/getting-started/bootstrap-cardano-node) to bootstrap the local node.

Specify how to connect to the local cardano-node, use `--node-socket` and `--testnet-magic`:

```shell
hydra-node \
  --testnet-magic 42 \
  --node-socket devnet/node.socket \
```

:::info
The `hydra-node` is compatible with the Cardano `mainnet` network, and can consequently operate using **real funds**. Please be sure to read the [known issues](/docs/known-issues) to fully understand the limitations and consequences of running Hydra nodes on mainnet. To choose `mainnet`, use `--mainnet` instead of `--testnet-magic`. 
:::

Using the direct node connection, the `hydra-node` does synchronize the chain and observes Hydra protocol transactions. On first startup, it will start observing from the chain's tip. Once a Hydra head has been observed, the point of the last known state change is used automatically.

You can manually set the intersection point using `--start-chain-from <slot>.<hash>` which specifies a `slot` and block header `hash`. For example:

```shell
hydra-node \
  --testnet-magic 2 \
  --node-socket preview/node.socket \
  --start-chain-from 49533501.e364500a42220ea47314215679b7e42e9bbb81fa69d1366fe738d8aef900f7ee
```

To synchronize from the genesis block, use `--start-chain-from 0`.

:::info
If the `hydra-node` already tracks a head in its `state` and `--start-chain-from` is given, the **newer** point is used.
:::

## Offline mode

Hydra supports an offline mode, which allows for disabling the Layer 1 interface (that is, the underlying Cardano blockchain which Hydra heads use to seed funds and ultimately funds are withdrawn to). Disabling Layer 1 interactions allows use-cases which would otherwise require running and configuring an entire Layer 1 private devnet. For example, the offline mode can be used to quickly validate a series of transactions against a UTXO, without having to spin up an entire Layer 1 Cardano node.

In this offline mode, only the Layer 2 ledger is run, along with the Hydra API and persistence, to support interacting with the offline Hydra. Therefore, ledger genesis parameters that normally influence things like time-based transaction validation, may be set to defaults that aren't reflective of mainnet. To set this, set --ledger-protocol-parameters to a non-zero file, as described [here](https://hydra.family/head-protocol/unstable/docs/configuration/#ledger-parameters).
Depending on your use case, you can [configure your node's event source and sinks](./how-to/event-sinks-and-sources.md) to better suite your needs.

To initialize the Layer 2 ledger's UTXO state, offline mode takes an obligatory --initial-utxo parameter, which points to a JSON encoded UTXO file. This UTXO is independent of Event Source loaded events, and the latter are validated against this UTXO. The UTXO follows the following schema `{ txout : {address, value : {asset : quantity}, datum, datumhash, inlinedatum, referenceScript }`

An example UTXO:
```json
{"1541287c2598ffc682742c961a96343ac64e9b9030e6b03a476bb18c8c50134d#0":{"address":"addr_test1vqg9ywrpx6e50uam03nlu0ewunh3yrscxmjayurmkp52lfskgkq5k","datum":null,"datumhash":null,"inlineDatum":null,"referenceScript":null,"value":{"lovelace":100000000}},"39786f186d94d8dd0b4fcf05d1458b18cd5fd8c6823364612f4a3c11b77e7cc7#0":{"address":"addr_test1vru2drx33ev6dt8gfq245r5k0tmy7ngqe79va69de9dxkrg09c7d3","datum":null,"datumhash":null,"inlineDatum":null,"referenceScript":null,"value":{"lovelace":100000000}}}```
