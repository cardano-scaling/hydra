# Configuration

Running a Hydra head means running a Hydra node connected to some other Hydra nodes and connected to a Cardano node.

The entire configuration of the `hydra-node` is provided through command-line options. Options are used to configure various elements of the network, API, chain connection and used ledger. You can use `--help` to get a description of all options:

```shell
hydra-node --help
```

Below we document selected aspects of the configuration. See the [tutorial](./tutorial) for an end-to-end guide or individual _How to_ articles for more details.

### Cardano Keys

In a head, every participant is authenticated by two sets of keys. The first are used to identify a participant on the Cardano L1 and hold ADA to pay fees. Every `hydra-node` needs to be given a `--cardano-signing-key` and for each other participant of the head, we need to provide their `--cardano-verification-key`.

Such a key pair can be generated using the `cardano-cli` as follows:

```shell
cardano-cli address key-gen \
  --verification-key-file cardano.vk \
  --signing-key-file cardano.sk
```

These keys are currently used to authenticate on-chain transactions which drives the execution of the Hydra protocol. They prevent unsolicited actors to fiddle with the head life-cycle (for instance, someone external to the head could otherwise _abort_ an initialised head). While this wouldn't put head participants' funds at risk, it is still an annoyance that one wants to prevent.

### Hydra keys

The second set of keys are the so-called Hydra keys, which are used for multi-signing snapshots within a Head. While in the long-run, those keys may be used in an aggregated multi-signature scheme. At present however, Hydra keys are also Ed25519 keys.

These are similar to cardano keys but are only on the layer 2. New keys can be generated using `hydra-node`:

```
hydra-node gen-hydra-key --output-file my-key
```

This will create two files, `my-key.sk` and `my-key.vk` containing Hydra keys suitable for use inside a head.

Alternatively, we provide demo key pairs as `alice.{vk,sk}`, `bob.{vk,sk}` and `carol.{vk,sk}` in our [demo folder](https://github.com/input-output-hk/hydra/tree/master/demo). These should obviously not be used in production.


### Contestation Period

The contestation period is a protocol parameter and given as a number of seconds, for example:

```
hydra-node --contestation-period 120s
```

Contestation period is used in:

- Constructing the upper validity bound for `Close` and `Contest` transactions,
- Computing the contestation deadline and therefore the lower validity
  bound for `FanOut` transaction.

This parameter has a default value of _60 seconds_, which should be
enough under normal circumstances, but it needs to be tailored to the
condition of network we are running on since slot lengths and block
production rates are different on different networks.

:::info Consistent contestation period

All parties in the hydra head protocol need to configure the same
value for `--contestation-period` otherwise the `Init` transaction
will be ignored.  This prevents certain party from stalling/DoS-ing
the head by setting contestation period to unreasonably large values.

:::

:::warning Invalid Close and Contest Transactions

Depending on the contestation period value and the network conditions,
`Close` and `Contest` transactions could become invalid and be
silently rejected by the cardano-node to which they have been
submitted. This can happen, for example, if:
* The network is congested, with lot of transactions waiting to be
  included in a block,
* The node's connectivity to the network drops and the transaction is
  not propagated to block producers fast enough.

The hydra-node itself does not currently handle this situation and
therefore each client application needs to put in place some retry
mechanism depending on the time it should "normally" take to have the
transaction.

:::

### Reference Scripts

The `hydra-node` makes use of reference scripts to reduce the size of transactions driving the Head's lifecycle. To reference the right scripts, the `--hydra-scripts-tx-id` needs to be provided. When provided, the `hydra-node` checks whether the right scripts are available at this transaction on-chain.

You can also check against which scripts a hydra-node was compiled using:

```shell
hydra-node --script-info
```

For public [(test) networks](https://book.world.dev.cardano.org/environments.html), we publish the hydra scripts on each new release and advertise transaction ids on the [release notes](https://github.com/input-output-hk/hydra/releases) and on the repository in [`networks.json`](https://github.com/input-output-hk/hydra/blob/master/networks.json).

If you want to publish the scripts yourself, you can do so using the `publish-scripts` command:

```shell
hydra-node publish-scripts \
  --testnet-magic 42 \
  --node-socket /path/to/node.socket \
  --cardano-signing-key cardano.sk
```

On success, this commands outputs a transaction id ready to be used. The provided key is expected to hold funds (> 50 ADA), and will be used to create multiple **UNSPENDABLE** UTxO entries on-chain, each carrying a script that can be later referenced by the Hydra node.

### Ledger Parameters

At the core of a Hydra head, there's a ledger. At the moment, Hydra is wired only to Cardano and assumes a ledger configuration similar to the one used on the layer 1. This translates as a command-line option `--ledger-protocol-parameters`. This defines the updatable protocol parameters such as fees or transaction sizes. They use the same format as the one used by the cardano-cli (e.g. `cardano-cli query protocol-parameters`'s output).

We provide existing files in [hydra-cluster/config](https://github.com/input-output-hk/hydra/blob/master/hydra-cluster/config) which can be used as basis. In particular, the protocol parameters are defined to nullify costs inside a head. Apart from that, they are the direct copy the current mainnet parameters. An interesting point about the Hydra's ledger is that, while it re-uses the same rules and code as the layer 1 (a.k.a. isomorphic), parameters may also be altered to slightly differ from the layer 1. This is the case for fees, but could also be done for script maximum execution budget for instance. However, not all parameters are safe to alter! Changing parameters that control the maximum size of a value (carrying native assets), or the minimum Ada value for a UTxO may render a head "unclosable"! A good rule thumb is that anything that applies strictly to transactions (fees, execution units, max tx size...) is safe to change. But anything that could be reflected in the UTxO is not.

:::info About Protocol Parameters
Note that many of protocol-parameters are actually irrelevant in the context of Hydra (for example, there's no treasury or stake pool inside a head; consequently, parameters configuring the reward incentive or delegation rules are pointless and unused).
:::

### Fuel vs. Funds

All the transactions driving the Head lifecycle (Init, Abort, Close, ...) need to be submitted to the layer 1, and hence they cost money! For that, any UTxO owned by the `--cardano-signing-key` given to the `--hydra-node` will be considered spendable to pay fees or use as collateral for these Hydra protocol transactions. We sometimes this **fuel**.

Consequently, sending some ADA to the address of this "internal wallet" is required. To get the address for the cardano keys as generated above, one can use for example the cardano-cli:

```shell
cardano-cli address build --verification-key-file cardano.vk --mainnet
# addr1v92l229athdj05l20ggnqz24p4ltlj55e7n4xplt2mxw8tqsehqnt
```

<!-- TODO: this part below feels ab it odd here, rather move to API or how-to? -->

While the `hydra-node` needs to pay fees for protocol transactions, any wallet can be used to commit **funds** into an `initializing` Hydra head. The `hydra-node` provides an HTTP endpoint at `/commit`, which allows to specify either:
 - a `UTxO` set of outputs to commit (belonging to public keys) or
 - a _blueprint_ transaction together with the `UTxO` which resolves it.

and eventually returns a commit transaction.

This transaction is already balanced and all fees are paid by the funds held by the `hydra-node`. Hence, an integrated wallet would need to sign this transaction and submit it to the Cardano network. See the [api documentation](pathname:///api-reference/#operation-publish-/commit) for details.

If the user wants to use some `UTxO` they own and commit it to a `Head` then they need to send the appropriate JSON representation of said `UTxO` to the `/commit` API endpoint.

Using a _blueprint_ transaction with `/commit` allows for more flexibility since `hydra-node` only adds needed commit transaction data without removing any additional information specified in the _blueprint_ transaction. For example, any present reference inputs, redeemers or validity ranges will be kept.

> Note: It is important to note that any **outputs** of a blueprint transaction will not be considered, only inputs are used to commit funds to the `Head`. `hydra-node` will also **ignore** any minting or burning specified in the blueprint transaction.

For more details refere to the [how to](./how-to/commit-blueprint) about committing to a `Head` using blueprint transaction.

## Connect to Cardano

The `hydra-node` needs to be connected to the cardano network (unless it runs in [offline mode](./configuration.md#offline-mode)). 

Currently a direct connection to a [`cardano-node`](https://github.com/input-output-hk/cardano-node/) is a pre-requisite and please refer to existing documentation on starting a node, e.g. on [developers.cardano.org](https://developers.cardano.org/docs/get-started/running-cardano) or [use mithril](https://mithril.network/doc/manual/getting-started/bootstrap-cardano-node) to bootstrap the local node.

To specify how to connect to the local cardano-node, use `--node-socket` and `--testnet-magic`:

```shell
hydra-node \
  --testnet-magic 42 \
  --node-socket devnet/node.socket \
```

The `hydra-node` is compatible with the Cardano `mainnet` network, and can consequently operate using **real funds**. Please be sure to read the [known issues](/docs/known-issues) to fully understand the limitations and consequences of running Hydra nodes on mainnet. To choose `mainnet`, use `--mainnet` instead of `--testnet-magic`. 

## Offline mode

Hydra supports an offline mode, which allows for disabling the Layer 1 interface (that is, the underlying Cardano blockchain which Hydra heads use to seed funds and ultimately funds are withdrawn to). Disabling Layer 1 interactions allows use-cases which would otherwise require running and configuring an entire Layer 1 private devnet. For example, the offline mode can be used to quickly validate a series of transactions against a UTxO, without having to spin up an entire Layer 1 Cardano node.

In this offline mode, only the Layer 2 ledger is run, along with the Hydra API and persistence, to support interacting with the offline Hydra. Therefore, ledger genesis parameters that normally influence things like time-based transaction validation, may be set to defaults that aren't reflective of mainnet. To set this, set --ledger-protocol-parameters to a non-zero file, as described [here](https://hydra.family/head-protocol/unstable/docs/configuration/#ledger-parameters).
Depending on your use case, you can [configure your node's event source and sinks](./how-to/event-sinks-and-sources.md) to better suite your needs.

To initialize the Layer 2 ledger's UTXO state, offline mode takes an obligatory --initial-utxo parameter, which points to a JSON encoded UTXO file. This UTXO is independent of Event Source loaded events, and the latter are validated against this UTXO. The UTXO follows the following schema `{ txout : {address, value : {asset : quantity}, datum, datumhash, inlinedatum, referenceScript }`

An example UTXO:
```json
{"1541287c2598ffc682742c961a96343ac64e9b9030e6b03a476bb18c8c50134d#0":{"address":"addr_test1vqg9ywrpx6e50uam03nlu0ewunh3yrscxmjayurmkp52lfskgkq5k","datum":null,"datumhash":null,"inlineDatum":null,"referenceScript":null,"value":{"lovelace":100000000}},"39786f186d94d8dd0b4fcf05d1458b18cd5fd8c6823364612f4a3c11b77e7cc7#0":{"address":"addr_test1vru2drx33ev6dt8gfq245r5k0tmy7ngqe79va69de9dxkrg09c7d3","datum":null,"datumhash":null,"inlineDatum":null,"referenceScript":null,"value":{"lovelace":100000000}}}```
