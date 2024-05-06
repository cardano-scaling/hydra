# Configuration

Running a Hydra head means running a Hydra node connected to some other Hydra nodes and connected to a Cardano node.

A working [cardano-node](https://github.com/input-output-hk/cardano-node/) is therefore a pre-requisite for running a Hydra head. In this guide, we won't go over details about running a Cardano node and we invite you to look for existing documentation on the matter if need be.

:::tip cardano-node & cardano-cli
We recommend using containers and the [official Docker image](https://hub.docker.com/r/inputoutput/cardano-node) for running a Cardano node.

This image contains both `cardano-node` and `cardano-cli`. The latter is handy to run various commands, for example to create addresses and to generate keys.
:::

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

```mdx-code-block
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

Hydra makes use of reference scripts to reduce the size of transactions driving the Head's lifecycle. In principle, reference scripts will be published with each release and the corresponding transaction id will be advertised in the release notes. However, if you do want to play around with this and provide alternative versions, you can do so by first publishing the scripts yourself via the `publish-scripts` command:

```mdx-code-block
<TerminalWindow>
hydra-node publish-scripts --testnet-magic 42 --node-socket /path/to/node.socket --cardano-signing-key cardano.sk
</TerminalWindow>
```

On success, this commands outputs a transaction id ready to be used. The provided key is expected to hold funds (> 50 ADA), and will be used to create multiple **UNSPENDABLE** UTxO entries on-chain, each carrying a script that can be later referenced by the Hydra node.

### Ledger Parameters

At the core of a Hydra head, there's a ledger. At the moment, Hydra is wired only to Cardano and assumes a ledger configuration similar to the one used on the layer 1. This translates as a command-line option `--ledger-protocol-parameters`. This defines the updatable protocol parameters such as fees or transaction sizes. They use the same format as the one used by the cardano-cli (e.g. `cardano-cli query protocol-parameters`'s output).

We provide existing files in [hydra-cluster/config](https://github.com/input-output-hk/hydra/blob/master/hydra-cluster/config) which can be used as basis. In particular, the protocol parameters are defined to nullify costs inside a head. Apart from that, they are the direct copy the current mainnet parameters. An interesting point about the Hydra's ledger is that, while it re-uses the same rules and code as the layer 1 (a.k.a. isomorphic), parameters may also be altered to slightly differ from the layer 1. This is the case for fees, but could also be done for script maximum execution budget for instance. However, not all parameters are safe to alter! Changing parameters that control the maximum size of a value (carrying native assets), or the minimum Ada value for a UTxO may render a head "unclosable"! A good rule thumb is that anything that applies strictly to transactions (fees, execution units, max tx size...) is safe to change. But anything that could be reflected in the UTxO is not.

:::info About Protocol Parameters
Note that many of protocol-parameters are actually irrelevant in the context of Hydra (for example, there's no treasury or stake pool inside a head; consequently, parameters configuring the reward incentive or delegation rules are pointless and unused).
:::

### Fuel

Finally, one last bit necessary to get Hydra nodes up and running is to add funds to their "internal wallet". All the transactions driving the Head lifecycle (Init, Abort, Close, ...) need to be submitted to the layer 1, and hence they cost money!

For that, any funds owned by the `--cardano-signing-key` given to the `--hydra-node` will be considered spendable to pay fees or use as collateral for these Hydra protocol transactions. Consequently, sending some ADA-only funds to the address of this "internal wallet" is required. To get the address for the cardano keys as generated above, one can use for example the cardano-cli:

<TerminalWindow>

```sh
cardano-cli address build --verification-key-file cardano.vk --mainnet
# addr1v92l229athdj05l20ggnqz24p4ltlj55e7n4xplt2mxw8tqsehqnt
```

</TerminalWindow>

## External commits

While the `hydra-node` holds funds to fuel protocol transactions, any wallet can be used to commit funds into an `initializing` Hydra head. The `hydra-node` provides an HTTP endpoint at `/commit`, which allows to specify either:
 - a `UTxO` set of outputs to commit (belonging to public keys) or
 - a _blueprint_ transaction together with the `UTxO` which resolves it.

and eventually returns a commit transaction.

This transaction is already balanced and all fees are paid by the funds held by the `hydra-node`. Hence, an integrated wallet would need to sign this transaction and submit it to the Cardano network. See the [api documentation](pathname:///api-reference/#operation-publish-/commit) for details.

If the user wants to use some `UTxO` they own and commit it to a `Head` then they need to send the appropriate JSON representation of said `UTxO` to the `/commit` API endpoint.

Using a _blueprint_ transaction with `/commit` allows for more flexibility since `hydra-node` only adds needed commit transaction data without removing any additional information specified in the _blueprint_ transaction. For example, any present reference inputs, redeemers or validity ranges will be kept.

> Note: It is important to note that any **outputs** of a blueprint transaction will not be considered, only inputs are used to commit funds to the `Head`. `hydra-node` will also **ignore** any minting or burning specified in the blueprint transaction.

You can take a look at the small example on how to commit to a `Head` using blueprint transaction [here](/docs/blueprint_transaction.md)

## Generating transactions for the WebSocket API

To perform a transaction within an initialized Head via the WebSocket API of Hydra Node, use the commands below and send the output to the node:

```bash title="Transaction building"
cardano-cli transaction build-raw \
  --babbage-era \
  --tx-in 09d34606abdcd0b10ebc89307cbfa0b469f9144194137b45b7a04b273961add8#687 \
  --tx-out addr1vx8apm8x8rla2w6tk7dxnwrlxkmeuera4x4tw5j695xhxeq4wawpz+7620669 \
  --fee 0 \
  --out-file tx.json

cardano-cli transaction sign \
  --tx-body-file tx.json \
  --signing-key-file cardano.sk \
  --out-file signed-tx.json

echo "{ \"tag\": \"NewTx\", \"transaction\": $(cat signed-tx.json | jq ".cborHex") }"
{ "tag": "NewTx", "transaction": "84a3008182582009d34606abdcd0b10ebc89307cbfa0b469f9144194137b45b7a04b273961add81902af0181a200581d618fd0ece638ffd53b4bb79a69b87f35b79e647da9aab7525a2d0d7364011a0074483d0200a10081825820c736d40ee64c031851af26007c00a3b6fcbebccfd333a8ee6f14983f9be5331c58404bae506f5235778ec65eca6fdfcf6ec61ab93420b91e0b71ca82d437904f860e999372cf00252246ca77012e19c344b3af60df9f853af53fc86835f95a119609f5f6" }
```

The `--tx-in` value is a UTxO obtained from the `GET /snapshot/utxo` request. For example:

```json title="Example response of GET /snapshot/utxo"
{
  "09d34606abdcd0b10ebc89307cbfa0b469f9144194137b45b7a04b273961add8#687": {
    "address": "addr1w9htvds89a78ex2uls5y969ttry9s3k9etww0staxzndwlgmzuul5",
    "value": {
      "lovelace": 7620669
    }
  }
}
```

## Example Setup

### Google Cloud w/ Terraform

We provide sample node configurations that will help you get started hosting a Hydra node on virtual machines in the Cloud in the [`sample-node-config/` directory](https://github.com/input-output-hk/hydra/tree/master/sample-node-config/gcp/). In particular, this setup contains a [docker-compose.yaml](https://github.com/input-output-hk/hydra/blob/master/sample-node-config/gcp/docker-compose.yaml) specification which gives a good template for configuring cardano-node + hydra-node services. It also offers various useful scripts to setup your cluster.

> Note: This setup is meant to configure your cluster for devnet network. If you want to run the node on mainnet check out _Running on Mainnet_ paragraph.

## Running on Mainnet

Hydra node is compatible with the mainnet network. To choose this network you need to specify `--mainnet` flag for the network id in the hydra-node arguments. We publish the hydra scripts on each new release and you can find them on the [release page](https://github.com/input-output-hk/hydra/releases) (look for section _Hydra Scripts_).

Please be sure to read the [relevant section](/docs/known-issues) section to fully understand the limitations and consequences of running Hydra nodes on mainnet.
