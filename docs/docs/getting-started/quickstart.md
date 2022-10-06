---
sidebar_position: 3
---

# QuickStart

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```

> Your first steps with a `hydra-node`.

Running a Hydra head means running a Hydra node connected to some other Hydra nodes and connected to a Cardano node. A working [cardano-node](https://github.com/input-output-hk/cardano-node/) is therefore a pre-requisite for running a Hydra head. In this guide, we won't go over details about running a Cardano node and we invite you to look for existing documentation on the matter if need be.

:::tip cardano-node & cardano-cli
We recommend using containers and the [official Docker image](https://hub.docker.com/r/inputoutput/cardano-node) for running a Cardano node.

This image contains both `cardano-node` and `cardano-cli`. The latter is handy to run various commands, for example to create addresses and to generate keys.
:::

## Hydra-node options...

So far, the `hydra-node` command-line provide a single command only for starting a node. The entire configuration is provided through command-line options and is fully static. Options are used to configured various elements, and can be summarized as follows (note however that reference documentation for those options is available under the `--help` flag):

Options                                                 | Description
---                                                     | ---
`--node-id`                                             | The Hydra node identifier, serving as identifier within the Head network.
`--peer`                                                | The Hydra network peers address. Must be provided multiple time, one for each peer.
`--host` <br/> `--port`                                 | This Hydra node host and port, to which peers from the Hydra network can connect to.
`--node-socket`                                         | The Cardano node's IPC socket filepath, used for inter-process communication with the node.
`--ledger-genesis` <br/> `--ledger-protocol-parameters` | The Hydra ledger rules and parameters for the head.
`--hydra-scripts-tx-id`                                 | A transaction id referencing Hydra's Plutus scripts. See release notes.
`--hydra-signing-key` <br/> `--cardano-signing-key` <br/> `--hydra-verification-key` <br/> `--cardano-verification-key` | The Cardano and Hydra credentials for peers and the node itself. Those options may also be provided multiple times depending on the number of peers.

Also, optionally:

Options                         | Description
---                             | ---
`--api-host` <br/> `--api-port` | The Hydra API host and port, to interact with the [WebSocket API](/api-reference).
`--monitoring-port`             | The port this node listens on for monitoring and metrics via Prometheus. If left empty, monitoring server is not started.

:::info  Dynamic Configuration

We realise that the command-line in its current form isn't as user-friendly as it could, and is somewhat cumbersome to use for setting up large clusters.

There are however plans to make the configuration more user-friendly and configurable dynamically; see [#240](https://github.com/input-output-hk/hydra-poc/issues/240) & [ADR-15](/adr/15)
:::

## ...and Where to Find Them

### Cardano Keys

The previous section describes the various options and elements needed to setup a Hydra node. In this section, we'll show how to obtain some of those elements. First, let's start with the Cardano keys (`--cardano-signing-key` and `--cardano-verification-key`).

In a head, every participant is authenticated by two sets of keys, one key pair is a plain Ed25519 public/private key pair quite common on Cardano already. Such a key pair can be generated using the `cardano-cli` as follows:

```mdx-code-block
<TerminalWindow>
cardano-cli address key-gen --verification-key-file cardano.vk --signing-key-file cardano.sk
</TerminalWindow>
```

From there, each participant is expected to share their verification key with other participants. To start a node, one will need its **own signing key** and **other participants' verification key**. Those keys are currently used to authenticate on-chain transactions which drives the execution of the Hydra protocol. They prevent unsolicited actors to fiddle with the head life-cycle (for instance, someone external to the head could otherwise _abort_ an initialised head). While this wouldn't put head participants' funds at risk, it is still an annoyance that one wants to prevent.

### Hydra keys

The second set of keys are the so-called Hydra keys, which are used for multi-signing snapshots within a Head. While in the long-run, those keys will be key pairs used within MuSig2 aggregated multi-signature scheme. At present however, the aggregated multisig cryptography is [yet to be implemented](https://github.com/input-output-hk/hydra-poc/issues/193) and the Hydra nodes are a naiive, but secure multi-signature scheme based on Ed25519 keys.

These are similar to cardano keys but are used only in the layer 2. We provide demo key pairs as `alice.{vk,sk}`, `bob.{vk,sk}` and `carol.{vk,sk}` in our [demo folder](https://github.com/input-output-hk/hydra-poc/tree/master/demo).

Alternatively, unique keys can be generated using `hydra-tools`, a command-line utility that's provided as part of Hydra:

```mdx-code-block
<TerminalWindow>
hydra-tools gen-hydra-key --output-file my-key
</TerminalWindow>
```

This will create two files, `my-key.sk` and `my-key.vk` containing Hydra keys suitable for use inside a head.

### Reference Scripts

Hydra makes use of reference scripts to reduce the size of transactions driving the Head's lifecycle. In principle, reference scripts will be published with each release and the corresponding transaction id will be advertised in the release notes. However, if you do want to play around with this and provide alternative versions, you can do so by first publishing the scripts yourself via the `publish-scripts` command:

```mdx-code-block
<TerminalWindow>
hydra-node publish-scripts --network-id 42 --node-socket /path/to/node.socket --cardano-signing-key cardano.sk
</TerminalWindow>
```

On success, this commands outputs a transaction id ready to be used. The provided key is expected to hold funds (> 50 ADA), and will be used to create multiple **UNSPENDABLE** UTxO entries on-chain, each carrying a script that can be later referenced by the Hydra node.

### Ledger Parameters

At the core of a Hydra head, there's a ledger. At the moment, Hydra is wired only to Cardano and assumes a ledger configuration similar to the one used on the layer 1. This translates as two command-line options `--ledger-genesis` and `--ledger-protocol-parameters`. The former defines the (Shelley!) genesis rules and more specifically, the **global**, non-updatable protocol parameters required by the ledger. The latter defines the updatable protocol parameters such as fees or transaction sizes. They use the same format as the one used by the cardano-cli (e.g. `cardano-cli query protocol-parameters`'s output).

We provide existing files in [hydra-cluster/config](https://github.com/input-output-hk/hydra-poc/blob/master/hydra-cluster/config) which can be used as basis. In particular, the protocol parameters are defined to nullify costs inside a head. Apart from that, they are the direct copy the current mainnet parameters. An interesting point about the Hydra's ledger is that, while it re-uses the same rules and code as the layer 1 (a.k.a. isomorphic), parameters may also be altered to slightly differ from the layer 1. This is the case for fees, but could also be done for script maximum execution budget for instance. However, not all parameters are safe to alter! Changing parameters that control the maximum size of a value (carrying native assets), or the minimum Ada value for a UTxO may render a head "unclosable"! A good rule thumb is that anything that applies strictly to transactions (fees, execution units, max tx size...) is safe to change. But anything that could be reflected in the UTxO is not.

:::info About Protocol Parameters
Note that there's a bit of overlap between the two files since most protocol parameters are first and foremost genesis parameters. Moreover, many of those parameters are actually irrelevant in the context of Hydra (for example, there's no treasury or stake pool inside a head; consequently, parameters configuring the reward incentive or delegation rules are pointless and unused).
:::

### Fuel

Finally, one last bit necessary to get Hydra nodes all working regards their _internal wallet_. Indeed, Hydra-nodes currently come with a rudimentary wallet which they use for fueling transactions driving the Head lifecycle (Init, Commit, Close, Fanout...). Since those transactions happen on the layer 1, they cost money!

For now, this is managed internally by the Hydra's wallet, but it needs some help. The Cardano keys provided to the node are expected to hold funds. More specifically, at least one UTxO entry, marked with a specific datum hash:

```bash title="Fuel datum hash"
a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3
```

Conveniently (at least, as much as it can possibly be right now), we provide a [create-marker-utxo.sh](https://github.com/input-output-hk/hydra-poc/blob/master/sample-node-config/gcp/scripts/create-marker-utxo.sh) script that uses the cardano-cli to convert a normal UTxO into a marked fuel UTxO. Note that the marker is necessary because, the Cardano keys are expected to hold funds necessary for commits as well, however unmarked.

For easy scripting purpose, `hydra-tools` provide a dedicated command to output the current marker datum hash:

```mdx-code-block
<TerminalWindow>
hydra-tools marker-hash
> "a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3"
</TerminalWindow>
```

:::info About commits
In the long-run, we'll [move commits outside of the Hydra node](https://github.com/input-output-hk/hydra-poc/issues/215) to be done by external wallets (likely through wallets following the [CIP-0030](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0030) standard).
:::

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

The `--tx-in` value is a UTxO obtained from the reply to the `{ "tag": "GetUTxO" }` message.
E.g.:

```json title="GetUTxOResponse"
{
  "tag": "GetUTxOResponse",
  "utxo": {
    "09d34606abdcd0b10ebc89307cbfa0b469f9144194137b45b7a04b273961add8#687": {
      "address": "addr1w9htvds89a78ex2uls5y969ttry9s3k9etww0staxzndwlgmzuul5",
      "value": {
        "lovelace": 7620669
      }
    }
  }
}
```


## Example Setup

### Google Cloud w/ Terraform

We provide sample node configurations that will help you get started hosting a Hydra node on virtual machines in the Cloud in the [`sample-node-config/` directory](https://github.com/input-output-hk/hydra-poc/tree/master/sample-node-config/gcp/). In particular, this setup contains a [docker-compose.yaml](https://github.com/input-output-hk/hydra-poc/blob/master/sample-node-config/gcp/docker-compose.yaml) specification which gives a good template for configuring cardano-node + hydra-node services. It also offers various useful scripts to setup your cluster.
