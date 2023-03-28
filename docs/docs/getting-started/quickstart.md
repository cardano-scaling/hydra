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

The entire configuration of the `hydra-node` is provided through command-line options. Options are used to configure various elements of the network, API, chain connection and used ledger. You can use the `--help` option to get a description of all options:

```


Usage: hydra-node ([-q|--quiet] (-n|--node-id NODE-ID) [-h|--host IP]
                    [-p|--port PORT] [-P|--peer ARG] [--api-host IP]
                    [--api-port PORT] [--monitoring-port PORT]
                    [--hydra-signing-key FILE] [--hydra-verification-key FILE]
                    [--hydra-scripts-tx-id TXID] [--persistence-dir DIR]
                    [--mainnet | --testnet-magic NATURAL] [--node-socket FILE]
                    [--cardano-signing-key FILE]
                    [--cardano-verification-key FILE]
                    [--start-chain-from SLOT.HEADER_HASH]
                    [--contestation-period CONTESTATION-PERIOD]
                    [--ledger-genesis FILE]
                    [--ledger-protocol-parameters FILE] |
                    COMMAND) [--version] [--script-info]

  Starts a Hydra Node

Available options:
  -q,--quiet               Turns off logging.
  -n,--node-id NODE-ID     The Hydra node identifier used on the Hydra network.
                           It is important to have a unique identifier in order
                           to be able distinguish between connected peers.
  -h,--host IP             Listen address for incoming Hydra network
                           connections. (default: 127.0.0.1)
  -p,--port PORT           Listen port for incoming Hydra network connections.
                           (default: 5001)
  -P,--peer ARG            A peer address in the form <host>:<port>, where
                           <host> can be an IP address, or a host name. Can be
                           provided multiple times, once for each peer (current
                           maximum limit is 4 peers).
  --api-host IP            Listen address for incoming client API connections.
                           (default: 127.0.0.1)
  --api-port PORT          Listen port for incoming client API connections.
                           (default: 4001)
  --monitoring-port PORT   Listen port for monitoring and metrics via
                           prometheus. If left empty, monitoring server is not
                           started.
  --hydra-signing-key FILE Hydra signing key used by our hydra-node.
                           (default: "hydra.sk")
  --hydra-verification-key FILE
                           Hydra verification key of another party in the Head.
                           Can be provided multiple times, once for each
                           participant (current maximum limit is 4 ).
  --hydra-scripts-tx-id TXID
                           The transaction which is expected to have published
                           Hydra scripts as reference scripts in its outputs.
                           Note: All scripts need to be in the first 10 outputs.
                           See release notes for pre-published versions. You can
                           use the 'publish-scripts' sub-command to publish them
                           yourself.
  --persistence-dir DIR    The directory where the Hydra Head state is stored.Do
                           not edit these files manually!
  --mainnet                Use the mainnet magic id.
  --testnet-magic NATURAL  Network identifier for a testnet to connect to. We
                           only need to provide the magic number here. For
                           example: '2' is the 'preview' network. See
                           https://book.world.dev.cardano.org/environments.html
                           for available networks. (default: 42)
  --node-socket FILE       Filepath to local unix domain socket used to
                           communicate with the cardano node.
                           (default: "node.socket")
  --cardano-signing-key FILE
                           Cardano signing key of our hydra-node. This will be
                           used to 'fuel' and sign Hydra protocol transactions,
                           as well as commit UTxOs from. (default: "cardano.sk")
  --cardano-verification-key FILE
                           Cardano verification key of another party in the
                           Head. Can be provided multiple times, once for each
                           participant (current maximum limit is 4).
  --start-chain-from SLOT.HEADER_HASH
                           The id of the block we want to start observing the
                           chain from. If not given, uses the chain tip at
                           startup. Composed by the slot number, a separator
                           ('.') and the hash of the block header. For example:
                           52970883.d36a9936ae7a07f5f4bdc9ad0b23761cb7b14f35007e54947e27a1510f897f04.
  --contestation-period CONTESTATION-PERIOD
                           Contestation period for close transaction in seconds.
                           If this value is not in sync with other participants
                           hydra-node will ignore the initial tx. Additionally,
                           this value needs to make sense compared to the
                           current network we are running. (default: 60s)
  --ledger-genesis FILE    Path to a Shelley-compatible genesis JSON file used
                           for the Hydra ledger. You can use the corresponding
                           Cardano network's shelley genesis file from:
                           https://book.world.dev.cardano.org/environments.html
                           (default: "genesis-shelley.json")
  --ledger-protocol-parameters FILE
                           Path to protocol parameters used in the Hydra Head.
                           See manual how to configure this.
                           (default: "protocol-parameters.json")
  --version                Show version
  --script-info            Dump script info as JSON
  -h,--help                Show this help text

Available commands:
  publish-scripts          Publish Hydra's Plutus scripts on chain to be used
                           by the hydra-node as --hydra-script-tx-id.

                            ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
                            ┃              ⚠ WARNING ⚠              ┃
                            ┣═══════════════════════════════════════┫
                            ┃    This costs money. About 50 Ada.    ┃
                            ┃ Spent using the provided signing key. ┃
                            ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛


```

:::info Dynamic Configuration

We realise that the command-line in its current form isn't as user-friendly as it could, and is somewhat cumbersome to use for setting up large clusters.

There are however plans to make the configuration more user-friendly and configurable dynamically; see [#240](https://github.com/input-output-hk/hydra/issues/240) & [ADR-15](/adr/15)
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

The second set of keys are the so-called Hydra keys, which are used for multi-signing snapshots within a Head. While in the long-run, those keys will be key pairs used within MuSig2 aggregated multi-signature scheme. At present however, the aggregated multisig cryptography is [yet to be implemented](https://github.com/input-output-hk/hydra/issues/193) and the Hydra nodes are a naiive, but secure multi-signature scheme based on Ed25519 keys.

These are similar to cardano keys but are used only in the layer 2. We provide demo key pairs as `alice.{vk,sk}`, `bob.{vk,sk}` and `carol.{vk,sk}` in our [demo folder](https://github.com/input-output-hk/hydra/tree/master/demo).

Alternatively, unique keys can be generated using `hydra-tools`, a command-line utility that's provided as part of Hydra:

```mdx-code-block
<TerminalWindow>
hydra-tools gen-hydra-key --output-file my-key
</TerminalWindow>
```

This will create two files, `my-key.sk` and `my-key.vk` containing Hydra keys suitable for use inside a head.

### Contestation Period

Contestation period is the argument to the hydra node expressed as the number of seconds. This value needs to be in sync with the network we are running on since
slot lengths are different on different networks.

Contestation period is used in:

- Constructing the upper transaction bound for close transaction
- Contestation deadline.

:::info Note on contestation period
All parties in the hydra head protocol need to configure the same value for `--contestation-period` otherwise the `Init` transaction will be ignored.
This prevents certain party from stalling/DoS-ing the head by setting unreasonable big number for contestation period.
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

At the core of a Hydra head, there's a ledger. At the moment, Hydra is wired only to Cardano and assumes a ledger configuration similar to the one used on the layer 1. This translates as two command-line options `--ledger-genesis` and `--ledger-protocol-parameters`. The former defines the (Shelley!) genesis rules and more specifically, the **global**, non-updatable protocol parameters required by the ledger. The latter defines the updatable protocol parameters such as fees or transaction sizes. They use the same format as the one used by the cardano-cli (e.g. `cardano-cli query protocol-parameters`'s output).

We provide existing files in [hydra-cluster/config](https://github.com/input-output-hk/hydra/blob/master/hydra-cluster/config) which can be used as basis. In particular, the protocol parameters are defined to nullify costs inside a head. Apart from that, they are the direct copy the current mainnet parameters. An interesting point about the Hydra's ledger is that, while it re-uses the same rules and code as the layer 1 (a.k.a. isomorphic), parameters may also be altered to slightly differ from the layer 1. This is the case for fees, but could also be done for script maximum execution budget for instance. However, not all parameters are safe to alter! Changing parameters that control the maximum size of a value (carrying native assets), or the minimum Ada value for a UTxO may render a head "unclosable"! A good rule thumb is that anything that applies strictly to transactions (fees, execution units, max tx size...) is safe to change. But anything that could be reflected in the UTxO is not.

:::info About Protocol Parameters
Note that there's a bit of overlap between the two files since most protocol parameters are first and foremost genesis parameters. Moreover, many of those parameters are actually irrelevant in the context of Hydra (for example, there's no treasury or stake pool inside a head; consequently, parameters configuring the reward incentive or delegation rules are pointless and unused).
:::

### Fuel

Finally, one last bit necessary to get Hydra nodes all working regards their _internal wallet_. Indeed, Hydra-nodes currently come with a rudimentary wallet which they use for fueling transactions driving the Head lifecycle (Init, Commit, Close, Fanout...). Since those transactions happen on the layer 1, they cost money!

For now, this is managed internally by the Hydra's wallet, but it needs some help. The Cardano keys provided to the node are expected to hold funds. More specifically, at least one UTxO entry, marked with a specific datum hash:

```bash title="Fuel datum hash"
a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3
```

Conveniently (at least, as much as it can possibly be right now), we provide a [create-marker-utxo.sh](https://github.com/input-output-hk/hydra/blob/master/sample-node-config/gcp/scripts/create-marker-utxo.sh) script that uses the cardano-cli to convert a normal UTxO into a marked fuel UTxO. Note that the marker is necessary because, the Cardano keys are expected to hold funds necessary for commits as well, however unmarked. As a secondary note, this is meant to be used for local devnet, for mainnet usage you would need to adapt the script.

For easy scripting purpose, `hydra-tools` provide a dedicated command to output the current marker datum hash:

```mdx-code-block
<TerminalWindow>
hydra-tools marker-hash
> "a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3"
</TerminalWindow>
```

:::info About commits
In the long-run, we'll [move commits outside of the Hydra node](https://github.com/input-output-hk/hydra/issues/215) to be done by external wallets (likely through wallets following the [CIP-0030](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0030) standard).
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

We provide sample node configurations that will help you get started hosting a Hydra node on virtual machines in the Cloud in the [`sample-node-config/` directory](https://github.com/input-output-hk/hydra/tree/master/sample-node-config/gcp/). In particular, this setup contains a [docker-compose.yaml](https://github.com/input-output-hk/hydra/blob/master/sample-node-config/gcp/docker-compose.yaml) specification which gives a good template for configuring cardano-node + hydra-node services. It also offers various useful scripts to setup your cluster.

> Note: This setup is meant to configure your cluster for devnet network. If you want to run the node on mainnet check out _Running on Mainnet_ paragraph.

## Running on Mainnet

Hydra node is compatible with the mainnet network. To choose this network you need to specify `--mainnet` flag for the network id in the hydra-node arguments. On each release we are alredy pre-publishing the hydra scripts and you can find them on the [release page](https://github.com/input-output-hk/hydra/releases) (look for section _Hydra Scripts_).
You would need to match the hydra-node version with the appropriate scripts published for this release and make sure to choose the correct network.
Currently there is a hard-coded limit on mainnet network where you can only commit up to 100 ADA into the Hydra head which is meant to prevent the users from _shooting themselves in the foot_ until we have more experiments on the mainnet.
