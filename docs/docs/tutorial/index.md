---
sidebar_position: 2
---

# Tutorial

This tutorial will show you how to use `hydra-node` on the `preprod` Cardano
network to open a layer-two state channel between two actors using the Hydra
Head protocol. We will also use [Mithril](https://mithril.network) to bootstrap
our nodes for a speedy setup.

This setup is also known as the [Basic Hydra Head](/topologies/basic) topology
and we will be creating the "green" Hydra Head between `X` and `Y` as shown
below:

![](../../topologies/basic/basic-hydra-head.jpg)

The tutorial will be using the `docker` images of the software components
involved. There are usually other ways to acquire and run the Cardano, Mithril,
and Hydra nodes.

## What you will need

- [ ] Terminal access to a machine that can connect to and can be reached from the internet.
- [ ] Either
  - [ ] someone else following this tutorial as well to connect to (recommended), or
  - [ ] two such machines (or you can run it on one machine).
- [ ] 100 tADA in a wallet on `preprod` (per participant)
- [ ] Some tools installed
  - [ ] `curl`
  - [ ] [`websocat`](https://github.com/vi/websocat)
  - [ ] [`docker`](https://docs.docker.com/get-docker)
  - [ ] [`jq`](https://jqlang.github.io/jq/)

## Step 1: Connect to Cardano

The Hydra Head protocol a connection to the Cardano layer one network to verify
and post protocol transactions in a trustless way. Hence, the first step is to
set up a `cardano-node` on a public testnet. Using Mithril, we can skip
synchronizing the whole history and get started quickly.

We will be using the `mithril-client` docker image using the `preprod`
configuration via a shell alias:

```shell
mithril-client () {
  docker run --rm -it --init \
    -e NETWORK=preprod \
    -e GENESIS_VERIFICATION_KEY=$(curl https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey 2> /dev/null) \
    -e AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator \
    -v $(pwd):/app/data \
    -w /app/data \
    -u $(id -u) \
    ghcr.io/input-output-hk/mithril-client:latest $@
}
```

And download the latest blockchain snapshot:

```shell
SNAPSHOT_DIGEST=$(mithril-client snapshot list --json | jq -r '.[0].digest')
mithril-client snapshot download $SNAPSHOT_DIGEST
```

Then we can follow the instructions on-screen or run a `cardano-node` in the
background with:

```shell
docker run -d \
  -e NETWORK=preprod \
  -v $(pwd):$(pwd) \
  -v $(pwd):/data \
  -v $(pwd):/ipc \
  -u $(id -u) \
  --restart unless-stopped \
  --name cardano-node \
  inputoutput/cardano-node:8.1.2
```

To interact with the `cardano-node` we prepare ourselves a shell alias for the
`cardano-cli`:

<!-- TODO: This failed on mac aarch64 -->

```shell
cardano-cli () {
  docker exec \
    -w $(pwd) \
    -u $(id -u) \
    -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket \
    -e CARDANO_NODE_NETWORK_ID=1 \
    cardano-node cardano-cli $@
}
```

<details>
<summary>Bash auto-completion</summary>

If you are using `bash`, you can get auto-completion of `cardano-cli` using:

```shell
source <(cardano-cli --bash-completion-script cardano-cli)
```

</details>

With `cardano-cli` we can now check the synchronization status:

```shell
cardano-cli query tip
```

This should show something like:

```json
{
  "block": 1275938,
  "epoch": 88,
  "era": "Babbage",
  "hash": "7d22ae918f3ffd35e18c5a7859af27dbcbd29fe08f274b76c284c00042044a2e",
  "slot": 36501000,
  "slotInEpoch": 126600,
  "slotsToEpochEnd": 305400,
  "syncProgress": "100.00"
}
```

Detailed steps on bootstrapping a `cardano-node` using Mithril with more
explanations can be found
[here](https://mithril.network/doc/manual/getting-started/bootstrap-cardano-node)

## Step 2: Prepare keys and funding

As introduced before, the tutorial considers a minimal setup of two participants
that together want to open a Hydra head. We will call them `alice` and `bob`
going forward. Depending on whether you do this tutorial with a friend or alone,
decide who is who or execute the commands on your two distinct setups.

With the `cardano-cli` we first generate Cardano key pairs and addresses to
identify the `hydra-node` and hold funds on the layer one:

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

<Tabs queryString="role">
<TabItem value="alice" label="Alice">

```shell
mkdir -p credentials

cardano-cli address key-gen \
  --verification-key-file credentials/alice-node.vk \
  --signing-key-file credentials/alice-node.sk

cardano-cli address build \
  --verification-key-file credentials/alice-node.vk \
  --out-file credentials/alice-node.addr

cardano-cli address key-gen \
  --verification-key-file credentials/alice-funds.vk \
  --signing-key-file credentials/alice-funds.sk

cardano-cli address build \
  --verification-key-file credentials/alice-funds.vk \
  --out-file credentials/alice-funds.addr
```

</TabItem>
<TabItem value="bob" label="Bob">

```shell
mkdir -p credentials

cardano-cli address key-gen \
  --verification-key-file credentials/bob-node.vk \
  --signing-key-file credentials/bob-node.sk

cardano-cli address build \
  --verification-key-file credentials/bob-node.vk \
  --out-file credentials/bob-node.addr

cardano-cli address key-gen \
  --verification-key-file credentials/bob-funds.vk \
  --signing-key-file credentials/bob-funds.sk

cardano-cli address build \
  --verification-key-file credentials/bob-funds.vk \
  --out-file credentials/bob-funds.addr
```

</TabItem>
</Tabs>

Next we need to send some funds to the node and funding keys. If you have a
wallet on `preprod`, you can send some tADA directly to these addresses shown
after executing:

<Tabs queryString="role">
<TabItem value="alice" label="Alice">

```shell
echo "Send at least 30 tADA to alice-node:"
echo $(cat credentials/alice-node.addr)"\n"

echo "Send any amount of tADA or assets to alice-funds:"
echo $(cat credentials/alice-funds.addr)"\n"
```

</TabItem>
<TabItem value="bob" label="Bob">

```shell
echo "Send at least 30 tADA to bob-node:"
echo $(cat credentials/bob-node.addr)"\n"

echo "Send any amount of tADA or assets to bob-funds:"
echo $(cat credentials/bob-funds.addr)"\n"
```

</TabItem>
</Tabs>

:::info Where to get funds

In case you have no tADA on `preprod`, you can use the [Testnet Faucet](https://docs.cardano.org/cardano-testnet/tools/faucet/) to seed your wallet or the addresses above.
:::

You can check the balance of your addresses via:

<Tabs queryString="role">
<TabItem value="alice" label="Alice">

```shell
echo "# UTxO of alice-node"
cardano-cli query utxo --address $(cat credentials/alice-node.addr) --out-file /dev/stdout | jq

echo "# UTxO of alice-funds"
cardano-cli query utxo --address $(cat credentials/alice-funds.addr) --out-file /dev/stdout | jq
```

</TabItem>
<TabItem value="bob" label="Bob">

```shell
echo "# UTxO of bob-node"
cardano-cli query utxo --address $(cat credentials/bob-node.addr) --out-file /dev/stdout | jq

echo "# UTxO of bob-funds"
cardano-cli query utxo --address $(cat credentials/bob-funds.addr) --out-file /dev/stdout | jq
```

</TabItem>
</Tabs>

Besides the Cardano keys, we now also need to generate Hydra key pairs which
will be used on the layer two by the `hydra-node`. For this, we will use the
`hydra-tools` via docker and a shell alias again:

```shell
hydra-tools () {
  docker run --rm \
    -v $(pwd):$(pwd) \
    -w $(pwd) \
    -u $(id -u) \
    ghcr.io/input-output-hk/hydra-tools:unstable $@
}
```

With `hydra-tools` we generate the keys for `alice` and/or `bob` respectively:

<!-- TODO: feedback of these commands is not great -->
<Tabs queryString="role">
<TabItem value="alice" label="Alice">

```shell
hydra-tools gen-hydra-key --output-file credentials/alice-hydra
```

</TabItem>
<TabItem value="bob" label="Bob">

```shell
hydra-tools gen-hydra-key --output-file credentials/bob-hydra
```

</TabItem>
</Tabs>

If you are doing this tutorial with a friend, now is the time to exchange the
verification (public) keys: `{alice,bob}-node.vk` and `{alice,bob}-hydra.vk`.
You can use any authenticated communication channel for this where you can be
sure your peer cannot be easily impersonated.

Besides keys, we also want to communicate each other's connectivity information.
That is, an IP address / hostname + port where we will be reachable for our
layer two network using `hydra-node`. For the purpose of this tutorial we are
assuming an IP address and port for `alice` and `bob` which works on a single
machine, but please replace usages below with your respective addresses:

<!-- TODO: can we make peers configurable via some text input? -->

Alice: <code>127.0.0.1:5001</code>

Bob: <code>127.0.0.1:5001</code>

We still need one thing, before we can spin up the `hydra-node`, that is the
protocol parameters that the ledger in our Hydra head will use. We can use the
same parameters as on the Cardano layer one, but we tweak them for this tutorial
such that there are no fees! This will fetch the parameters and sets fees +
prices to zero:

```
cardano-cli query protocol-parameters \
  | jq '.txFeeFixed = 0 |.txFeePerByte = 0 | .executionUnitPrices.priceMemory = 0 | .executionUnitPrices.priceSteps = 0' \
  > protocol-parameters.json
```

In summary, the Hydra head participants exchanged and agreed on:

- IP addresses + port on which their `hydra-node` will run.
- A Hydra verification key to identify them in the head.
- A Cardano verification key to identify them on the blockchain.
- The protocol parameters that they want to use in the Hydra head.
- A contestation period for the head closing (we will use the default here).

## Step 3: Start the Hydra node

With all these parameters defined, we now pick a version of the Head protocol we
want to use. This is defined by the `hydra-node --version` itself and the
`--hydra-scripts-tx-id` which point to scripts published on-chain.

For all [released](https://github.com/input-output-hk/hydra/releases) versions
of the `hydra-node` and common Cardano networks, the scripts do get
pre-published and we can just use them. See the [user
manual](../getting-started/quickstart#reference-scripts) for more information
how to publish scripts yourself.

Let's start the `hydra-node` with all these parameters now:

<!-- TODO: add step to wipe or pull all images in the beginning? -->
<Tabs queryString="role">
<TabItem value="alice" label="Alice">

```shell
docker run -d \
  -v $(pwd)/credentials:/credentials:ro \
  -v $(pwd):/ipc \
  -v $(pwd):/data \
  -u $(id -u) \
  --restart unless-stopped \
  --network host \
  --name hydra-node-alice \
  ghcr.io/input-output-hk/hydra-node:unstable \
  --node-id "alice-node" \
  --persistence-dir /data/persistence-alice \
  --cardano-signing-key /credentials/alice-node.sk \
  --hydra-signing-key /credentials/alice-hydra.sk \
  --hydra-scripts-tx-id e5eb53b913e274e4003692d7302f22355af43f839f7aa73cb5eb53510f564496 \
  --ledger-protocol-parameters /data/protocol-parameters.json \
  --testnet-magic 1 \
  --node-socket /ipc/node.socket \
  --port 5001 \
  --api-port 4001 \
  --peer 127.0.0.1:5002 \
  --hydra-verification-key /credentials/bob-hydra.vk \
  --cardano-verification-key /credentials/bob-node.vk
```

</TabItem>
<TabItem value="bob" label="Bob">

```shell
docker run -d \
  -v $(pwd)/credentials:/credentials:ro \
  -v $(pwd):/ipc \
  -v $(pwd):/data \
  -u $(id -u) \
  --restart unless-stopped \
  --network host \
  --name hydra-node-bob \
  ghcr.io/input-output-hk/hydra-node:unstable \
  --node-id "bob-node" \
  --persistence-dir /data/persistence-bob \
  --cardano-signing-key /credentials/bob-node.sk \
  --hydra-signing-key /credentials/bob-hydra.sk \
  --hydra-scripts-tx-id e5eb53b913e274e4003692d7302f22355af43f839f7aa73cb5eb53510f564496 \
  --ledger-protocol-parameters /data/protocol-parameters.json \
  --testnet-magic 1 \
  --node-socket /ipc/node.socket \
  --port 5002 \
  --api-port 4002 \
  --peer 127.0.0.1:5001 \
  --hydra-verification-key /credentials/alice-hydra.vk \
  --cardano-verification-key /credentials/alice-node.vk
```

</TabItem>
</Tabs>

And we can check whether it is running by opening a Websocket connection to the API port:

<Tabs queryString="role">
<TabItem value="alice" label="Alice">

```shell
websocat ws://127.0.0.1:4001 | jq
```

</TabItem>
<TabItem value="bob" label="Bob">

```shell
websocat ws://127.0.0.1:4002 | jq
```

</TabItem>
</Tabs>

This opens a duplex connection and we should see something like:

```json
{
  "peer": "bob-node",
  "seq": 0,
  "tag": "PeerConnected",
  "timestamp": "2023-08-17T18:25:02.903974459Z"
}
{
  "headStatus": "Idle",
  "hydraNodeVersion": "0.12.0-54db2265c257c755df98773c64754c9854d879e8",
  "me": {
    "vkey": "ab159b29b87b498fa060f6045cccf84ecd20cf623f7820ed130ffc849633a120"
  },
  "seq": 1,
  "tag": "Greetings",
  "timestamp": "2023-08-17T18:32:29.092329511Z"
}
```

Before continuing, make sure that you see a `PeerConnected` message for each of
the configured other `hydra-node`. If this is not showing up, double-check
network configuration and connectivity.

## Step 4: Open a Hydra head

Using the `jq` enhanced `websocat` session, we can now communicate with the `hydra-node` through its Websocket API on the terminal. This is a duplex connection and we can just insert commands directly.

<!-- TODO: ideally we would have a ping-pong or hello command (e.g. re-sending greetings) -->

Send this command to initialize a head through the Websocket connection:

```json title="Websocket API"
{ "tag": "Init" }
```

Depending on the network connection, this might take a bit as the node does
submit a transaction on-chain. Eventually, both Hydra nodes and connected
clients should see `HeadIsInitializing` with a list of parties that need to
commit now.

Committing funds to the head means that we pick which UTxO we want to have
available on the layer two. We use the HTTP API of `hydra-node` to commit all
funds given to `{alice,bob}-funds.vk` beforehand:

<Tabs queryString="role">
<TabItem value="alice" label="Alice">

```shell
cardano-cli query utxo \
  --address $(cat credentials/alice-funds.addr) \
  --out-file alice-commit-utxo.json

curl -X POST 127.0.0.1:4001/commit \
  --data @alice-commit-utxo.json \
  > alice-commit-tx.json

cardano-cli transaction sign \
  --tx-file alice-commit-tx.json \
  --signing-key-file credentials/alice-funds.sk \
  --out-file alice-commit-tx-signed.json

cardano-cli transaction submit --tx-file alice-commit-tx-signed.json
```

</TabItem>
<TabItem value="bob" label="Bob">

```shell
cardano-cli query utxo \
  --address $(cat credentials/bob-funds.addr) \
  --out-file bob-commit-utxo.json

curl -X POST 127.0.0.1:4002/commit \
  --data @bob-commit-utxo.json \
  > bob-commit-tx.json

cardano-cli transaction sign \
  --tx-file bob-commit-tx.json \
  --signing-key-file credentials/bob-funds.sk \
  --out-file bob-commit-tx-signed.json

cardano-cli transaction submit --tx-file bob-commit-tx-signed.json
```

</TabItem>
</Tabs>

<details>
<summary>Alternative: Not commit anything</summary>

If you don't want to commit any funds, for example only receive things on the
layer two, you can just request an empty commit transaction like this (example
for `bob`):

```shell
curl -X POST 127.0.0.1:4002/commit --data "{}" > bob-commit-tx.json
cardano-cli transaction submit --tx-file bob-commit-tx.json
```

</details>

This does find all UTxO owned by the funds key, request a commit transaction
draft from the `hydra-node`, sign it with the funds key and submit the
transaction to the Cardano layer one.

Once this transaction was seen by the `hydra-node`, you should see a `Committed`
message on the Websocket connection.

When both parties, `alice` and `bob`, have committed, the head will
automatically open and you will see a `HeadIsOpen` on the Websocket session.
This message also includes the starting balance `utxo`. Notice that the entries
correspond exactly the ones which were committed to the Head (even the Tx hash
and index are the same). The head is now open and ready to be used!

## Step 5: Using the Hydra head

We want to make a basic transaction between `alice` and `bob`. Since Hydra Head
is an isomorphic protocol, all things that work on the layer one also work in
the head. This means that constructing transactions is no different than on
Cardano. This is great since it allows us to use already existing tools like the
`cardano-cli` or frameworks to create transactions!

In this example, we will send `10₳` from `alice` to `bob`, hence you may need to
change the values depending on what you (and your partner) committed to the
head.

First, we need to select a UTxO to spend. We can do this either by looking at
the `utxo` field of the last `HeadIsOpen` or `SnapshotConfirmed` message, or
query the API for the current UTxO set through the websocket session:

```json title="Websocket API"
{ "tag": "GetUTxO" }
```

From the response, we would need to select a UTxO that is owned by `alice` to
spend. We can do that also via the `snapshotUtxo` field in the `Greetings`
message and using this `websocat` and `jq` invocation:

```shell
websocat -U "ws://0.0.0.0:4001?history=no" \
  | jq "select(.tag == \"Greetings\") \
    | .snapshotUtxo \
    | with_entries(select(.value.address == \"$(cat credentials/alice-funds.addr)\"))" \
  > utxo.json
```

Then, just like on the Cardano layer one, we can construct a transaction via the
`cardano-cli` that spends this UTxO:

```shell
cardano-cli transaction build-raw \
  --tx-in $(jq -r 'to_entries[0].key' < utxo.json) \
  --tx-out $(cat credentials/bob-funds.addr)+10000000 \
  --tx-out $(cat credentials/alice-funds.addr)+$(jq 'to_entries[0].value.value.lovelace - 10000000' < utxo.json) \
  --fee 0 \
  --out-file tx.json
```

Note that we need to use the `build-raw` version, since the client cannot (yet?)
index the Hydra head directly and would not find the UTxO to spend. This means
we need to also create a change output with the right amount. Also, because we
have set the protocol parameters of the head to have zero fees, we can use the
`--fee 0` option.

Before submission, we need to sign the transaction to authorize spending `alice`'s funds:

```shell
cardano-cli transaction sign \
  --tx-body-file tx.json \
  --signing-key-file credentials/alice-funds.sk \
  --out-file tx-signed.json
```

To submit the transaction we can use our websocket session again. This command
will print the `NewTx` command to copy paste into an already open websocket
connection:

```shell
cat tx-signed.json | jq -c '{tag: "NewTx", transaction: .cborHex}'
```

The transation will be validated by both `hydra-node`s and either result in a
`TxInvalid` message with a reason, or a `TxValid` message and a
`SnapshotConfirmed` with the new UTxO available in the head shortly after.

🎉 Congratulations, you just processed your first Cardano transaction off-chain
in a Hydra head!

At this stage you can continue experimenting with constructing & submitting
transactions to the head as you wish. Proceed in the tutorial once you're done
and want to realize the exchanged funds from the Hydra head back to the Cardano
layer one.

## Step 6: Closing the Hydra head

Each participant of the head can close it at any point in time. To do this, we
can use the websocket API and submit this command:

```json title="Websocket API"
{ "tag": "Close" }
```

This will have the `hydra-node` submit a protocol transaction to the Cardano
network with the last known snapshot. A smart contract on the layer one will
check the snapshot signatures and confirm the head closed. When this close
transaction is observed, the websocket API sends a `HeadIsClosed` message (this
can also happen if any other `hydra-node` closes the head).

Included in the message will be a `contestationDeadline` which gets set using
the configurable `--contestation-period`. Until this deadline, the closing
snapshot can be contested with a more recent, multi-signed snapshot. Your
`hydra-node` would contest automatically for you if the closed snapshot is not
the last known one.

We need to wait now until the deadline has passed, which will be notified by the
`hydra-node` through the websocket API with a `ReadyToFanout` message.

At this point any head member can issue distribution of funds on the layer one.
You can do this through the websocket API one last time:

```json title="Websocket API"
{ "tag": "Fanout" }
```

This will again submit a transactin to the layer one and once successful is
indicated by a `HeadIsFinalized` message which includes the distributed `utxo`.

To confirm, you can query the funds of both, `alice` and `bob`, on the layer
one:

```shell
echo "# UTxO of alice"
cardano-cli query utxo --address $(cat credentials/alice-funds.addr) --out-file /dev/stdout | jq

echo "# UTxO of bob"
cardano-cli query utxo --address $(cat credentials/bob-funds.addr) --out-file /dev/stdout | jq
```

That's it. That's the full life-cycle of a Hydra head.
