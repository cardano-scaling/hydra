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
    --name cardano-node \
    inputoutput/cardano-node:8.1.2
```

To interact with the `cardano-node` we prepare ourselves a shell alias for the
`cardano-cli`:

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

## Step 2: Prepare keys

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
    ghcr.io/input-output-hk/hydra-tools:latest $@
}
```

With `hydra-tools` we generate the keys for `alice` and/or `bob` respectively:

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

export const AliceHost = () => <code>127.0.0.1:5001</code>;

export const BobHost = () => <code>127.0.0.1:5002</code>;

Alice: <AliceHost />

Bob: <BobHost />

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
