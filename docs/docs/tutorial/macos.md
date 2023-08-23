# Running on Mac OS ARM64

Support for docker on newer Mac running ARM chips M1/M2, aka. `aarch64-darwin` or `arm64-darwin`, is not great, therefore we provide this lighter version of the tutorial running all pre-built binaries for all the needed components. While we follow the same path, the details of the commands are slightly different.

Note that all commands below are assumed to be run from within the same directory.

## Step 0: Preparing the environment

### Installing executables & Required tools

Hydra executables require various libraries to be present on the system, which can be installed through [homebrew](https://brew.sh/):

```
brew install openssl zlib websocat jq
```

You can download pre-built executables for `cardano-node`, `cardano-cli`, `hydra-node`, `mithril-client`, and the accompanying needed libraries from the [Hydra release 0.12.0](https://github.com/input-output-hk/hydra/releases/tag/0.12.0) page.

```
curl -L -o darwin.tgz https://github.com/input-output-hk/hydra/releases/download/0.12.0/tutorial-binaries-aarch64-darwin.tar.gz
```

Then unpack those

```
mkdir bin
tar xzf darwin.tgz -C bin
```

### Defining environment variables

A number of environment variables need to be set in order to run the tutorial and provide basic configuration for the various tools. Make sure those variables are available in all shells you will be running various commands in:

```
export GENESIS_VERIFICATION_KEY=$(curl https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey 2> /dev/null)
export AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator
export CARDANO_NODE_SOCKET_PATH=$(pwd)/node.socket
export CARDANO_NODE_NETWORK_ID=1
export DYLD_FALLBACK_LIBRARY_PATH=$(pwd)/bin
```

## Step 1: Connect to Cardano

Download  mithril snapshot for `preprod` network

```
SNAPSHOT_DIGEST=$(bin/mithril-client snapshot list --json | jq -r '.[0].digest')
bin/mithril-client snapshot download $SNAPSHOT_DIGEST
```

Download proper configuration

```
curl -o config.json https://book.world.dev.cardano.org/environments/preprod/config.json
curl -o topology.json https://book.world.dev.cardano.org/environments/preprod/topology.json
curl -o byron-genesis.json https://book.world.dev.cardano.org/environments/preprod/byron-genesis.json
curl -o shelley-genesis.json https://book.world.dev.cardano.org/environments/preprod/shelley-genesis.json
curl -o alonzo-genesis.json https://book.world.dev.cardano.org/environments/preprod/alonzo-genesis.json
curl -o conway-genesis.json https://book.world.dev.cardano.org/environments/preprod/conway-genesis.json
```

Run a cardano-node

```
bin/cardano-node run --database-path db --socket-path ./node.socket --config config.json --topology topology.json
```

To check the cardano-node is working correctly, open another terminal, setting the environment variables as above and run:

```
bin/cardano-cli query tip --socket-path ./node.socket --testnet-magic 1
```

If it works you should see something like:

```
{
    "block": 1296795,
    "epoch": 89,
    "era": "Babbage",
    "hash": "accf56511fcdeefe9129713c7dc6a0306494b68c26152faf4f472e6568b5cfe0",
    "slot": 37041486,
    "slotInEpoch": 235086,
    "slotsToEpochEnd": 196914,
    "syncProgress": "100.00"
}
```

Note that if the cardano-node is not yet ready because it's still verifying its database, the connection might fail.

## Step 2: Prepare keys and funding

Preparing cardano keys/addresses and funding those is identical to the main tutorial.

To generate Hydra keys,

```
bin/hydra-node gen-hydra-key --output-file credentials/alice-hydra
```

## Step 3: Start the Hydra node

The hydra-node will need to be started from another terminal window.
Make sure you replace bob's address, hydra verification key, and cardano verification key with the right values:

```
bin/hydra-node \
  --node-id "alice-node" \
  --persistence-dir persistence-alice \
  --cardano-signing-key credentials/alice-node.sk \
  --hydra-signing-key credentials/alice-hydra.sk \
  --hydra-scripts-tx-id e5eb53b913e274e4003692d7302f22355af43f839f7aa73cb5eb53510f564496 \
  --ledger-protocol-parameters protocol-parameters.json \
  --testnet-magic 1 \
  --node-socket node.socket \
  --port 5001 \
  --api-port 4001 \
  --peer 127.0.0.1:5002 \
  --hydra-verification-key credentials/bob-hydra.vk \
  --cardano-verification-key credentials/bob-node.vk
```

## Step 4: Open a Hydra head

To connect to the hydra-node using `websocat`, you will need to open a new terminal window. Otherwise, this step is identical to the main tutorial.

## Step 5: Using the Hydra head

This step is identical to the main tutorial

## Step 6: Closing the Hydra head

This step is identical to the main tutorial
