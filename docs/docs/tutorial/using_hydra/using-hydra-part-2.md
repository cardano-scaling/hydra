---
sidebar_position: 5
---

# Setup

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```

To showcase the protocol, we consider a minimal setup of two participants that together want to open a hydra head, call these two Bob and Alice. To start, we enter a nix-shell in the `hydra` repo and create a directory to hold some setup files.

```
mkdir test-head
mkdir test-head/Bob
mkdir test-head/Alice
```

Then we create for both participants a Cardano key pair and calculate its associated address. We do this with

```
cardano-cli address key-gen --verification-key-file test-head/Bob/BobCardano.vk --signing-key-file test-head/Bob/BobCardano.sk
cardano-cli address build --payment-verification-key-file test-head/Bob/BobCardano.vk --testnet-magic 2 --out-file test-head/Bob/BobCardano.addr
```

and

```
cardano-cli address key-gen --verification-key-file test-head/Alice/AliceCardano.vk --signing-key-file test-head/Alice/AliceCardano.sk
cardano-cli address build --payment-verification-key-file test-head/Alice/AliceCardano.vk --testnet-magic 2 --out-file test-head/Alice/AliceCardano.addr
```

Next we fund the wallets of Alice

```
cat ./test-head/Alice/AliceCardano.addr
```

via the preview testnet <a href="https://docs.cardano.org/cardano-testnet/tools/faucet">faucet</a>. You can check the balance of this address via

```
cardano-cli query utxo --testnet-magic 2 --address $(cat ./test-head/Alice/AliceCardano.addr)
```

We can use the following script to split these funds with the wallet of Bob.

```
#!/usr/bin/env bash

fullInput=$(cardano-cli query utxo --testnet-magic 2 --address $(cat ./test-head/Alice/AliceCardano.addr) | tail -n 1)
inputTxRef=$(echo $fullInput | awk '{print $1}')
inputTxId=$(echo $fullInput | awk '{print $2}')
inputValue=$(echo $fullInput | awk '{print $3}')

cardano-cli transaction build --babbage-era --testnet-magic 2 \
	--tx-in $inputTxRef#$inputTxId \
	--tx-out $(cat ./test-head/Bob/BobCardano.addr)+$(($inputValue / 2))\
	--change-address $(cat ./test-head/Alice/AliceCardano.addr) \
	--out-file ./test-head/splitTx.tx

cardano-cli transaction sign --testnet-magic 2 \
      	--signing-key-file ./test-head/Alice/AliceCardano.sk \
     	--tx-body-file ./test-head/splitTx.tx \
    	--out-file ./test-head/splitTx.signed

rm ./test-head/splitTx.tx

cardano-cli transaction submit --testnet-magic 2 \
	--tx-file ./test-head/splitTx.signed

rm ./test-head/splitTx.signed
```

We can check the balance of both addresses with

```
cardano-cli query utxo --address $(cat ./test-head/Alice/AliceCardano.addr) --testnet-magic 2
cardano-cli query utxo --address $(cat ./test-head/Bob/BobCardano.addr) --testnet-magic 2
```

:::warning Fuel is deprecated and will be removed in future Hydra versions.
Please take a look at [external-commits](/docs/getting-started/quickstart#external-commits).
:::

Next we will mark some funds at each address so that the hydra-node can use these to pay for the hydra transactions and make sure that these are not committed in the head. Besides preventing having no funds left to close the head or contest to a false checkpoint, it also acts as the fuel for other stages of the protocol. These commands and script will make an output with a specific datum that the hydra node recognizes as fuel. Before we use the script make sure that `jq` is in your path, if not use

```
nix-shell -p jq
```

Then, to execute the script use

```
export CCLI_CMD=$(which cardano-cli)
./sample-node-config/gcp/scripts/fuel-testnet.sh ./preview-testnet/ ./test-head/Alice/AliceCardano.sk 4900000000
./sample-node-config/gcp/scripts/fuel-testnet.sh ./preview-testnet/ ./test-head/Bob/BobCardano.sk 4900000000
```

This will mark about 100 ada as fuel for transactions hydra related. The other funds can be committed to the head.

Now we are going to set up the Hydra keys for the two parties. We can do this via the `hydra-tool` executable. Before we use this, we build this tool along with the `hydra-node` package with

```
cabal build hydra-tools
cabal build hydra-node
```

This can take some time. After this is done, we create two aliases that access these two binaries (you can also add an export to your `.bashrc` file)

```
alias hydra-tools=full/path/to/dist-newstyle/build/x86_64-linux/ghc-8.10.7/hydra-node-0.8.0/x/hydra-tools/build/hydra-tools/hydra-tools
alias hydra-node=full/path/to/dist-newstyle/build/x86_64-linux/ghc-8.10.7/hydra-node-0.8.0/x/hydra-node/build/hydra-node/hydra-node
```

to make them locally available. Alternatively, you can download the prebuilt binaries via this <a href="https://github.com/input-output-hk/hydra-poc/releases/tag/0.8.0">link</a>. Then we can use

```
hydra-tools gen-hydra-key --output-file ./test-head/Alice/AliceHydra
hydra-tools gen-hydra-key --output-file ./test-head/Bob/BobHydra
```

We see the creation of the files `AliceHydra.sk` and `AliceHydra.vk` (similar for Bob). These are the cryptographic key pairs that sign each snapshot for Alice and Bob.

We still need one thing, before we spin up the two hydra-nodes, that is the protocol parameter that we will use in our test head. We will use the protocol parameters that are the same on the testnet, but with a small tweak that there are no fees! Copy these settings from the `hydra` directory with

```
cp hydra-cluster/config/protocol-parameters.json ./test-head/protocol-parameters.json
```

Next we assign Alice the localhost address `127.0.0.1:5001` and Bob `127.0.0.1:5002`. As stated in the protocol outline, we need these four things to initiate the communication of a head

- An IP address + port of their machine that will run the Hydra node.
- A Hydra verification key to identify them in the head.
- A Cardano verification key to identify them on the blockchain.
- The protocol parameters that they want to use in the Hydra head.

Which we have set up above, now we can start a hydra node for each party.

Now we open two terminals and enter a nix-shell for each from the `hydra` directory, and reassign the aliases as above for the `hydra-node`. Before we launch the node, we first change directory to `test-head/Alice. We do this such that each node (that of Alice or Bob) does not interfere with the other. The node might produce some temporary files, so it is good practice to keep nodes separated at run time. After that, we launch a hydra-node for Alice with

```
hydra-node \
	--node-id 1 --port 5001 --api-port 4001 \
	--peer 127.0.0.1:5002 \
	--hydra-signing-key AliceHydra.sk \
	--hydra-verification-key ../Bob/BobHydra.vk \
	--hydra-scripts-tx-id 4081fab39728fa3c05c0edc4dc7c0e8c45129ca6b2b70bf8600c1203a79d2c6d \
	--cardano-signing-key AliceCardano.sk \
	--cardano-verification-key ../Bob/BobCardano.vk \
	--ledger-protocol-parameters ../protocol-parameters.json \
	--network-id 2 \
	--node-socket ../../preview-testnet/node.socket
```

Similarly, we change directories to `test-head/Bob and execute for Bob

```
hydra-node \
	--node-id 2 --port 5002 --api-port 4002 \
	--peer 127.0.0.1:5001 \
	--hydra-signing-key BobHydra.sk \
	--hydra-verification-key ../Alice/AliceHydra.vk \
	--hydra-scripts-tx-id 4081fab39728fa3c05c0edc4dc7c0e8c45129ca6b2b70bf8600c1203a79d2c6d \
	--cardano-signing-key BobCardano.sk \
	--cardano-verification-key ../Alice/AliceCardano.vk \
	--ledger-protocol-parameters ../protocol-parameters.json \
	--network-id 2 \
	--node-socket ../../preview-testnet/node.socket
```

Here a few things stand out, first we see that each party adds the other as a `--peer`. Secondly, each party adds its own Cardano and Hydra signing key and peers Cardano and Hydra verification key. We also see that each node opens a local API for that party to communicate with its own node (using the `--api-port` flag). Lastly, we see the flag `--hydra-scripts-tx-id` followed by a hash. This is a transaction hash on the preview network that contains the hydra protocol scripts in its outputs. This way, we can reference these in our transactions to save on fees when making onchain transactions.
