---
sidebar_position: 4
---

# Preliminaries

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```
In this section, we will showcase the usage of the hydra implementation. Below we will show detailed usages of the main components, these consist of the Hydra node and the usage of the associated API it exposes.

Before we start, we will use some tools that are assumed to be installed. These are

- Nix (with the correct IOG nix caches setup)
- Postman

Here we will use Nix to build the software and Postman will be used to connect to the Hydra API Web Socket. Also, before we start using Hydra, we will set up a cardano-node connected to the preview testnet. Every Hydra node needs a connection to a layer one network to verify and post onchain transactions in a trustless way.
		
To start, we clone the hydra repository using

```
git clone https://github.com/input-output-hk/hydra
```

Change directory to the `hydra` repository and perform checkout to release 0.8.0

```
git checkout 0.8.0
```

Then enter a nix shell using

```
nix-shell
```

This shell also brings a `cardano-node` and `cardano-cli` in scope of our path. Then we make a directory for the preview testnet in the repo with

```
mkdir preview-testnet
mkdir preview-testnet/db
cd preview-testnet/
```

Then download the environment configurations for this testnet via

```
wget https://raw.githubusercontent.com/input-output-hk/cardano-world/master/docs/environments/preview/alonzo-genesis.json
wget https://raw.githubusercontent.com/input-output-hk/cardano-world/master/docs/environments/preview/byron-genesis.json
wget https://raw.githubusercontent.com/input-output-hk/cardano-world/master/docs/environments/preview/shelley-genesis.json
wget https://raw.githubusercontent.com/input-output-hk/cardano-world/master/docs/environments/preview/config.json
wget https://raw.githubusercontent.com/input-output-hk/cardano-world/master/docs/environments/preview/topology.json
```

You can start the node with the command

```
cardano-node run +RTS -N -A16m -qg -qb -RTS --topology ./topology.json --database-path ./db --socket-path ./node.socket --host-addr 0.0.0.0 --port 6000 --config ./config.json
```

Keep this terminal running and open another terminal in the `hydra` repository. Again we enter a nix-shell with

```
nix-shell
```

Once we are in this shell, we export the location of the cardano-node socket with

```
export CARDANO_NODE_SOCKET_PATH=/full/path/to/hydra/preview-testnet/node.socket
```

This will let our system know where the entry point for communication with the node resides, this is necessary for other programs that will utilize the node. We will also use the following command to add auto-completion of the client to our path
```
source <(cardano-cli --bash-completion-script cardano-cli)
```
To check the synchronization process of the node, we query the tip of the local known blockchain data

```
cardano-cli query tip --testnet-magic 2
{
	"block": 337207,
   	"epoch": 84,
   	"era": "Babbage",
   	"hash": "19b809bc1cb6ee28d6f6d004e2f311c26c26fb364ffb9bffa2821e00c3aae98a",
   	"slot": 7307855,
   	"syncProgress": "100.00"
}
```
We see that we are fully in sync with the network.