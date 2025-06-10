---
sidebar_position: 3
---

# Getting started without Docker

```mdx-code-block
import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';
```

This tutorial guides you through the same setup as the [docker-based one](./getting-started), but without using Docker containers and only using executables and scripts.

:::info Shortcut using nix
All steps of this tutorial are also available in a combined [process-compose](https://github.com/F1bonacc1/process-compose) setup via `nix run .#demo`
:::

## Preparation

Make sure that you have a `cardano-node`, `hydra-node` and `hydra-tui` executable in your scope. You can either

- use `nix develop .#demo` or
- `cabal build` and `cabal exec` them (do not forget the `--` before passing further arguments).

All further commands are written as if executed from the `demo` folder in the project repository:

```shell
cd demo
```

:::info Tip for nix-direnv users
Allowing the `demo/.envrc` will ensure you have the nix shell environment available whenever you are in the `demo/` directory. To use this, opt-in via `direnv allow` after `cd demo`.
:::

:::info Tmux shortcut
In the `demo` nix shell, there is a `run-tmux` script which starts a new `tmux` session with multiple windows and panes executing all the commands below!
:::


## Set up the devnet

First, let's prepare and start an ad-hoc, single `cardano-node` devnet using our configuration. Note that this will create a `devnet` directory in your current working directory:

```
./prepare-devnet.sh
cd devnet
cardano-node run \
  --config cardano-node.json \
  --topology topology.json \
  --database-path db \
  --socket-path node.socket \
  --shelley-operational-certificate opcert.cert \
  --shelley-kes-key kes.skey \
  --shelley-vrf-key vrf.skey
```

From the `demo` folder you can use the `seed-devnet.sh` script by passing it the path/command to a cardano-cli and hydra-node executable to use, instead of having it using the Docker container. For example:

```
export CARDANO_NODE_SOCKET_PATH=devnet/node.socket
./seed-devnet.sh $(which cardano-cli) $(which hydra-node)
```

Note, should you want to use `cabal`, pass the invocation for example like this `"cabal exec hydra-node --"`.

## Start Hydra nodes

Then, in 3 different terminals, start 3 Hydra nodes from the `demo/` directory:

:::info Note
We are trying to force ipv4 addresses by using `--peer 127.0.0.1`.
If you don't see any connected peers in the tui it probably means that your system is configured to use ipv6.
:::

````mdx-code-block
<Tabs>

<TabItem value="Alice">

```
source .env && hydra-node \
  --node-id 1 --listen 127.0.0.1:5001 --api-port 4001 --monitoring-port 6001 \
  --peer 127.0.0.1:5002 \
  --peer 127.0.0.1:5003 \
  --hydra-signing-key alice.sk \
  --hydra-verification-key bob.vk \
  --hydra-verification-key carol.vk \
  --hydra-scripts-tx-id $HYDRA_SCRIPTS_TX_ID \
  --cardano-signing-key devnet/credentials/alice.sk \
  --cardano-verification-key devnet/credentials/bob.vk \
  --cardano-verification-key devnet/credentials/carol.vk \
  --ledger-protocol-parameters devnet/protocol-parameters.json \
  --testnet-magic 42 \
  --node-socket devnet/node.socket \
  --persistence-dir devnet/persistence/alice
```

</TabItem>

<TabItem value="Bob">

```
source .env && hydra-node \
  --node-id 2 --listen 127.0.0.1:5002 --api-port 4002 --monitoring-port 6002 \
  --peer 127.0.0.1:5001 \
  --peer 127.0.0.1:5003 \
  --hydra-signing-key bob.sk \
  --hydra-verification-key alice.vk \
  --hydra-verification-key carol.vk \
  --hydra-scripts-tx-id $HYDRA_SCRIPTS_TX_ID \
  --cardano-signing-key devnet/credentials/bob.sk \
  --cardano-verification-key devnet/credentials/alice.vk \
  --cardano-verification-key devnet/credentials/carol.vk \
  --ledger-protocol-parameters devnet/protocol-parameters.json \
  --testnet-magic 42 \
  --node-socket devnet/node.socket \
  --persistence-dir devnet/persistence/bob
```

</TabItem>

<TabItem value="Carol">

```
source .env && hydra-node \
  --node-id 3 --listen 127.0.0.1:5003 --monitoring-port 6003 \
  --peer 127.0.0.1:5001 \
  --peer 127.0.0.1:5002 \
  --hydra-signing-key carol.sk \
  --hydra-verification-key alice.vk \
  --hydra-verification-key bob.vk \
  --hydra-scripts-tx-id $HYDRA_SCRIPTS_TX_ID \
  --cardano-signing-key devnet/credentials/carol.sk \
  --cardano-verification-key devnet/credentials/alice.vk \
  --cardano-verification-key devnet/credentials/bob.vk \
  --ledger-protocol-parameters devnet/protocol-parameters.json \
  --testnet-magic 42 \
  --node-socket devnet/node.socket \
  --persistence-dir devnet/persistence/carol
```

</TabItem>


</Tabs>
````

If things go well, the nodes should start logging once connected to the chain.

## Run Hydra clients

Connect to the nodes using hydra-tui.

````mdx-code-block
<Tabs>

<TabItem value="Alice">

```
hydra-tui \
  --connect 0.0.0.0:4001 \
  --cardano-signing-key devnet/credentials/alice-funds.sk \
  --testnet-magic 42 \
  --node-socket devnet/node.socket
```

</TabItem>

<TabItem value="Bob">

```
hydra-tui \
  --connect 0.0.0.0:4002 \
  --cardano-signing-key devnet/credentials/bob-funds.sk \
  --testnet-magic 42 \
  --node-socket devnet/node.socket
```

</TabItem>

<TabItem value="Carol">

```
hydra-tui \
  --connect 0.0.0.0:4003 \
  --cardano-signing-key devnet/credentials/carol-funds.sk \
  --testnet-magic 42 \
  --node-socket devnet/node.socket
```

</TabItem>

</Tabs>
````
