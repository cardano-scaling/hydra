---
sidebar_position: 3
---

# Without Docker

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';
```

> Running the demo without Docker containers, but with plain executables and scripts.

## Preparation

Make sure that you have a `cardano-node`, `hydra-node` and `hydra-tui` executable in your scope. You can either

 - use `nix-shell demo` or
 - `cabal build` and `cabal exec` them (do not forget the `--` before passing further arguments).

:::info Tip for tmux users
In the `demo` nix-shell, there is a `run-tmux` script which starts a new `tmux` session with multiple windows and panes executing all the commands below!
:::

All further commands are written as if executed from the `demo` folder in the project repository, so make sure to `cd demo` before continuing.

:::info Tip for nix-direnv users
Allowing the `demo/.envrc` will ensure you have the nix shell environment available whenever you are in the `demo/` directory. To use this, opt-in via `direnv allow` after `cd demo`.
:::

## Setting-up The Chain

First, let's prepare and start an ad-hoc, single `cardano-node` devnet using our configuration. Note that this will create a `devnet` directory in your current working directory:

<TerminalWindow>

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

</TerminalWindow>

## Seeding The Network

You can use the `seed-devnet.sh` script by passing it the path/command to a cardano-cli and hydra-node executable to use, instead of having it using the Docker container. For example:


<TerminalWindow>

```
export CARDANO_NODE_SOCKET_PATH=devnet/node.socket
./seed-devnet.sh $(which cardano-cli) $(which hydra-node)
```

</TerminalWindow>

Note, should you want to use `cabal`, pass the invocation for example like this `"cabal exec hydra-node --"`.

## Setting-up The Hydra Network

Then, in 3 different terminals, start 3 Hydra nodes from the `demo/` directory:

:::info Note
We are trying to force ipv4 addresses by using `--peer 127.0.0.1`.
If you don't see any connected peers in the tui it probably means that your system is configured to use ipv6.
:::

````mdx-code-block
<Tabs>

<TabItem value="Alice">
<TerminalWindow>

```
source .env && hydra-node \
  --node-id 1 --port 5001 --api-port 4001 --monitoring-port 6001 \
  --peer 127.0.0.1:5002 \
  --peer 127.0.0.1:5003 \
  --hydra-signing-key alice.sk \
  --hydra-verification-key bob.vk \
  --hydra-verification-key carol.vk \
  --hydra-scripts-tx-id $HYDRA_SCRIPTS_TX_ID \
  --cardano-signing-key devnet/credentials/alice.sk \
  --cardano-verification-key devnet/credentials/bob.vk \
  --cardano-verification-key devnet/credentials/carol.vk \
  --ledger-genesis devnet/genesis-shelley.json \
  --ledger-protocol-parameters devnet/protocol-parameters.json \
  --network-id 42 \
  --node-socket devnet/node.socket
```

</TerminalWindow>
</TabItem>

<TabItem value="Bob">
<TerminalWindow>

```
source .env && hydra-node \
  --node-id 2 --port 5002 --api-port 4002 --monitoring-port 6002 \
  --peer 127.0.0.1:5001 \
  --peer 127.0.0.1:5003 \
  --hydra-signing-key bob.sk \
  --hydra-verification-key alice.vk \
  --hydra-verification-key carol.vk \
  --hydra-scripts-tx-id $HYDRA_SCRIPTS_TX_ID \
  --cardano-signing-key devnet/credentials/bob.sk \
  --cardano-verification-key devnet/credentials/alice.vk \
  --cardano-verification-key devnet/credentials/carol.vk \
  --ledger-genesis devnet/genesis-shelley.json \
  --ledger-protocol-parameters devnet/protocol-parameters.json \
  --network-id 42 \
  --node-socket devnet/node.socket
```

</TerminalWindow>
</TabItem>

<TabItem value="Carol">
<TerminalWindow>

```
source .env && hydra-node \
  --node-id 3 --port 5003 --api-port 4003 --monitoring-port 6003 \
  --peer 127.0.0.1:5001 \
  --peer 127.0.0.1:5002 \
  --hydra-signing-key carol.sk \
  --hydra-verification-key alice.vk \
  --hydra-verification-key bob.vk \
  --hydra-scripts-tx-id $HYDRA_SCRIPTS_TX_ID \
  --cardano-signing-key devnet/credentials/carol.sk \
  --cardano-verification-key devnet/credentials/alice.vk \
  --cardano-verification-key devnet/credentials/bob.vk \
  --ledger-genesis devnet/genesis-shelley.json \
  --ledger-protocol-parameters devnet/protocol-parameters.json \
  --network-id 42 \
  --node-socket devnet/node.socket
```

</TerminalWindow>
</TabItem>


</Tabs>
````

If things go well, the nodes should start logging once connected to the chain.

## Running The Clients
Connect to the nodes using hydra-tui. For example, to use Alice's hydra-node and her on-chain credentials:

````mdx-code-block
<TerminalWindow>

```
cabal exec hydra-tui -- \
  --connect 0.0.0.0:4001 \
  --cardano-signing-key devnet/credentials/alice.sk \
  --network-id 42 \
  --node-socket devnet/node.socket
```

</TerminalWindow>
````

Replace port `4001` with `4002` or `4003` to connect to the other 2 nodes and `alice.sk` with `bob.sk` or `carol.sk` respectively.
