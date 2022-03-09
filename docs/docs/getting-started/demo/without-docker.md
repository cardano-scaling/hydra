---
sidebar_position: 2
---

# Without Docker

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';
```

> Running the demo without docker containers, but with plain executables and scripts.

:::info Context
All commands below are written as if executed from the `demo` folder in the project repository, so make sure to clone the repository and `cd demo` before anything.
:::

# Setting-up The Network

One needs to prepare a `cardano-node` (devnet) and `hydra-node`s "manually". These instructions assume you have both built and in scope for `cabal exec`.

First, let's prepare and start an ad-hoc, single `cardano-node` devnet using our configuration. Note that this will create a `devnet` directory in your current working directory:

````mdx-code-block
<TerminalWindow>

```
./prepare-devnet.sh
cd devnet
cabal exec cardano-node -- run \
  --config cardano-node.json \
  --topology topology.json \
  --database-path db \
  --socket-path ipc/node.socket \
  --shelley-operational-certificate credentials/opcert1.cert \
  --shelley-kes-key credentials/delegate1.kes.skey \
  --shelley-vrf-key credentials/delegate1.vrf.skey
```

</TerminalWindow>
````

Then in 3 different terminals, start 3 Hydra nodes from the `demo/` directory:

````mdx-code-block
<Tabs>

<TabItem value="Alice">
<TerminalWindow>

```
cabal exec hydra-node -- \
  --node-id 1 --port 5001 --api-port 4001 --monitoring-port 6001 \
  --peer localhost:5002 \
  --peer localhost:5003 \
  --hydra-signing-key alice.sk \
  --hydra-verification-key bob.vk \
  --hydra-verification-key carol.vk \
  --cardano-signing-key devnet/credentials/alice.sk \
  --cardano-verification-key devnet/credentials/bob.vk \
  --cardano-verification-key devnet/credentials/carol.vk \
  --ledger-genesis devnet/genesis-shelley.json \
  --ledger-protocol-parameters devnet/protocol-parameters.json \
  --network-id 42 \
  --node-socket devnet/ipc/node.socket
```

</TerminalWindow>
</TabItem>

<TabItem value="Bob">
<TerminalWindow>

```
cabal exec hydra-node -- \
  --node-id 2 --port 5002 --api-port 4002 --monitoring-port 6002 \
  --peer localhost:5001 \
  --peer localhost:5003 \
  --hydra-signing-key bob.sk \
  --hydra-verification-key alice.vk \
  --hydra-verification-key carol.vk \
  --cardano-signing-key devnet/credentials/bob.sk \
  --cardano-verification-key devnet/credentials/alice.vk \
  --cardano-verification-key devnet/credentials/carol.vk \
  --ledger-genesis devnet/genesis-shelley.json \
  --ledger-protocol-parameters devnet/protocol-parameters.json \
  --network-id 42 \
  --node-socket devnet/ipc/node.socket
```

</TerminalWindow>
</TabItem>

<TabItem value="Carol">
<TerminalWindow>

```
cabal exec hydra-node -- \
  --node-id 3 --port 5003 --api-port 4003 --monitoring-port 6003 \
  --peer localhost:5001 \
  --peer localhost:5002 \
  --hydra-signing-key carol.sk \
  --hydra-verification-key alice.vk \
  --hydra-verification-key bob.vk \
  --cardano-signing-key devnet/credentials/carol.sk \
  --cardano-verification-key devnet/credentials/alice.vk \
  --cardano-verification-key devnet/credentials/bob.vk \
  --ledger-genesis devnet/genesis-shelley.json \
  --ledger-protocol-parameters devnet/protocol-parameters.json \
  --network-id 42 \
  --node-socket devnet/ipc/node.socket
```

</TerminalWindow>
</TabItem>


</Tabs>
````

If things go well, the nodes should start logging once they are connected to the chain.

# Seeding The Network

You can use the `seed-devnet.sh` script by passing it the path to a cardano-cli executable to use instead of having it using the docker container, e.g.


```mdx-code-block
<TerminalWindow>
./seed-devnet.sh $(which cardano-cli)
</TerminalWindow>
```

Running The Clients
Connect to the nodes using hydra-tui. For example to use Alice's hydra-node and her on-chain credentials:

````mdx-code-block
<TerminalWindow>

```
cabal exec hydra-tui -- \
  --connect localhost:4001 \
  --cardano-signing-key devnet/credentials/alice.sk \
  --network-id 42 \
  --node-socket devnet/ipc/node.socket
```

</TerminalWindow>
````

Replace port `4001` with `4002` or `4003` to connect to the other 2 nodes and `alice.sk` with `bob.sk` or `carol.sk` respectively.
