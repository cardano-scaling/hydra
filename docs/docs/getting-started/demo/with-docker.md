---
sidebar_position: 2
---

# With Docker

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```

We'll be using [Docker](https://www.docker.com/get-started) and [compose](https://www.docker.com/get-started) to get the demo running, so make sure you have them in scope or, jump right away to [Running The Demo: Without Docker](./without-docker) if you feel like doing it the hard way.

:::info Shortcut
For convenience, we also provide a script `./run-docker.sh`, which combines the steps above. It also performs a few sanity checks to avoid tripping ourselves.
:::

:::info Context
All commands below are written as if executed from the `demo` folder in the project repository, so make sure to clone the repository and `cd demo` before doing anything else.
:::

:::warning OS Compatibility
These instructions have been tested only on Linux environments (Ubuntu, NixOS). If you're on Windows or Mac OS X, you might need to adapt to use [Volumes](https://docs.docker.com/storage/volumes/).
:::

## Setting-up The Chain

To get started, let's pull the necessary images for services defined in the compose file:

```mdx-code-block
<TerminalWindow>
docker-compose --profile tui --profile hydra-node pull
</TerminalWindow>
```

From there, we can run the `./prepare-devnet.sh` script to create an initial configuration for our development network. This creates genesis files needed to bootstrap a Cardano blockchain. Note that, for the demo, we use a simple variant of Cardano that requires no stake pools whatsoever.

```mdx-code-block
<TerminalWindow>
./prepare-devnet.sh
</TerminalWindow>
```

We can now bring the Cardano node up with:

```mdx-code-block
<TerminalWindow>
docker-compose up -d cardano-node
</TerminalWindow>
```

:::caution Caution!
As we use ad-hoc private devnets that start from the genesis block, you need to ensure the devnet configuration is reasonably up to date. If you get `TraceNoLedgerView` errors from the Cardano node, the start times are too far in the past and you should update them by using the `prepare-devnet.sh` script, for example.
:::

You can verify that the node is up-and-running by checking the logs with `docker-compose logs cardano-node -f`. You should see traces containing `TraceAdoptedBlock`, which means that the devnet is producing blocks .. nice!

## Seeding The Network

We include a script `seed-devnet.sh` that uses the `cardano-cli` in the already
running `cardano-node` container to give Alice, Bob, and Carol some UTXO entries
to commit and some fuel UTXO.

```mdx-code-block
<TerminalWindow>
./seed-devnet.sh
</TerminalWindow>
```

## Starting Hydra Nodes

Finally, now that the on-chain preparations are ready, we can bring the Hydra network (i.e. all three nodes for Alice, Bob and Carol) up by running:

```mdx-code-block
<TerminalWindow>
docker-compose --profile hydra-node up -d
</TerminalWindow>
```

## Running The Clients

Using compose, you can start the demo Terminal-based User Interface (a.k.a. `hydra-tui`) to interact with Hydra nodes. There are 3 preconfigured TUI services in the compose definition: `hydra-tui-1`, `hydra-tui-2`, and `hydra-tui-3`. To connect to the first Hydra node in a terminal, run the following commands:

```mdx-code-block
<TerminalWindow>
docker-compose --profile tui run hydra-tui-1
</TerminalWindow>
```

This will start a full-blown terminal interface loaded with signing keys corresponding to the first Hydra node. In other terminals, you can start other nodes in a similar fashion targeting `hydra-tui-2` and `hydra-tui-3` services.
