---
sidebar_position: 2
---

# With Docker

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```

> Our standard demo setup for demonstrating the Hydra Head protocol.

The demo consists of:

- a cluster of three Hydra nodes, directly connected to each other, each having access to one of three Hydra credentials `alice`, `bob`, or `carol`;
- a single Cardano node producing blocks used as a local devnet;
- a Prometheus server to gather metrics;
- ad-hoc terminal user interface clients to interact with the individual Hydra nodes;

:::caution Caution!
As we use ad-hoc private devnets that start from the genesis block, you need to ensure the devnet configuration is reasonably up to date. If you get `TraceNoLedgerView` errors from the Cardano node, the start times are too far in the past and you should update them by using the `prepare-devnet.sh` script, for example.
:::

## Setting-up The Network

We'll be using [Docker](https://www.docker.com/get-started) and [compose](https://www.docker.com/get-started) to get the demo running, so make sure you have them in scope or, jump right away to [Running The Demo: Without Docker](/docs/getting-started/demo/without-docker) if you feel like doing it the hard way.

:::info Context
All commands below are written as if executed from the `demo` folder in the project repository, so make sure to clone the repository and `cd demo` before doing anything else.
:::

:::warning OS Compatibility
These instructions have been tested only on Linux environments (Ubuntu, NixOS). If you're on Windows or Mac OS X, you might need to adapt to use [Volumes](https://docs.docker.com/storage/volumes/).
:::

To get started, let's pull the necessary images for services defined in the compose file:

```mdx-code-block
<TerminalWindow>
docker-compose --profile tui pull
</TerminalWindow>
```

From there, we can run the `./prepare-devnet.sh` script to create an initial configuration for our development network. This creates genesis files needed to bootstrap a Cardano blockchain. Note that, for the demo, we use a simple variant of Cardano that requires no stake pools whatsoever.

```mdx-code-block
<TerminalWindow>
./prepare-devnet.sh
</TerminalWindow>
```

That's all for the preliminaries. We can now bring the network up with:

```mdx-code-block
<TerminalWindow>
docker-compose up -d
</TerminalWindow>
```

For convenience, we also provide a script `./run-docker.sh`, which combines the steps above. It also performs a few sanity checks to avoid tripping ourselves.

## Seeding The Network

In the current stage of development, Hydra nodes need a specially crafted set of UTXO to drive the Head protocol ("fuel"), and of course, some UTXO to be committed to the Head.
The script included `./seed-devnet.sh` uses the `cardano-cli` in the already running `cardano-node` container to give Alice, Bob, and Carol some UTXO entries to commit and some fuel UTXO.

:::info
There is nothing special about those transactions, so one could use any other Cardano client to create them. Yet, they must have the following characteristics:
Must pay outputs to commit to the key used by the Hydra Node's internal wallet, as defined by argument `--cardano-signing-key` of hydra-node executable,
One of the outputs must include datum hash `a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3`, as this is our "fuel" marker.
:::

## Running The Clients

Using compose, you can start the demo Terminal-based User Interface (a.k.a. `hydra-tui`) to interact with Hydra nodes. There are 3 preconfigured TUI services in the compose definition: `hydra-tui-1`, `hydra-tui-2`, and `hydra-tui-3`. To connect to the first Hydra node in a terminal, run the following commands:

```mdx-code-block
<TerminalWindow>
docker-compose --profile tui run hydra-tui-1
</TerminalWindow>
```

This will start a full-blown terminal interface loaded with signing keys corresponding to the first Hydra node. In other terminals, you can start other nodes in a similar fashion targeting `hydra-tui-2` and `hydra-tui-3` services.
