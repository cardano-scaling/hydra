# Getting started
To get started quickly, we'll walk you through the standard demo setup, which includes:

* A cluster of three `hydra-nodes`, each directly connected to the others and configured with one of three Hydra credentials: `Alice`, `Bob`, or `Carol`
* A single `cardano-node` producing blocks used as a (very fast) local `devnet`
* The `hydra-tui` example for clients to interact with the individual `hydra-node`.

This tutorial uses [Docker](https://www.docker.com/get-started) to install the nodes, so ensure Docker is installed. If you want to explore alternative ways of running the tools, see a [variant of this tutorial](./getting-started-without-docker.md) or the [testnet tutorial](./tutorial/index.md), which builds Hydra with Nix and downloads Cardano binaries. The documentation pages on [installation](./installation) and [configuration](./configuration) provide more details.

Additionally, the `hydra-tui` uses the HTTP/WebSocket API provided by the `hydra-node` behind the scenes. The [testnet tutorial](./tutorial/index.md) will show how to use this API using low-level commands, or you can see the [API reference](/api-reference) for more details.



:::caution OS Compatibility
These instructions have been tested only on Linux environments (Ubuntu, NixOS). If you're on Windows or Mac OS X, you might need to adapt to use [Volumes](https://docs.docker.com/storage/volumes/).
:::

## Preparation

All commands below are written as if executed from the `demo` folder in the project repository. Ensure you clone the repository, switch into the `demo` folder, and pull the latest Docker images:

```shell
git clone --depth 1 https://github.com/cardano-scaling/hydra.git
cd hydra/demo
docker compose pull
```

:::info Faster clone
`--depth 1` fetches only the latest commit, which is all this tutorial needs and downloads a small fraction of the full repository. Drop the flag if you want the complete history.
:::

:::info Shortcut
For convenience, we provide a script `./run-docker.sh`, which combines all the following steps. It also performs a few sanity checks to prevent potential issues.
:::

## Set up the devnet

Next, prepare the devnet configuration to bootstrap a local Cardano blockchain.
**Note** that we use a simplified variant of Cardano that does not require any stake pools.

```shell
./prepare-devnet.sh
```

Bring the Cardano node up with:

```shell
docker compose up -d cardano-node
```

Verify that the devnet is up-and-running by checking the logs with:
```shell
docker compose logs cardano-node -f
```

You should see traces containing `TraceAdoptedBlock`, which means that the devnet is producing blocks .. nice!


:::info
Do not wait too long between these two commands. If you get `TraceNoLedgerView` errors from the Cardano node, the genesis start time is too far in the past, and you need to update them by running `prepare-devnet.sh` again.
:::

:::tip
You can use [jq](https://jqlang.github.io/jq/) to follow the logs and see the node update kinds with the
following command:

```
docker compose logs -f --no-log-prefix cardano-node | jq -Rr 'fromjson? | .data.kind'
```
:::


Next, we need to give Alice, Bob, and Carol some UTXOs to deposit into the head and ada for paying fees. To do this, use the `seed-devnet.sh` script, which uses the `cardano-cli` within the already running `cardano-node` container:

```shell
./seed-devnet.sh
```

## Start Hydra nodes

Finally, now that the on-chain preparations are done, we can bring the Hydra network (all three nodes for Alice, Bob, and Carol) up by running:

```shell
docker compose up -d hydra-node-{1,2,3}
```

Using `compose`, you can start the demo Terminal-based User Interface (`hydra-tui`) to interact with Hydra nodes. There are three preconfigured TUI services in the `compose` definition: `hydra-tui-1`, `hydra-tui-2`, and `hydra-tui-3`. To connect to the first Hydra node in a terminal, run the following command:

```shell
docker compose run hydra-tui-1
```

This will start a full-blown terminal interface loaded with signing keys corresponding to the first Hydra node. In other terminals, you can start other nodes in a similar fashion targeting `hydra-tui-2` and `hydra-tui-3` services.

## Use the head

Press `[i]nit` in any TUI to open an empty head. Once it is open, use
`[c]ommit` to deposit the pre-funded UTxOs. Wait for the deposit to finalize
before spending its outputs. The demo uses short activation and deposit
periods suitable only for the local devnet.

Use the TUI's on-screen actions to submit transactions, inspect deposits,
close the head, and fan out funds after the contestation deadline. For the
same workflow through HTTP and WebSocket, follow the
[testnet tutorial](./tutorial/index.md).

## Monitoring

We provide a minimal monitoring setup using Prometheus and Grafana to give you insight into the node's activity, including snapshot processing and peer connectivity.

:::info Note
This setup covers Hydra activity, queues, API requests, persistence, process CPU and Linux resident memory. Use host exporters for machine-wide disk, memory and network utilization.
:::

Hydra nodes expose [Prometheus](https://prometheus.io/)-compliant _metrics_ through an HTTP server, available on the standard `/metrics` endpoint.

:::info Shortcut
Reference the [Operating Hydra nodes guide](./how-to/operating-hydra.md#monitoring) for a more in-depth explanation on available metrics.
:::

### Start the monitoring stack

Bring up Prometheus and Grafana:

```shell
docker compose up -d prometheus grafana
```

Prometheus will scrape the `/metrics` endpoint of each Hydra node automatically.

Grafana is preconfigured with per-node readiness, transaction rates, windowed
mean and p95 latency, chain and network health, input queues, deposits, decommit,
fanout, API, persistence and process resource panels. The instance dropdown
selects nodes; peer counts keep complete instance names. Confirmation rates are
per replica and must not be summed as unique Head throughput.

Prometheus also loads the example rules in `demo/prometheus/hydra.rules.yml`.
Tune the thresholds and configure Alertmanager destinations for notifications.


### Using Grafana

1. Open Grafana in your browser at `http://localhost:3000`.
2. Default login credentials are `admin/admin`.
3. Explore the preconfigured dashboard or create your own panels using Prometheus queries.
4. Use the top-left `instance` dropdown to inspect metrics per Hydra node.
5. Hover on any graph or stat panel to see per-node values in real time.

