# Configuration

```mdx-code-block
import TerminalWindow from '@site/src/components/TerminalWindow';
```

Running a Hydra head involves operating a Hydra node connected to other Hydra nodes and a Cardano node. The entire configuration of the `hydra-node` is managed through command-line options. Use the `--help` option to see a full list of available commands:

```shell
hydra-node --help
```

Below, we document selected aspects of the configuration. For a comprehensive guide, refer to the [tutorial](./tutorial) or specific _how to_ articles.

### YAML configuration file

Instead of passing every option as a CLI flag, you can write a YAML file and point `hydra-node` at it with `--config`:

```shell
hydra-node --config alice.yaml
```

Key names are the same as their CLI counterparts (kebab-case). Any CLI flag you also provide on the command line **overrides** the value from the file, so you can keep a shared base config and tweak individual options per-run.

A few things to keep in mind:

- **Relative paths are resolved relative to the config file's directory**, not the current working directory. For example, if `alice.yaml` lives in `/etc/hydra/` and sets `hydra-signing-key: alice.sk`, the node will look for `/etc/hydra/alice.sk` regardless of where you run `hydra-node` from.
- **Scalar fields**: CLI flags override YAML values. There is one known limitation: if you pass a CLI flag whose value happens to equal the compiled-in default (e.g. `--api-port 4001` when 4001 is the default), the YAML value wins silently. The node logs a warning when this happens, so watch for it on startup.
- **List fields** (`peers`, `hydra-verification-keys`, `cardano-verification-keys`): CLI and YAML values are **unioned**, not overridden. There is no way to "clear" the YAML list from the CLI — if you need full CLI control over a list, omit it from the config file.

#### Peer list with co-located keys

The YAML format lets you list each peer's addresses and verification keys together in one block. You can include your own node in the list — the node drops any entry whose address matches its own `listen`/`advertise` address, so a single peer block can be shared across all participants.

**Address matching for self-filtering:**

- If `advertise` is set, the self entry must match it exactly (same host string, same port).
- If only `listen` is set, a peer on the same port is considered self when:
  - `listen` is a wildcard (`0.0.0.0`, `::`, `*`) — any peer host on that port is self; or
  - both sides are loopback (`127.0.0.1`, `localhost`, `::1`) in some combination; or
  - the host strings match exactly.

If your `listen` uses a DNS name and your peers are written as IP addresses (or vice versa), self-filtering will *not* match — pin the form in both places, or set `advertise` explicitly.

**`alice.yaml`**
```yaml
node-id: "1"
listen: "127.0.0.1:5001"
advertise: "127.0.0.1:5001"
api-port: 4001
monitoring-port: 6001
hydra-signing-key: "alice.sk"
peers:
  - address: "127.0.0.1:5001"          # self — filtered out automatically
    hydra-verification-key: "alice.vk"
    cardano-verification-key: "alice.cardano.vk"
  - address: "127.0.0.1:5002"
    hydra-verification-key: "bob.vk"
    cardano-verification-key: "bob.cardano.vk"
  - address: "127.0.0.1:5003"
    hydra-verification-key: "carol.vk"
    cardano-verification-key: "carol.cardano.vk"
ledger-protocol-parameters: "protocol-parameters.json"
persistence-dir: "persistence/alice"
chain:
  mode: cardano
  network: preview
  cardano-signing-key: "alice.cardano.sk"
  contestation-period: 43200
  deposit-period: 3600
  backend:
    mode: direct
    node-socket: "node.socket"
```

The self entry (`127.0.0.1:5001` here) carries keys like every other peer — those keys are simply ignored for the self entry, which lets you copy the same `peers:` block into `bob.yaml` and `carol.yaml` and only change the top-level fields.

`bob.yaml` and `carol.yaml` follow the same structure — change `node-id`, `listen`/`advertise`, `api-port`, `monitoring-port`, `hydra-signing-key`, `persistence-dir`, and `cardano-signing-key`.

#### Mirror nodes

A **mirror node** observes the head without holding signing keys. To set one up, add the mirror's address to every participant's peer list **without** any verification keys:

```yaml
# in alice.yaml, bob.yaml, carol.yaml
peers:
  - address: "127.0.0.1:5001"
    hydra-verification-key: "alice.vk"
    cardano-verification-key: "alice.cardano.vk"
  - address: "127.0.0.1:5002"
    hydra-verification-key: "bob.vk"
    cardano-verification-key: "bob.cardano.vk"
  - address: "127.0.0.1:5003"
    hydra-verification-key: "carol.vk"
    cardano-verification-key: "carol.cardano.vk"
  - address: "127.0.0.1:5004"   # mirror — address only, no keys
```

The mirror node itself lists all signing peers and includes its own address with full keys (so it knows which entries to filter out):

**`alice-mirror.yaml`**
```yaml
node-id: "1-mirror"
listen: "127.0.0.1:5004"
advertise: "127.0.0.1:5004"
api-port: 4004
monitoring-port: 6004
hydra-signing-key: "alice.sk"          # same Hydra key as the primary alice node
peers:
  - address: "127.0.0.1:5001"          # alice primary — address only, no keys needed
  - address: "127.0.0.1:5002"
    hydra-verification-key: "bob.vk"
    cardano-verification-key: "bob.cardano.vk"
  - address: "127.0.0.1:5003"
    hydra-verification-key: "carol.vk"
    cardano-verification-key: "carol.cardano.vk"
  - address: "127.0.0.1:5004"          # self — filtered out automatically
    hydra-verification-key: "alice.vk"
    cardano-verification-key: "alice.cardano.vk"
persistence-dir: "persistence/alice-mirror"
chain:
  mode: cardano
  cardano-signing-key: "alice.cardano.sk"
  contestation-period: 43200
  deposit-period: 3600
  backend:
    mode: direct
    node-socket: "node.socket"
```

Run both nodes side by side:

```shell
hydra-node --config alice.yaml
hydra-node --config alice-mirror.yaml
```

Gotchas when running mirror nodes:

- A **peer entry with exactly one verification key** (just `hydra-verification-key` or just `cardano-verification-key`) is rejected at startup. Signing peers need both; observer/mirror peers need neither.
- The mirror in this example reuses the primary Alice node's `hydra-signing-key`, so both processes sign as the same party. Hydra dedups identical `AckSn` messages (there is explicit handling for "same keys, multiple instances"), so this is safe while the two are in sync. The risks are at the edges: if one drifts out of sync and then resyncs, the process whose `AckSn` lost the race can end up one step behind the confirmed chain snapshot, and any on-chain action (close, contest) posted by both races on the UTxO. Treat the mirror as a warm standby rather than a second independent signer.
- Self-filtering compares host strings, so the `listen`/`advertise` value and the self peer entry must use the same form (both IP, or both DNS name). See the address-matching rules above.

### Cardano keys

In a Hydra head, each participant is authenticated using two sets of keys. The first set identifies a participant on the Cardano layer 1 and is used to hold ada for paying fees. Each `hydra-node` requires a `--cardano-signing-key`, and you must provide the `--cardano-verification-key` for each participant.

Generate a Cardano key pair using the `cardano-cli`:

```shell
cardano-cli address key-gen \
  --verification-key-file cardano.vk \
  --signing-key-file cardano.sk
```

These keys authenticate on-chain transactions and ensure that only authorized participants can control the head's lifecycle, preventing unauthorized actors from interfering (eg, posting a spurious `Close`). While this issue does not put participants' funds at risk, it is still inconvenient and can be avoided.

### Hydra keys

The second set of keys are Hydra keys, used for multi-signing snapshots within a head. Although these keys may eventually support an aggregated multi-signature scheme, they currently use the Ed25519 format.

Generate new Hydra keys using the `hydra-node`:

```
hydra-node gen-hydra-key --output-file my-key
```

This command creates two files: `my-key.sk` and `my-key.vk` containing Hydra keys suitable for use inside a head.

For demonstration purposes, we also provide demo key pairs (`alice.{vk,sk}`, `bob.{vk,sk}`, and `carol.{vk,sk}`) in our [demo folder](https://github.com/cardano-scaling/hydra/tree/master/demo). These demo keys should not be used in production.

### Contestation period

The contestation period (CP) is an important protocol parameter, defined in seconds:

```
hydra-node --contestation-period 43200s
```

The contestation period is used to set the **contestation deadline**. That is, after `Close`, all participants have at minimum `CP` to submit a `Contest` transaction. The `hydra-node` does that automatically if it sees a closed state not be the latest it knows.

:::important Consistent contestation period

Being a protocol parameter, all participants in a head must configure the same `--contestation-period` value. Otherwise, the `Init` transaction will be ignored, preventing any participant from stalling or causing a denial-of-service (DoS) attack by setting an unreasonably large contestation period.

:::

The default contestation period is **12 hours (43200 seconds)**, aligned with Cardano's **safe zone** (~12 hours, derived from `3 * k / f`). This ensures that L1 transactions have enough time to settle and participants can reliably dispute when needed. On testnets, you can use shorter periods (e.g., `--contestation-period 600s`) to speed up testing.

:::danger Mainnet safety
On mainnet, the contestation period should be **at least 12 hours**. Shorter periods may not provide sufficient time for dispute resolution due to Cardano's consensus security parameters. See [#2389](https://github.com/cardano-scaling/hydra/issues/2389) for details.
:::

The contestation deadline decides when a closed head can be fanned out. At worst, this is `(1 + n) * CP` after submitting a `Close` transaction, where `n` is the number of participants in the head. This is because the deadline is pushed forward on each `Contest`. With no contestations which may still be `2 * CP` after `Close` depending on the upper validity set on che close transaction. The `hydra-node` currently picks a blanket 200 seconds as [max grace time](https://hydra.family/head-protocol/haddock/hydra-node/Hydra-Chain-Direct-Handlers.html#v:maxGraceTime).

:::warning Invalid `Close` and `Contest` transactions

Depending on the upper validity picked by `hydra-node` and the current network conditions, `Close` and `Contest` transactions could become invalid and be silently rejected by the `cardano-node` to which they have been submitted. This can happen, for example, if:
* The network is congested with many transactions waiting to be included in a block
* The node's connectivity to the network drops, and the transaction is not propagated to block producers fast enough.

Currently, the `hydra-node` does not handle this situation. Each client application should implement a retry mechanism based on the expected time for transaction inclusion.
:::

#### Node synchronization policy

In addition to governing on-chain disputes, the contestation period is also used to determine when a Hydra node is considered **in sync** with the Cardano chain.

:::important
By default, if a node has not observed a new block for **_half the contestation period_**, it is considered **out of sync** and transitions to a **catching up** state.
Beyond this period, the node will reject client inputs and refuse to process new transactions or sign snapshots. This ensures that nodes process inputs only when their view of the chain is recent enough to safely enforce L2 interactions on L1, preserving head safety.
:::

For example, if the head has already closed on L1 but the node is behind the chain by more than the contestation period, it is unsafe to continue signing snapshots on L2, because the node may be unable to contest the head closing and those L2 interactions will become invalid and lost.

In other words, _we can’t risk processing L2 actions while running out of time._

:::warning
A party must not go offline longer than the configured contestation period, otherwise it risks being unable to contest a head closing, violating the principle of head safety.
:::

Using T/2 gives a clean safety property:

  _"An in-sync Hydra node always has at least half the contestation period (T/2)
  to observe and react to an on-chain event."_

This margin:
  - allow for backend or network lag,
  - gives the node time to catch up,
  - ensures it will see at least one new block within the full contestation period (T).

> This policy is based on **wall-clock time**, not the latest known chain tip, as it is unreliable while the chain backend is still synchronizing with the Cardano network.

As a rule of thumb:

  * Large contestation ⇒ relaxed sync requirement:
    - Suitable when blocks may be observed every large period of time.
    - **Availability requirement is low:** the node may be offline for longer periods
      (up to half the contestation period) without falling out of sync.

  * Low contestation ⇒ strict sync requirement:
    - Suitable only for fast and reliable networks where blocks are seen frequently.
    - **Availability requirement is high:** the node must not be offline for more
      than half the contestation period to remain in sync.

:::info
The API server notifies clients whenever the node falls out of sync or returns to a synchronized state.
:::

#### Custom unsynced period

If the default behavior (half the contestation period) does not fit your use case, you can explicitly configure the unsynced period using the `--unsynced-period` option:

```
hydra-node --unsynced-period 3600
```

This sets the period (in seconds) after which the node considers itself out of sync with the chain. For example, with `--unsynced-period 3600`, if no new block is observed for 1 hour, the node will transition to the "catching up" state.

:::warning
The unsynced period should generally be shorter than the contestation period to ensure safety. Setting it too large may cause the node to continue processing L2 transactions when it can no longer safely enforce them on L1.
:::

If not provided, the unsynced period defaults to half the contestation period, maintaining the safety property that an in-sync node always has sufficient time to react to on-chain events.

:::info
This option only applies when connected to a Cardano chain. In [offline mode](#offline-mode), there is no real chain to sync with, so the node is always considered in sync.
:::

### Deposit period

While not a protocol parameter, the deposit period (DP) can be set by any `hydra-node` to configure incremental commits to a head:

```
hydra-node --deposit-period 7200s
```

Anyone can submit a deposit transaction that targets a given head. Each deposit has a **deposit deadline**, after which a deposit can be recovered. All participants need to agree before a deposit can be incremented into the head state and deposited funds are made available on the L2.

For a deposit to be considered by the `hydra-node` the deadline must be further out than `now + DP`. The `hydra-node` will pick the deadline `now + 3 * DP` for any deposit transactions created through `POST /commit`. For example, if you set a deposit period of 2 hours, the deposit will be picked up after 2 hours and at latest after 4 hours, while it may be recovered by the user after 6 hours.

See the [how-to](./how-to/incremental-commit) and [protocol documentation](./dev/protocol#incremental-commits) for more details.

### Reference scripts

The `hydra-node` uses reference scripts to reduce transaction sizes driving the head's lifecycle. Specify the `--hydra-scripts-tx-id` to reference the correct scripts. The `hydra-node` will verify the availability of these scripts on-chain.

:::important Alternative: use --network
Since we pre-publish hydra scrips before each release, instead of specifying hydra-scripts using `--hydra-scripts-tx-id`, you can use `--network` together with the network name you would like to run your hydra-node on (e.g. `--network preview`).
:::

:::warning
`--network` argument only works with officially released hydra-node versions!
:::

Check the scripts against which a hydra-node was compiled using:

```shell
hydra-node --hydra-script-catalogue
```

For public [(test) networks](https://book.world.dev.cardano.org/environments.html), we publish Hydra scripts with each new release, listing transaction IDs in the [release notes](https://github.com/cardano-scaling/hydra/releases) and [`networks.json`](https://github.com/cardano-scaling/hydra/blob/master/hydra-node/networks.json).

To publish scripts yourself, use the `publish-scripts` command:

```shell
hydra-node publish-scripts \
  --testnet-magic 42 \
  --node-socket /path/to/node.socket \
  --cardano-signing-key cardano.sk
```

This command outputs a transaction ID upon success. The provided key should hold sufficient funds (> 50 ada) to create multiple **UNSPENDABLE** UTXO entries on-chain, each carrying a script referenced by the Hydra node.

```shell
hydra-node publish-scripts \
  --testnet-magic 42 \
  --node-socket /path/to/node.socket \
  --cardano-signing-key cardano.sk
```

You can also use blockfrost for script publishing. On top of providing cardano signing key you need to provide a path to the file containing the blockfrost (project id)[https://blockfrost.dev/overview/getting-started#creating-first-project].

```shell
hydra-node publish-scripts \
  --blockfrost /path/to/blockfrost-project.txt \
  --cardano-signing-key cardano.sk
```

### Ledger parameters

The ledger is at the core of a Hydra head. Hydra is currently integrated with Cardano and assumes a ledger configuration similar to layer 1. This translates as a command-line option `--ledger-protocol-parameters`. This defines the updatable protocol parameters such as fees or transaction sizes. These parameters follow the same format as the `cardano-cli query protocol-parameters` output.

We provide existing files in [hydra-cluster/config](https://github.com/cardano-scaling/hydra/blob/master/hydra-cluster/config), which can be used as the basis. In particular, the protocol parameters nullify costs inside a head. Apart from that, they are the direct copy of the current mainnet parameters. An interesting point about Hydra's ledger is that while it re-uses the same rules and code as layer 1 (isomorphic), some parameters can be altered. For example, fees can be adjusted, but not parameters controlling maximum value sizes or minimum ada values, as altering these could make a head unclosable.

A good rule of thumb is that anything that applies strictly to transactions (fees, execution units, max tx size, etc) is safe to change. But anything that could be reflected in the UTXO is not.

:::info About protocol parameters
Many protocol parameters are irrelevant in the Hydra context (eg, there is no treasury or stake pools within a head). Therefore, parameters related to reward incentives or delegation rules are unused.
:::

### Fuel vs funds

Transactions driving the head lifecycle (`Init`, `Close`, etc) must be submitted to layer 1 and hence incur costs. Any UTXO owned by the `--cardano-signing-key` provided to the `hydra-node` can be used to pay fees or serve as collateral for these transactions. We refer to this as **fuel**.

Consequently, sending some ada to the address of this 'internal wallet' is required. To get the address for the Cardano keys as generated above, one can use, for example, the `cardano-cli`:

```shell
cardano-cli address build --verification-key-file cardano.vk --mainnet
# addr1v92l229athdj05l20ggnqz24p4ltlj55e7n4xplt2mxw8tqsehqnt
```

The `hydra-tui` can display this fuel alongside the commit funds on the `Funds`
tab: pass the internal wallet's verification key via `--fuel-key cardano.vk`.
The fuel listing is display-only and is never used when committing.

While the `hydra-node` needs to pay fees for protocol transactions, any wallet can be used to deposit **funds** into an open Hydra head. The `hydra-node` provides an HTTP endpoint at `POST /commit`, allowing you to specify either:
 - A set of `UTXO` outputs to deposit (belonging to public keys), or
 - A _blueprint_ transaction along with the `UTXO` that resolves it.

This endpoint returns a deposit transaction, which is balanced, and all fees are paid by the `hydra-node`. The wallet must sign and submit this transaction to the Cardano network. See the [API documentation](pathname:///api-reference/#operation-publish-/commit) for details.

For more details, refer to the [how to deposit funds](./how-to/incremental-commit) guide.

### Connect to Cardano

The `hydra-node` must be connected to the Cardano network, unless running in [offline mode](./configuration.md#offline-mode).

Hydra node can talk to cardano-node directly or it can be used with the [`Blockfrost API`](https://blockfrost.io/).

#### Through a cardano-node

When using a direct connection to a [`cardano-node`](https://github.com/input-output-hk/cardano-node/) please refer to existing documentation on starting a node, for example on [developers.cardano.org](https://developers.cardano.org/docs/get-started/running-cardano), or [use Mithril](https://mithril.network/doc/manual/getting-started/bootstrap-cardano-node) to bootstrap the local node.

To specify how to connect to the local `cardano-node`, use `--node-socket` and `--testnet-magic`:

```shell
hydra-node \
  --testnet-magic 42 \
  --node-socket devnet/node.socket \
```

#### Through Blockfrost

If you decide to use `Blockfrost` service then hydra-node is started with provided path to the blockfrost [project file](https://blockfrost.dev/overview/getting-started#creating-first-project). Underlying Cardano network is then determined using the blockfrost project file so you should not specify `--mainnet` or `--testnet-magic` arguments:

```shell
hydra-node \
  --blockfrost blockfrost-project.txt \
```

:::info
The `hydra-node` is compatible with the Cardano `mainnet` network, and can consequently operate using **real funds**. Please be sure to read the [known issues](/docs/known-issues) to fully understand the limitations and consequences of running Hydra nodes on mainnet. To choose `mainnet`, use `--mainnet` instead of `--testnet-magic`.
:::

Using the direct node connection or Blockfrost, the `hydra-node` synchronizes the chain and observes Hydra protocol transactions. On startup, it starts observing from the chain's tip. Once a Hydra head has been observed, the point of the last known state change is used automatically.

You can manually set the intersection point using `--start-chain-from <slot>.<hash>` which specifies a `slot` and block header `hash`. For example:

```shell
hydra-node \
  --testnet-magic 2 \
  --node-socket preview/node.socket \
  --start-chain-from 49533501.e364500a42220ea47314215679b7e42e9bbb81fa69d1366fe738d8aef900f7ee
```

To synchronize from the genesis block, use `--start-chain-from 0`.

:::info
If the `hydra-node` already tracks a head in its `hydra.db` and `--start-chain-from` is given, the **newer** point is used.
:::

### Offline mode

Hydra supports an offline mode that allows for disabling the layer 1 interface – the underlying Cardano blockchain from which Hydra heads acquire funds and to which funds are eventually withdrawn. Disabling layer 1 interactions allows use cases that would otherwise require running and configuring an entire layer 1 private devnet. For example, the offline mode can be used to quickly validate a series of transactions against a UTXO, without having to spin up an entire layer 1 Cardano node.

As an offline head will not connect to any chain, we need to provide an `--offline-head-seed` manually, which is a hexadecimal byte string. Offline heads can still use the L2 network and to make multiple `hydra-node` "see" the same offline head, the offline head seed needs to match along with provided [hydra keys](#hydra-keys).

To initialize UTxO state available on the L2 ledger, offline mode takes an obligatory `--initial-utxo` parameter, which points to a JSON-encoded UTxO file. See the [API reference](https://hydra.family/head-protocol/api-reference#schema-UTxO) for the schema.

For example, the following UTxO contains 100 ADA owned by test key [alice-funds.sk](https://github.com/cardano-scaling/hydra/tree/master/hydra-cluster/config/credentials/alice-funds.sk):
```json utxo.json
{
  "0000000000000000000000000000000000000000000000000000000000000000#0": {
    "address": "addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z",
    "value": {
      "lovelace": 100000000
    }
  }
}
```

A single participant offline Hydra head can be started with our demo keys and protocol parameters:
```shell
hydra-node \
  --offline-head-seed 0001 \
  --initial-utxo utxo.json \
  --hydra-signing-key demo/alice.sk \
  --ledger-protocol-parameters hydra-cluster/config/protocol-parameters.json
```

As the node is not connected to a real network, genesis parameters that normally influence things like time-based transaction validation cannot be fetched and are set to defaults. To configure block times, set `--ledger-genesis` to a Shelley genesis file similar to the [shelley-genesis.json](https://book.world.dev.cardano.org/environments/mainnet/shelley-genesis.json).

### API server

The `hydra-node` exposes an [API](/api-reference) for clients to interact with the hydra node, submit transactions to an open, but also initialize / close Hydra heads!

As the API is not authenticated by default, the node is only binding to `localhost`/`127.0.0.1` interfaces and listens on port `4001`. This can be configured using `--api-host` and `--api-port`.

:::warning
The API is not authenticated, and if exposed, an open head can be easily closed through the API!
:::

The API server also supports `TLS` connections (`https://` and `wss://`) when a certificate and key are configured with `--tls-cert` and `--tls-key` respectively.

#### Inspecting the effective configuration

The node exposes a `GET /config` endpoint that returns the effective configuration as a JSON document — the result of merging the YAML config file with any CLI overrides, after resolving all relative paths to absolute ones.

```mdx-code-block
<TerminalWindow>
{`curl http://localhost:4001/config`}
</TerminalWindow>
```

The response mirrors the YAML config file format (kebab-case keys, same hierarchy):

```json
{
  "node-id": "1",
  "listen": "127.0.0.1:5001",
  "api-host": "127.0.0.1",
  "api-port": 4001,
  "quiet": false,
  "hydra-signing-key": "/abs/path/to/alice.sk",
  "hydra-verification-keys": ["/abs/path/to/bob.vk"],
  "peers": ["127.0.0.1:5002"],
  "persistence-dir": "/abs/path/to/persistence/alice",
  "ledger-protocol-parameters": "/abs/path/to/protocol-parameters.json",
  "use-system-etcd": false,
  "api-transaction-timeout": 300.0,
  "chain": {
    "mode": "cardano",
    "cardano-signing-key": "/abs/path/to/alice.cardano.sk",
    "cardano-verification-keys": [],
    "contestation-period": 43200,
    "deposit-period": 3600.0,
    "unsynced-period": 21600.0,
    "backend": {
      "mode": "direct",
      "node-socket": "/abs/path/to/node.socket",
      "testnet-magic": 2
    }
  }
}
```

This is useful for verifying that all paths were resolved correctly, that CLI flags took effect, and for debugging configuration issues. The full schema is described in the [API reference](pathname:///api-reference/#operation-subscribe-/config).

:::caution

The response exposes **absolute filesystem paths** to signing keys, sockets, and the persistence directory. The API is unauthenticated (see the warning above) — do not expose the API port to networks you do not trust.

:::

### Auto compaction of networking buffers

Since switching to the [`etcd`-backed network configuration](https://github.com/cardano-scaling/hydra/pull/1854), the system is more resiliant to nodes going offline.

By default, the network stack keeps **1000** messages which other nodes may be picking up after being offline.

You can override this by setting the relevant `etcd` [environment variables](https://etcd.io/docs/v3.4/op-guide/configuration/). For example, to configure retention of 7 days worth of network messages, in the call to running hydra-node, you can define:

```
ETCD_AUTO_COMPACTION_MODE=periodic
ETCD_AUTO_COMPACTION_RETENTION=168h
```
