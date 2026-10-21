---

sidebar_position: 8
---

# Sideload a snapshot

```mdx-code-block
import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';
```

Use `POST /snapshot` to reset local state to a confirmed snapshot when an open head cannot make progress. This example deliberately gives one node different ledger parameters, then fixes the configuration and resets all three nodes.

**Prerequisites**

Run the commands from the Hydra repository root in a development shell with `cabal`, `cardano-cli`, `jq`, `curl`, and `websocat`. Use Bash and a fresh `tmp-sideload/` directory.

## Step 1: Open a Head

For an easy demo setup, we will open an [offline head](../configuration#offline-mode).

:::info
You could also run a local [demo](./../getting-started).
:::

<details>
  <summary>Offline Head</summary>

```shell
cat > utxo.json <<EOF
{
  "0000000000000000000000000000000000000000000000000000000000000000#0": {
    "address": "addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z",
    "value": {
      "lovelace": 100000000
    }
  }
}
EOF
``` 
</details>
  
````mdx-code-block
<Tabs>

<TabItem value="Alice">

```shell
cabal run hydra-node:exe:hydra-node -- --offline-head-seed 0001 --initial-utxo utxo.json \
  --ledger-protocol-parameters hydra-cluster/config/protocol-parameters.json \
  --persistence-dir tmp-sideload/alice \
  --api-port 4001 \
  --listen 127.0.0.1:5001 \
  --peer 127.0.0.1:5002 \
  --peer 127.0.0.1:5003 \
  --hydra-signing-key demo/alice.sk \
  --hydra-verification-key demo/bob.vk \
  --hydra-verification-key demo/carol.vk
```

</TabItem>

<TabItem value="Bob">

```shell
cabal run hydra-node:exe:hydra-node -- --offline-head-seed 0001 --initial-utxo utxo.json \
  --ledger-protocol-parameters hydra-cluster/config/protocol-parameters.json \
  --persistence-dir tmp-sideload/bob \
  --api-port 4002 \
  --listen 127.0.0.1:5002 \
  --peer 127.0.0.1:5001 \
  --peer 127.0.0.1:5003 \
  --hydra-signing-key demo/bob.sk \
  --hydra-verification-key demo/alice.vk \
  --hydra-verification-key demo/carol.vk
```

</TabItem>

<TabItem value="Carol">

```shell
cabal run hydra-node:exe:hydra-node -- --offline-head-seed 0001 --initial-utxo utxo.json \
  --ledger-protocol-parameters <(jq '.txFeeFixed = 16000' hydra-cluster/config/protocol-parameters.json) \
  --persistence-dir tmp-sideload/carol \
  --api-port 4003 \
  --listen 127.0.0.1:5003 \
  --peer 127.0.0.1:5001 \
  --peer 127.0.0.1:5002 \
  --hydra-signing-key demo/carol.sk \
  --hydra-verification-key demo/alice.vk \
  --hydra-verification-key demo/bob.vk
```

</TabItem>

</Tabs>
````

Run Alice, Bob, and Carol in separate terminals. Carol deliberately uses different protocol parameters.
```shell
curl --fail-with-body -sS http://127.0.0.1:{4001,4002,4003}/protocol-parameters | jq '.txFeeFixed'
```
```text
0
0
16000
```

> This is intentional to force the head to get stuck upon submitting a NewTx.

## Step 2: Stall snapshot confirmation

Offline mode deposits the supplied UTxO after opening the head. Wait for
`CommitFinalized` on each node before spending it. In a separate terminal,
connect to each API port (`4001`, `4002`, and `4003`):

```shell
websocat "ws://127.0.0.1:4001?history=yes"
```

After observing settlement, stop the monitor with Ctrl-C and build the
transaction for the next snapshot.

To do this, we will prepare a self-transfer from Alice to Alice using `cardano-cli`:

```shell
cardano-cli conway transaction build-raw \
  --tx-in 0000000000000000000000000000000000000000000000000000000000000000#0 \
  --tx-out addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z+100000000 \
  --fee 0 \
  --out-file self-tx.json

cardano-cli conway transaction sign \
  --tx-body-file self-tx.json \
  --signing-key-file hydra-cluster/config/credentials/alice-funds.sk \
  --out-file signed-self-tx.json
```

> Note that the transaction is expected to consume the output from the initial `utxo.json` used to open the head.

Now, let's submit it using `websocat`:

```shell
cat signed-self-tx.json | jq -c '{tag: "NewTx", transaction: .}' | websocat "ws://127.0.0.1:4001?history=no"
```

Query the latest confirmed snapshots:

```shell
curl --fail-with-body -sS http://127.0.0.1:{4001,4002,4003}/snapshot | jq
```

The latest confirmed snapshot still excludes the self-transfer. In-flight
state can differ between peers: Alice and Bob may report `SeenSnapshot`,
while Carol reports `LastSeenSnapshot` because she rejected the zero-fee
transaction. Inspect it with:

```shell
curl --fail-with-body -sS http://127.0.0.1:{4001,4002,4003}/snapshot/last-seen | jq
```

Carol's log reports `TxInvalid` with `FeeTooSmallUTxO` because her parameters
require a fee of 16000 lovelace.

## Step 3: Resume snapshot confirmation

Save each node's last confirmed snapshot and post it back to that node:

```shell
for port in 4001 4002 4003; do
  curl --fail-with-body -sS "http://127.0.0.1:$port/snapshot" \
    -o "snapshot-$port.json" &&
  curl --fail-with-body -sS "http://127.0.0.1:$port/snapshot" \
    -H 'Content-Type: application/json' \
    --data-binary "@snapshot-$port.json"
done
```

In this example, all three confirmed snapshots are identical. In a real
recovery, first agree on a valid confirmed snapshot; posting different
snapshots does not make the nodes consistent.

Each node emits `SnapshotSideLoaded` when it adopts the snapshot. Inspect
the history in a separate terminal for each port (`4001`, `4002`, and `4003`):

```shell
websocat "ws://127.0.0.1:4001?history=yes"
```

This output confirms that the node is ready to continue operating from that position.

> Note that the snapshot leader has been reset, and the ongoing snapshot signing round has been discarded. This means any pending transactions were pruned and must be re-submitted. A decommit the side-loaded snapshot is still settling is kept, as every snapshot request at that version must carry it; a decommit requested but not yet in a confirmed snapshot is pruned like the transactions.

Now, before we re-submit the same transaction, we need to fix Carol's node by stopping Carol with Ctrl-C and restarting her node with the correct ledger protocol parameters:

```shell
cabal run hydra-node:exe:hydra-node -- --offline-head-seed 0001 --initial-utxo utxo.json \
  --ledger-protocol-parameters hydra-cluster/config/protocol-parameters.json \
  --persistence-dir tmp-sideload/carol \
  --api-port 4003 \
  --listen 127.0.0.1:5003 \
  --peer 127.0.0.1:5001 \
  --peer 127.0.0.1:5002 \
  --hydra-signing-key demo/carol.sk \
  --hydra-verification-key demo/alice.vk \
  --hydra-verification-key demo/bob.vk
```

Wait for Carol to reconnect to Alice and Bob.

Let's re-submit the same transaction:
```shell
cat signed-self-tx.json | jq -c '{tag: "NewTx", transaction: .}' | websocat "ws://127.0.0.1:4001?history=no"
```

And check if it has been adopted:
```sh
curl --fail-with-body -sS http://127.0.0.1:{4001,4002,4003}/snapshot/last-seen | jq
```

We can also check the transaction was included in the latest confirmed snapshot:

```shell
curl --fail-with-body -sS http://127.0.0.1:{4001,4002,4003}/snapshot | jq
```

## Limits

A side-loaded snapshot has to be one the head could actually close and fan out,
so `POST /snapshot` refuses a snapshot committing to more than **4095** outputs
(the capacity of the embedded KZG trusted setup, counting any pending commit or
decommit alongside the UTxO set) with a `400`. See
[head size limit](/docs/dev/architecture/partial-fanout#head-size-limit).

Note also that the API is not authenticated. Keep `--api-host` on `localhost`,
or put an authenticating proxy in front of it: any party that can reach this
endpoint can side-load, and can also close the head.
