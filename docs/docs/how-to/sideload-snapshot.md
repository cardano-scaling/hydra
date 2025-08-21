---

sidebar_position: 8
---

# Sideload Snapshot

```mdx-code-block
import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';
```

This guide provides a walkthrough on how to use `POST /snapshot` to adopt a given confirmed snapshot to "unstuck" an open `head`.

**Prerequisites**

Ensure you have access to the following repositories:

- `hydra-node`
- `cardano-cli`
- `curl` binaries
- `websocat` binaries

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
  --listen 0.0.0.0:5001 \
  --peer 0.0.0.0:5002 \
  --peer 0.0.0.0:5003 \
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
  --listen 0.0.0.0:5002 \
  --peer 0.0.0.0:5001 \
  --peer 0.0.0.0:5003 \
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
  --listen 0.0.0.0:5003 \
  --peer 0.0.0.0:5001 \
  --peer 0.0.0.0:5002 \
  --hydra-signing-key demo/carol.sk \
  --hydra-verification-key demo/alice.vk \
  --hydra-verification-key demo/bob.vk
```

</TabItem>

</Tabs>
````

Note that Carol uses different protocol parameters.
```shell
curl -s 0.0.0.0:{4001,4002,4003}/protocol-parameters | jq '.txFeeFixed'
```
```shell
0
0
16000
```

> This is intentional to force the head to get stuck upon submitting a NewTx.

## Step 2: Stuck the Head

Now, let's build the transaction we would like to include in the next confirmed snapshot.

To do this, we will prepare a self-transfer from Alice to Alice using `cardano-cli`:

```shell
cardano-cli latest transaction build-raw \
  --tx-in 0000000000000000000000000000000000000000000000000000000000000000#0 \
  --tx-out addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z+100000000 \
  --fee 0 \
  --out-file self-tx.json

cardano-cli latest transaction sign \
  --tx-body-file self-tx.json \
  --signing-key-file hydra-cluster/config/credentials/alice-funds.sk \
  --out-file signed-self-tx.json
```

> Note that the transaction is expected to consume the output from the initial `utxo.json` used to open the head.

Now, let's submit it using `websocat`:

```shell
cat signed-self-tx.json | jq -c '{tag: "NewTx", transaction: .}' | websocat ws://localhost:4001
```

If we look into the latest confirmed snapshot:

```shell
curl -s 0.0.0.0:{4001,4002,4003}/snapshot | jq
```

We will notice every peer is still on `InitialSnapshot` and there is no snapshot in flight.

```shell
curl -s 0.0.0.0:{4001,4002,4003}/snapshot/last-seen | jq
```
```shell
{
  "tag": "NoSeenSnapshot"
}
{
  "tag": "NoSeenSnapshot"
}
{
  "tag": "NoSeenSnapshot"
}
```

The reason is that Carol has rejected the transaction due to her ledger misconfiguration:
```json
{"timestamp":"2025-04-02T14:11:09.622965Z","threadId":27,"namespace":"HydraNode-\"hydra-node-1\"","message":{"node":{"by":{"vkey":"7abcda7de6d883e7570118c1ccc8ee2e911f2e628a41ab0685ffee15f39bba96"},"outcome":{"effects":[],"stateChanges":[{"tag":"TransactionReceived","tx":{"cborHex":"84a300d9010281825820000000000000000000000000000000000000000000000000000000000000000000018182581d6069830961c6af9095b0f2648dff31fa9545d8f0b6623db865eb78fde81a05f5e1000200a100d9010281825820f953b2d6b6f319faa9f8462257eb52ad73e33199c650f0755e279e21882399c05840807ce5a384a4fa69bccd8d2778e9ff4ad568aa8ec11e037fdebb0ce5a9e1495985fa9d6c2783758acd9f9bacfc4d47e8a208398914eba98b0fc1bc63baa08602f5f6","description":"","txId":"5e0cc0a74606a48f6a99bd9793ac84aacb7db9141d4a526532aefe926b0ee589","type":"Tx ConwayEra"}},{"headId":"6f66666c696e652d0001","tag":"TxInvalid","transaction":{"cborHex":"84a300d9010281825820000000000000000000000000000000000000000000000000000000000000000000018182581d6069830961c6af9095b0f2648dff31fa9545d8f0b6623db865eb78fde81a05f5e1000200a100d9010281825820f953b2d6b6f319faa9f8462257eb52ad73e33199c650f0755e279e21882399c05840807ce5a384a4fa69bccd8d2778e9ff4ad568aa8ec11e037fdebb0ce5a9e1495985fa9d6c2783758acd9f9bacfc4d47e8a208398914eba98b0fc1bc63baa08602f5f6","description":"","txId":"5e0cc0a74606a48f6a99bd9793ac84aacb7db9141d4a526532aefe926b0ee589","type":"Tx ConwayEra"},"utxo":{"0000000000000000000000000000000000000000000000000000000000000000#0":{"address":"addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z","datum":null,"datumhash":null,"inlineDatum":null,"inlineDatumRaw":null,"referenceScript":null,"value":{"lovelace":100000000}}},"validationError":{"reason":"ConwayUtxowFailure (UtxoFailure (FeeTooSmallUTxO (Mismatch {mismatchSupplied = Coin 0, mismatchExpected = Coin 16000})))"}}],"tag":"Continue"},"tag":"Node"}}
```

## Step 3: Unstuck the Head

From this point, all participants would reset their last confirmed snapshot using the snapshot side-load API:

```shell
curl -X POST 0.0.0.0:4001/snapshot --data @<(curl 0.0.0.0:4001/snapshot)
curl -X POST 0.0.0.0:4002/snapshot --data @<(curl 0.0.0.0:4002/snapshot)
curl -X POST 0.0.0.0:4003/snapshot --data @<(curl 0.0.0.0:4003/snapshot)
```

This ensures consistency across all parties.

> Remember, for consensus to be reached on L2, everyone must share the same local state view.

If the sideloaded `ConfirmedSnapshot` is adopted by the nodes, we should see the `SnapshotSideLoaded` server output:

```shell
websocat ws://localhost:4001?history=yes
websocat ws://localhost:4002?history=yes
websocat ws://localhost:4003?history=yes
```

This output confirms that the node is ready to continue operating from that position.

> Note that the snapshot leader has been reset, and the ongoing snapshot signing round has been discarded. This means any pending transactions were pruned and must be re-submitted.

Now, before we re-submit the same transaction, we need to fix Carol's node by restarting it with the correct ledger protocol parameters:

```shell
cabal run hydra-node:exe:hydra-node -- --offline-head-seed 0001 --initial-utxo utxo.json \
  --ledger-protocol-parameters hydra-cluster/config/protocol-parameters.json \
  --persistence-dir tmp-sideload/carol \
  --api-port 4003 \
  --listen 0.0.0.0:5003 \
  --peer 0.0.0.0:5001 \
  --peer 0.0.0.0:5002 \
  --hydra-signing-key demo/carol.sk \
  --hydra-verification-key demo/alice.vk \
  --hydra-verification-key demo/bob.vk
```

> Once we do this, we should observe the Head reaching consensus and a new `ConfirmedSnapshot` being signed through the normal protocol workflow.

Let's re-submit the same transaction:
```shell
cat signed-self-tx.json | jq -c '{tag: "NewTx", transaction: .}' | websocat ws://localhost:4001
```

And check if it has been adopted:
```sh
curl -X GET 127.0.0.1:{4001,4002,4003}/snapshot/last-seen | jq
```

We can also check the transaction was included in the latest confirmed snapshot:

```shell
curl -s 0.0.0.0:{4001,4002,4003}/snapshot | jq
```
