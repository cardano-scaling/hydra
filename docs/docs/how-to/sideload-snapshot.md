---
sidebar_position: 7
---

# Sideload snapshot

This guide provides a walkthrough on using `POST /snapshot` to adopt a given confirmed snapshot to an open `head`.

**Prerequisites**

You should have access to the following repositories:

- `hydra-node`
- `hydra-tui`
- `cardano-cli`, and
- `curl` binaries.

## Step 1: Open a Head
Assuming we already have an open Head.


:::info
You could run a local [demo](./../getting-started)
:::

## Step 2: Get the latest confirmed snapshot

At the beginning, we should observe an InitialSnapshot with NoSeenSnapshot confirmed.  
This confirms that the head is fresh and that no L2 snapshots have been multisigned and confirmed yet.

curl -X GET 127.0.0.1:4001/snapshot

```json
{
    "headId": "64956cb50a35eef60b7abf2e3392cc815a50806d8088138e49ea5611",
    "initialUTxO": {
        "7b27f432e04984dc21ee61e8b1539775cd72cc8669f72cf39aebf6d87e35c697#0": {
            "address": "addr_test1vp0yug22dtwaxdcjdvaxr74dthlpunc57cm639578gz7algset3fh",
            "datum": null,
            "datumhash": null,
            "inlineDatum": null,
            "inlineDatumRaw": null,
            "referenceScript": null,
            "value": {
                "lovelace": 50000000
            }
        },
        "c9a5fb7ca6f55f07facefccb7c5d824eed00ce18719d28ec4c4a2e4041e85d97#0": {
            "address": "addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z",
            "datum": null,
            "datumhash": null,
            "inlineDatum": null,
            "inlineDatumRaw": null,
            "referenceScript": null,
            "value": {
                "lovelace": 100000000
            }
        },
        "f0a39560ea80ccc68e8dffb6a4a077c8927811f06c5d9058d0fa2d1a8d047d20#0": {
            "address": "addr_test1vqx5tu4nzz5cuanvac4t9an4djghrx7hkdvjnnhstqm9kegvm6g6c",
            "datum": null,
            "datumhash": null,
            "inlineDatum": null,
            "inlineDatumRaw": null,
            "referenceScript": null,
            "value": {
                "lovelace": 25000000
            }
        }
    },
    "tag": "InitialSnapshot"
}
```

curl -X GET 127.0.0.1:4001/snapshot/last-seen

```json
{
  "tag": "NoSeenSnapshot"
}
```

## Step 3: Understanding ConfirmedSnapshot to sideload

We have two options:

1. Build a custom ConfirmedSnapshot and get it multi-signed.

2. Fetch the latest confirmed snapshot, which is already multi-signed.

For simplicity, let's go with the second and retrieve the latest confirmed snapshot using `GET /snapshot`.

It should return a response like this:

```json
{
    "snapshot": {
        "confirmed": [],
        "headId": "00010000000001010001000101010101",
        "number": 0,
        "utxo": {},
        "utxoToCommit": null,
        "utxoToDecommit": null,
        "version": 1
    },
    "signatures": {
        "multiSignature": []
    },
    "tag": "ConfirmedSnapshot"
}
```

For a snapshot to be accepted:

* Its number must be greater than or equal to the latest seen snapshot.

* It must be valid and enforceable based on the current protocol state on L1, meaning:
  - It aligns with the chain’s rules and history.
  - It is multi-signed by all participants.

All this ensures consensus before the snapshot is considered confirmed.

## Step 4: Prepare the confirmed tx to be included

At this point, we will evolve the InitialSnapshot into a ConfirmedSnapshot.

To achieve this, we will prepare a self-transfer from Alice to Alice using cardano-cli:

```sh
cardano-cli conway transaction build-raw \
  --tx-in c9a5fb7ca6f55f07facefccb7c5d824eed00ce18719d28ec4c4a2e4041e85d97#0 \
  --tx-out addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z+100000000 \
  --fee 0 \
  --out-file self-tx.json
```

That will produce the following tx:

```json
{
    "type": "Unwitnessed Tx ConwayEra",
    "description": "Ledger Cddl Format",
    "cborHex": "84a300d9010281825820c9a5fb7ca6f55f07facefccb7c5d824eed00ce18719d28ec4c4a2e4041e85d9700018182581d6069830961c6af9095b0f2648dff31fa9545d8f0b6623db865eb78fde81a05f5e1000200a0f5f6"
}
```

Finally, it needs to be signed:

```sh
cardano-cli conway transaction sign \
  --tx-body-file self-tx.json \
  --signing-key-file ./devnet/credentials/alice.sk \
  --out-file signed-self-tx.json
```

And will produce the following signed tx:

```json
{
    "type": "Witnessed Tx ConwayEra",
    "description": "Ledger Cddl Format",
    "cborHex": "84a300d9010281825820c9a5fb7ca6f55f07facefccb7c5d824eed00ce18719d28ec4c4a2e4041e85d9700018182581d6069830961c6af9095b0f2648dff31fa9545d8f0b6623db865eb78fde81a05f5e1000200a100d9010281825820eb94e8236e2099357fa499bfbc415968691573f25ec77435b7949f5fdfaa5da058400809e9809a08f5412c326cc5f5aba259e633b458fde47fa52be61975b64af55bfa6c303522d9d784a136abd05aadcadbb52b8abad954cc61605341125a75b009f5f6"
}
```

## Prepare ConfirmedSnapshot from repl
run from root of the project: `cabal repl hydra-node:lib:hydra-node`

```hs
import Hydra.Cardano.Api (Tx, PParams, AsType(AsSigningKey))
import Hydra.Cardano.Api.Prelude (UTxO, LedgerEra, SigningKey, NetworkId(..), NetworkMagic(..))

import Hydra.Chain.ChainState (ChainSlot(..))
import Hydra.Chain.CardanoClient (QueryPoint(..), queryGenesisParameters)
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)

import Hydra.Ledger (Ledger(..))
import Hydra.Ledger.Cardano (Globals, LedgerEnv, newLedgerEnv, cardanoLedger)

import Hydra.Node.Run (getGlobalsForChain, newGlobals)

import Hydra.Tx.Crypto (AsType(AsHydraKey), aggregate, sign, HydraKey)
import Hydra.Tx.HeadId (HeadId, mkHeadId)
import Hydra.Tx.Snapshot (SnapshotNumber, SnapshotVersion, Snapshot(..), ConfirmedSnapshot(..))

import Hydra.Utils (readJsonFileThrow)

-- | assumes we are running the local demo
aliceSk :: SigningKey HydraKey <- readFileTextEnvelopeThrow (AsSigningKey AsHydraKey) "../demo/alice.sk"
bobSk :: SigningKey HydraKey <- readFileTextEnvelopeThrow (AsSigningKey AsHydraKey) "../demo/bob.sk"
carolSk :: SigningKey HydraKey <- readFileTextEnvelopeThrow (AsSigningKey AsHydraKey) "../demo/carol.sk"
pparams :: PParams LedgerEra <- readJsonFileThrow parseJSON "../demo/devnet/protocol-parameters.json"
let ledgerEnv :: LedgerEnv LedgerEra = newLedgerEnv pparams
let networkId = Testnet (NetworkMagic 42)
let nodeSocket = "../demo/devnet/node.socket"
globals <- queryGenesisParameters networkId nodeSocket QueryTip >>= newGlobals
let Ledger{applyTransactions} = cardanoLedger globals ledgerEnv

-- | obtained from GET /snapshot/last-seen
let headId :: HeadId = mkHeadId "64956cb50a35eef60b7abf2e3392cc815a50806d8088138e49ea5611"
let version :: SnapshotVersion = 0
let (utxoToCommit, utxoToDecommit) :: (Maybe UTxO, Maybe UTxO) = (Nothing, Nothing)
let initialUTxO :: UTxO = fromMaybe mempty $ Aeson.decode "{\"7b27f432e04984dc21ee61e8b1539775cd72cc8669f72cf39aebf6d87e35c697#0\":{\"address\":\"addr_test1vp0yug22dtwaxdcjdvaxr74dthlpunc57cm639578gz7algset3fh\",\"datum\":null,\"datumhash\":null,\"inlineDatum\":null,\"inlineDatumRaw\":null,\"referenceScript\":null,\"value\":{\"lovelace\":50000000}},\"c9a5fb7ca6f55f07facefccb7c5d824eed00ce18719d28ec4c4a2e4041e85d97#0\":{\"address\":\"addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z\",\"datum\":null,\"datumhash\":null,\"inlineDatum\":null,\"inlineDatumRaw\":null,\"referenceScript\":null,\"value\":{\"lovelace\":100000000}},\"f0a39560ea80ccc68e8dffb6a4a077c8927811f06c5d9058d0fa2d1a8d047d20#0\":{\"address\":\"addr_test1vqx5tu4nzz5cuanvac4t9an4djghrx7hkdvjnnhstqm9kegvm6g6c\",\"datum\":null,\"datumhash\":null,\"inlineDatum\":null,\"inlineDatumRaw\":null,\"referenceScript\":null,\"value\":{\"lovelace\":25000000}}}"

-- | built using cardano-cli
let confirmed :: [Tx] = maybeToList $ Aeson.decode "{\"type\":\"Witnessed Tx ConwayEra\",\"description\":\"Ledger Cddl Format\",\"cborHex\":\"84a300d9010281825820c9a5fb7ca6f55f07facefccb7c5d824eed00ce18719d28ec4c4a2e4041e85d9700018182581d6069830961c6af9095b0f2648dff31fa9545d8f0b6623db865eb78fde81a05f5e1000200a100d9010281825820eb94e8236e2099357fa499bfbc415968691573f25ec77435b7949f5fdfaa5da058400809e9809a08f5412c326cc5f5aba259e633b458fde47fa52be61975b64af55bfa6c303522d9d784a136abd05aadcadbb52b8abad954cc61605341125a75b009f5f6\"}"

-- | apply the confirmed tx to the initial UTxO to obtain the new confirmed UTxO
let utxo :: UTxO = either (error . show) id $ applyTransactions (ChainSlot 0) initialUTxO confirmed
let number :: SnapshotNumber = 1
let snapshot = Snapshot{headId, version, number, confirmed, utxo, utxoToCommit, utxoToDecommit}

-- | in practice, each member signs the snapshot individually and then shares their signed snapshot with the rest
let signatures = aggregate $ (flip sign $ snapshot) <$> [aliceSk, bobSk, carolSk]

-- | once all signatures are collected, we can build the ConfirmedSnapshot
let confirmedSnapshot = ConfirmedSnapshot{snapshot, signatures}
Aeson.encode confirmedSnapshot
```

## Step 5: Sideload ConfirmedSnapshot

Using the above, coordinate with all participants to submit the same snapshot

> Sideload the encoded ConfirmedSnapshot above:
```sh
curl -X POST 127.0.0.1:4001/snapshot -H "Content-Type: application/json" -d '{"signatures":{"multiSignature":["889b605f57d9233ba93ab38f061b26f0a666e60fb9f7bb5a791fb22c359ae13a8d22833aa4e56dcf3043dc16fa41abdd78b64878df13c8e2e1a1fc7c48c72605","9e6021fcd45efbcb72130eba2e806264f54ada18362eee48b55ce3807473ff1727c2186483160553f240aaafe0bdaca948eed6a3d597ec9aafc4687fa75c270b","25bdda7d72973495d27515646a0f376cb90deb1498d39d4b88beb32a938b142cc63fc9f5aeb65329c7cc54b370742f2950e8180c3abd3924cd00126f2217740c"]},"snapshot":{"confirmed":[{"cborHex":"84a300d9010281825820c9a5fb7ca6f55f07facefccb7c5d824eed00ce18719d28ec4c4a2e4041e85d9700018182581d6069830961c6af9095b0f2648dff31fa9545d8f0b6623db865eb78fde81a05f5e1000200a100d9010281825820f953b2d6b6f319faa9f8462257eb52ad73e33199c650f0755e279e21882399c05840968cc62ac470c4a0589d0309f3bdddeb28920b29711237035f5a6f9e9c674ae0c23442901c9db4c4b60b09ff95bb32fe6f39bfedaf6cf5397e79a2125ad28f02f5f6","description":"","txId":"93d8bb448a4dcd80da5199919a354ba76675139d71dc7e7845acc4d4e3717f64","type":"Tx ConwayEra"}],"headId":"64956cb50a35eef60b7abf2e3392cc815a50806d8088138e49ea5611","number":1,"utxo":{"7b27f432e04984dc21ee61e8b1539775cd72cc8669f72cf39aebf6d87e35c697#0":{"address":"addr_test1vp0yug22dtwaxdcjdvaxr74dthlpunc57cm639578gz7algset3fh","datum":null,"datumhash":null,"inlineDatum":null,"inlineDatumRaw":null,"referenceScript":null,"value":{"lovelace":50000000}},"93d8bb448a4dcd80da5199919a354ba76675139d71dc7e7845acc4d4e3717f64#0":{"address":"addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z","datum":null,"datumhash":null,"inlineDatum":null,"inlineDatumRaw":null,"referenceScript":null,"value":{"lovelace":100000000}},"f0a39560ea80ccc68e8dffb6a4a077c8927811f06c5d9058d0fa2d1a8d047d20#0":{"address":"addr_test1vqx5tu4nzz5cuanvac4t9an4djghrx7hkdvjnnhstqm9kegvm6g6c","datum":null,"datumhash":null,"inlineDatum":null,"inlineDatumRaw":null,"referenceScript":null,"value":{"lovelace":25000000}}},"utxoToCommit":null,"utxoToDecommit":null,"version":0},"tag":"ConfirmedSnapshot"}'
```

This ensures consistency across all parties.

Remember, for consensus to be reached on L2, everyone must share the same local state view.

If the sideloaded ConfirmedSnapshot is adopted by the node, we should see SnapshotConfirmed and SnapshotSideLoaded server outputs.

These outputs confirm that the node is ready to continue operating from that position.

> Check if it has been adopted:
```sh
curl -X GET 127.0.0.1:4001/snapshot/last-seen
```

Notice that the snapshot leader has been reset, and the ongoing snapshot signing round has been discarded. This means any pending transactions were pruned and must be re-submitted.

Once we do this, we should observe the Head reaching consensus and a new ConfirmedSnapshot being signed through the normal protocol workflow.

