---
sidebar_position: 4
---

# Use the withdraw-zero trick

A zero-value withdrawal runs a reward script once for the transaction. Hydra
supports this pattern by creating a zero-balance reward account during ledger
validation. See [CIP-112](https://cips.cardano.org/cip/CIP-0112) for the motivation
for transaction-level validation.

Run this example from the Hydra repository root in a development shell with
`cabal`, `cardano-cli`, `curl`, `jq`, and `websocat`. It uses demo keys and an offline head.

## Rewarding script

Use [`dummyRewardingScript`](https://github.com/cardano-scaling/hydra/blob/master/hydra-plutus/src/Hydra/Contract/Dummy.hs),
which accepts rewarding and certifying contexts. Start GHCi:

```shell
cabal repl hydra-plutus:lib:hydra-plutus
```

Export the script, then exit GHCi. Cabal runs this session in `hydra-plutus/`,
so `../rewarding.plutus` writes to the repository root:

```haskell
import Prelude
import Hydra.Contract.Dummy
import Hydra.Cardano.Api
let script = PlutusScript dummyRewardingScript
writeFileTextEnvelope (File "../rewarding.plutus") Nothing script
:quit
```

Back in the shell, derive the address from the exported script. This avoids
hard-coding a hash that changes when the script is recompiled:

```shell
STAKE_ADDR=$(cardano-cli conway stake-address build \
  --stake-script-file rewarding.plutus --testnet-magic 42)
```

## Withdraw zero transaction

Create an [offline head](../configuration#offline-mode) containing 100 ADA owned by Alice:

<details>
  <summary>Setup with offline head</summary>

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

```shell
cabal run hydra-node:exe:hydra-node -- \
  --offline-head-seed 0001 \
  --initial-utxo utxo.json \
  --ledger-protocol-parameters hydra-cluster/config/protocol-parameters.json \
  --hydra-signing-key demo/alice.sk \
  --persistence-dir tmp-withdraw-zero
```
</details>

Keep the node running in that terminal. In another terminal at the repository
root, set `STAKE_ADDR` as above. Offline mode deposits the supplied UTxO after
opening the head. Wait for `CommitFinalized` before spending it:

```shell
websocat "ws://127.0.0.1:4001?history=yes"
```

Stop the monitor with Ctrl-C once the event appears.

Use a fresh `tmp-withdraw-zero/` directory for each run. Build, sign, and
submit the transaction once; it spends the initial output:

```shell title="Withdraw zero transaction"
cardano-cli conway transaction build-raw \
  --tx-in 0000000000000000000000000000000000000000000000000000000000000000#0 \
  --tx-in-collateral 0000000000000000000000000000000000000000000000000000000000000000#0 \
  --tx-out addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z+100000000 \
  --withdrawal "$STAKE_ADDR+0" \
  --withdrawal-script-file rewarding.plutus \
  --withdrawal-redeemer-value "{}" \
  --withdrawal-execution-units "(10000000000, 14000000)" \
  --protocol-params-file hydra-cluster/config/protocol-parameters.json \
  --fee 0 \
  --out-file tx.json

cardano-cli conway transaction sign \
  --tx-body-file tx.json \
  --signing-key-file hydra-cluster/config/credentials/alice-funds.sk \
  --out-file tx-signed.json

curl --fail-with-body -sS http://127.0.0.1:4001/transaction \
  -H 'Content-Type: application/json' \
  --data-binary @tx-signed.json
```

An HTTP `200` response confirms inclusion in a snapshot. A `202` response means
the transaction is still pending; check the snapshot or WebSocket events.

## Registration is not required

The reward account does not need a registration certificate for this pattern.
Hydra creates the account for each zero-value withdrawal during validation;
it does not maintain persistent staking or reward-account state between
transactions.
