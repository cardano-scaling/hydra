---
sidebar_position: 4
---

# Use withdraw zero trick

In this how-to we want to show how you can use the "wihtdraw zero trick" in Hydra. Until [CIP-112](https://cips.cardano.org/cip/CIP-0112) is implemented, there is only one way to run a script once per transaction - the so-called withdraw zero trick. Refer to the CIP or [this design pattern document](https://github.com/Anastasia-Labs/design-patterns/blob/main/stake-validator/STAKE-VALIDATOR.md) for more details about the general approach.

The Hydra L2 ledger is mostly _isomorphic_ to the L1 Cardano ledger. This means that it honors all the same ledger rules and transactions use the exact same format. However, some things related to the Cardano Proof-of-Stake consensus are represented different because they would not make much sense on an L2. For example, transactions that [register stake certificates](https://docs.cardano.org/developer-resources/transaction-tutorials/stake-transaction) or [withdraw rewards](https://docs.cardano.org/developer-resources/transaction-tutorials/withdraw-transaction) don't work as-is on Hydra. However, for the purpose of "transaction-level validation", the withdraw zero trick can still be used as the Hydra L2 ledger **mocks script reward accounts** on the fly.

## Rewarding script

We are going to use the [dummyRewardingScript](https://github.com/cardano-scaling/hydra/blob/master/hydra-plutus/src/Hydra/Contract/Dummy.hs#L42-L53) as an example, which only validates the fact that we are in a `RewardingScript` context. Using GHCI we can determine the stake address and write the `rewarding.plutus` text envelope file for `cardano-cli`:

```haskell
cabal repl hydra-plutus
> import Prelude
> import Hydra.Contract.Dummy
> import Hydra.Cardano.Api
> script = PlutusScript dummyRewardingScript
> serialiseAddress $ makeStakeAddress (Testnet $ NetworkMagic 42) (StakeCredentialByScript $ hashScript script)
"stake_test17rekjamvnjyn3c3tcjpxe7ea20g7aek9vdqkaa3jefknz3gc066pt"
> writeFileTextEnvelope (File "../rewarding.plutus") Nothing script
Right ()
```

## Withdraw zero transaction

We can construct a transaction that withdraws zero lovelace from the rewarding script's "account". Usually, other purposes like spending a UTxO would require the reward script to run, but here we just assume there exists such a UTxO in the L2 state. You can initialize an [offline head](../configuration#offline-mode) for an easy demo set-up:

<details>
  <summary>Setup with offline head</summary>
```
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
  --hydra-signing-key demo/alice.sk
```
</details>

Then, build, sign and submit the transaction that runs the rewarding script:

```shell title="Withdraw zero transaction"
cardano-cli latest transaction build-raw \
  --tx-in 0000000000000000000000000000000000000000000000000000000000000000#0 \
  --tx-in-collateral 0000000000000000000000000000000000000000000000000000000000000000#0 \
  --tx-out addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z+100000000 \
  --withdrawal stake_test17zadf9dcekqn9cxkg3q68y56f4d6ujxsv8l6kq4kz2cuduc3gm35e+0 \
  --withdrawal-script-file rewarding.plutus \
  --withdrawal-redeemer-value "{}" \
  --withdrawal-execution-units "(10000000000, 14000000)" \
  --protocol-params-file hydra-cluster/config/protocol-parameters.json \
  --fee 0 \
  --out-file tx.json

cardano-cli latest transaction sign \
  --tx-body-file tx.json \
  --signing-key-file hydra-cluster/config/credentials/alice-funds.sk \
  --out-file tx-signed.json

cat tx-signed.json | jq -c '{tag: "NewTx", transaction: .}' | websocat ws://localhost:4001
```

## Registering script - not needed!

You might have noticed that registering the stake address was not needed - this is different than on the Cardano L1!

While you don't need to do it, you can still submit a transaction that registers the stake address and the L2 ledger will just ignore it:

```shell title="Register stake transaction"
cardano-cli latest stake-address registration-certificate \
  --stake-address stake_test17zadf9dcekqn9cxkg3q68y56f4d6ujxsv8l6kq4kz2cuduc3gm35e \
  --key-reg-deposit-amt 2000000 \
  --out-file reg.cert

cardano-cli latest transaction build-raw \
  --tx-in 0000000000000000000000000000000000000000000000000000000000000000#0 \
  --tx-in-collateral 0000000000000000000000000000000000000000000000000000000000000000#0 \
  --tx-out addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z+98000000 \
  --certificate reg.cert \
  --certificate-script-file rewarding.plutus \
  --certificate-redeemer-value "{}" \
  --certificate-execution-units "(10000000000, 14000000)" \
  --protocol-params-file hydra-cluster/config/protocol-parameters.json \
  --fee 0 \
  --out-file tx.json

cardano-cli latest transaction sign \
  --tx-body-file tx.json \
  --signing-key-file hydra-cluster/config/credentials/alice-funds.sk \
  --out-file tx-signed.json

cat tx-signed.json | jq -c '{tag: "NewTx", transaction: .}' | websocat ws://localhost:4001
```
