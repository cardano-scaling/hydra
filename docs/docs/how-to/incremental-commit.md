---
sidebar_position: 5
---

# Commit funds to an open Head

Assuming we already have an open Head and some funds on the L1 we would like to commit.


:::info
You could run a local [demo](./../getting-started)
:::

The following commands are expected to be run from the `demo` folder and using these environment variables:

```shell
export CARDANO_NODE_SOCKET_PATH=${PWD}/devnet/node.socket
export CARDANO_NODE_NETWORK_ID=42
```

We can inspect the L1 utxo with:

```shell
cardano-cli query utxo --whole-utxo
```
and the state of the faucet public key

```shell
cardano-cli query utxo \
  --address $(cardano-cli address build --payment-verification-key-file ${PWD}/../hydra-cluster/config/credentials/faucet.vk)
```


In this setup, we would be using the `faucet` keys to commit everything into the head.

```shell
export WALLET_SK=${PWD}/../hydra-cluster/config/credentials/faucet.sk
export WALLET_VK=${PWD}/../hydra-cluster/config/credentials/faucet.vk
```

### Deposit UTxO to commit

The `/commit` endpoint supports two ways of specifying what to commit, one is just by showing the UTxO (which is assumed to be owned by public keys), while the more advanced way would be using [blueprint transactions](./commit-blueprint).

We are using the simple request here and want to commit everything owned by `${WALLET_SK}`. So we can just query the L1 for all UTxO owned by it:
```shell
cardano-cli query utxo \
  --address $(cardano-cli address build --payment-verification-key-file ${WALLET_VK}) \
  --out-file commit-utxo.json
```

:::info
You can also specify `amount` of lovelace you want to commit as well as `assets` together with the `UTxO` and hydra-node would
commit only the specified amount of lovelace or any potential non ADA assets and return any leftover to the user address.
:::

:::danger
This functionality should be treated as **experimental** for the time being until we see some user reports that this API is
working well and is easy to use.
:::

<details>
 <summary>Partial commit example: </summary>

   If there exists a user UTxO that looks like this

   ```json
   {"0bdf069df8fa1084989a7bda419c900810fd5d5a72a95f7ab487f96df9052fb8#0":
     { "address":"addr_test1vq7j5vf74jw779y6ssxk2rwart5mltr2r7ju4gtfc3kcawcy0v2v8",
       "datum":null,
       "datumhash":null,
       "inlineDatum":null,
       "inlineDatumRaw":null,
       "referenceScript":null,
       "value":
          { "dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75":
              { "34":2007797983096461947,
                "7041ef64f4476c6f6d8a3c74b7":4866145859671050979,
                "b7a7b64d585f973e82be6ca36ae948":2,
                "beb7e61cb67dd301":2
              },
            "lovelace":5000000
          }
     }
   }

   ```
   Then the user can decide to commit some amount of lovelace and assets while the _change_ would be given back to the origin address.
   In order to do that they can send a http POST request to the `/commit` endpoint
   specifying the _amount_ of ADA and map of _tokens_ with quantities to commit:

   ```json

   {
     "amount":3000000,
     "tokens":
        [
          [ "dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75",
               { "34":2007797983096461947,
                 "7041ef64f4476c6f6d8a3c74b7":4866145859671050979
               }
          ]
        ],
     "utxoToCommit":
        { "0bdf069df8fa1084989a7bda419c900810fd5d5a72a95f7ab487f96df9052fb8#0":
             { "address":"addr_test1vq7j5vf74jw779y6ssxk2rwart5mltr2r7ju4gtfc3kcawcy0v2v8",
               "datum":null,
               "datumhash":null,
               "inlineDatum":null,
               "inlineDatumRaw":null,
               "referenceScript":null,
               "value":
                   { "dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75":
                        { "34":2007797983096461947,
                          "7041ef64f4476c6f6d8a3c74b7":4866145859671050979,
                          "b7a7b64d585f973e82be6ca36ae948":2,
                          "beb7e61cb67dd301":2
                        },
                     "lovelace":5000000
                   }
             }
        }
   }

   ```
   Hydra node returns a deposit transaction which then needs to be signed and submitted to the network:

   ```

   "d22e92621c1b66d947eff97887feb19c1fb5751ecb7440cd6749ac8d734af04f"

   == INPUTS (2)
   - 0bdf069df8fa1084989a7bda419c900810fd5d5a72a95f7ab487f96df9052fb8#0
         ShelleyAddress Testnet (KeyHashObj (KeyHash {unKeyHash = "3d2a313eac9def149a840d650ddd1ae9bfac6a1fa5caa169c46d8ebb"})) StakeRefNull
         5000000 lovelace
         2007797983096461947 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.34
         4866145859671050979 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.7041ef64f4476c6f6d8a3c74b7
         2 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.b7a7b64d585f973e82be6ca36ae948
         2 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.beb7e61cb67dd301
         TxOutDatumNone
         ReferenceScriptNone
   - 799028d0132ec355d6870704e040b73dbab67dba40313cb4f5dbc42d0e66bd40#1

   == COLLATERAL INPUTS (1)
   - 799028d0132ec355d6870704e040b73dbab67dba40313cb4f5dbc42d0e66bd40#1

   == REFERENCE INPUTS (0)

   == OUTPUTS (3)
   Total number of assets: 5
   - ShelleyAddress Testnet (ScriptHashObj (ScriptHash "ae01dade3a9c346d5c93ae3ce339412b90a0b8f83f94ec6baa24e30c")) StakeRefNull
         3000000 lovelace
         2007797983096461947 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.34
         4866145859671050979 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.7041ef64f4476c6f6d8a3c74b7
         TxOutDatumInline [0,["0x3fbb97e02ed6f34def7b87d9aaff67b4fe707f338d72bc2a7f93a409",1757516160812,[[0,[[0,["0x0bdf069df8fa1084989a7bda419c900810fd5d5a72a95f7ab487f96df9052fb8",0]],"0xd8799fd8799fd8799f581c3d2a313eac9def149a840d650ddd1ae9bfac6a1fa5caa169c46d8ebbffd87a80ffa240a1401a002dc6c0581cdcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75a241341b1bdd21a0bb35da7b4d7041ef64f4476c6f6d8a3c74b71b438805e09eee4ae3d87980d87a80ff"]]]]]
   - ShelleyAddress Testnet (KeyHashObj (KeyHash {unKeyHash = "3d2a313eac9def149a840d650ddd1ae9bfac6a1fa5caa169c46d8ebb"})) StakeRefNull
         2000000 lovelace
         2 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.b7a7b64d585f973e82be6ca36ae948
         2 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.beb7e61cb67dd301
         TxOutDatumNone
   - ShelleyAddress Testnet (KeyHashObj (KeyHash {unKeyHash = "f8a68cd18e59a6ace848155a0e967af64f4d00cf8acee8adc95a6b0d"})) StakeRefNull
         19625359 lovelace
         TxOutDatumNone

   == TOTAL COLLATERAL
   TxTotalCollateralNone

   == RETURN COLLATERAL
   TxReturnCollateralNone

   == FEE
   TxFeeExplicit ShelleyBasedEraConway (Coin 197973)

   == VALIDITY
   TxValidityNoLowerBound
   TxValidityUpperBound ShelleyBasedEraConway (Just (SlotNo 38))

   == MINT/BURN
   0 lovelace

   == SCRIPTS (0)
   Total size (bytes):  0

   == DATUMS (0)

   == REDEEMERS (0)

   == REQUIRED SIGNERS
   []

   == METADATA
   TxMetadataInEra ShelleyBasedEraConway (TxMetadata {unTxMetadata = fromList [(55555,TxMetaText "HydraV1/DepositTx")]})
   ```

   If you take a look at the outputs you will see that we only locked specified ADA amount + tokens at the deposit address and gave back any leftover to the
   user address.

</details>

Then a request to the `/commit` endpoint provides us with a transaction:

```shell
curl -X POST localhost:4001/commit \
  --data @commit-utxo.json \
  > deposit-tx.json
```

Which we can submit to the cardano network:
```shell
cardano-cli conway transaction sign \
  --tx-file deposit-tx.json \
  --signing-key-file ${WALLET_SK} \
  --out-file deposit-tx.signed.json

cardano-cli conway transaction submit \
  --tx-file deposit-tx.signed.json
```

This will result in a deposit being detected by the `hydra-node` and consequently the funds to be committed to the head.

### Recover a deposit

Do the same thing as above, **but** with one node stopped, so the deposit is not going to be picked up.

Once we deposited funds we should not see the corresponding UTxO belonging to faucet public key on L1:

```shell

cardano-cli query utxo \
  --address $(cardano-cli address build --payment-verification-key-file ${PWD}/../hydra-cluster/config/credentials/faucet.vk)
```


Inspect the pending deposits:

```
curl -X GET localhost:4001/commits

```
and you should see the tx-id of the deposit transaction `["6b51f3787f5482004b258c60fe0c94775164f547d9284b6233bbb4f6f8b9dfa6"]`

To recover, we can use the `/commits` endpoint again using the transaction id of the deposit:

```shell
curl -X DELETE localhost:4001/commits/$(printf "\"6b51f3787f5482004b258c60fe0c94775164f547d9284b6233bbb4f6f8b9dfa6"\" | jq -sRr '@uri')
```

If we inspect the faucet funds again we will see that the locked deposit is now recovered

```shell
cardano-cli query utxo \
  --address $(cardano-cli address build --payment-verification-key-file ${PWD}/../hydra-cluster/config/credentials/faucet.vk)
```

