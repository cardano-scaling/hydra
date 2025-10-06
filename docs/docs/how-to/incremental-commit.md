---
sidebar_position: 5
---

# Deposit funds to an open Head

Assuming we already have an open Head and some funds on the L1 we would like to deposit.


The `/commit` endpoint supports two ways of specifying what to deposit, one is just by showing the UTxO (which is assumed to be owned by public keys), while the more advanced way would be using _blueprint_ transaction as a recipe on what to deposit.

Blueprint transactions work differently in deposits (incremental commits) compared to commits (committing before the Head is in the open state).

When it comes to **depositing** our users have four ways of using the blueprint transaction so it should work for even more involved scenarios:

1. Only provide the UTxO to deposit - simple deposit where whole of the UTxO is deposited to the Head.
2. Provide UTxO and a bluerprint transaction **without** any outputs - whole of the UTxO will be deposited but users get to specify additional transaction context
needed for dApps.
3. Provide UTxO, blueprint transaction **with** specified outputs but **NO** change address is provided - In this case we expect to see fully balanced blueprint transaction where it's outputs will be deposited completely.
4. Provide UTxO, blueprint transaction **with** specified outputs **AND** a **change address** - hydra-node will **balance** the deposit transaction and give back any change to the provided change address. Provided outputs will be deposited.


:::info
Hydra node assumes that the blueprint transaction does not contain any stake pool or DRep registration or delegation certificates since these don't make too much sense on L2 network.
:::

The last option is most flexible one for dApp builders since they can just specify which UTxO assets they want to deposit (providing the appropriate outputs in the blueprint tx) while the hydra-node will make sure to return any change to the specified address. Using script UTxO as a reference inputs is also possible for optimising the dApp workflow.


<details>
 <summary>Partial deposit example: </summary>

   If there exists a user UTxO that looks like this

   ```json

      {
         "98ce7d553c87837735ce99d0452d6c78c14499a35a2ebe78a61653da422ba06b#0":
            { "address":"addr_test1vznxtg0ljhlqghzr9sdw3stfffna97l6xuaradgdk8jnhegyza948",
              "datum":null,
              "datumhash":null,
              "inlineDatum":null,
              "inlineDatumRaw":null,
              "referenceScript":null,
              "value":
                  { "dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75":
                       { "32":6459586778467727683,
                         "38":4726846541838542289,
                         "58cfd525f109":2,
                         "60c477033b55e1":6560058562233977644,
                         "768b8dfd46f4d5b14873b4c3851f2c82d0301f61661347":4661096764720222855,
                         "ab5f4e6486d24ad6744e54f95dd1bc4dbbe721":3250906605877226653,
                         "d47f9b":1,
                         "d6cc1ed8ac3cda":2,
                         "f26fe54468b98902751e8d54553c1878b5f952d486":1
                       },
                     "lovelace":30000000
                  }
            }
      }

   ```
   Then the user can decide to commit some amount of lovelace and assets while the _change_ would be given back to the origin address.
   In order to do that they can send a http POST request to the `/commit` endpoint
   providing the blueprint transaction with specified outputs, UTxO and a change address:

   ```json

      {
        "blueprintTx": {
          "cborHex": "84a300d901028182582098ce7d553c87837735ce99d0452d6c78c14499a35a2ebe78a61653da422ba06b00018182581d60a665a1ff95fe045c432c1ae8c1694a67d2fbfa373a3eb50db1e53be5821a00e4e1c0a1581cdcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75a44658cfd525f109024760c477033b55e11b5b0a028173b74b2c53ab5f4e6486d24ad6744e54f95dd1bc4dbbe7211b2d1d8a5581486c9d55f26fe54468b98902751e8d54553c1878b5f952d486010200a0f5f6",
          "description": "",
          "txId": "8451c42d1a1b48fdb1b1d2b014dc700cbba15b3c20addb2076feeceeeb3d22a4",
          "type": "Tx ConwayEra"
        },
        "changeAddress": "addr_test1vznxtg0ljhlqghzr9sdw3stfffna97l6xuaradgdk8jnhegyza948",
        "utxo": {
          "98ce7d553c87837735ce99d0452d6c78c14499a35a2ebe78a61653da422ba06b#0": {
            "address": "addr_test1vznxtg0ljhlqghzr9sdw3stfffna97l6xuaradgdk8jnhegyza948",
            "datum": null,
            "datumhash": null,
            "inlineDatum": null,
            "inlineDatumRaw": null,
            "referenceScript": null,
            "value": {
              "dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75": {
                "32": 6459586778467727000,
                "38": 4726846541838542000,
                "58cfd525f109": 2,
                "60c477033b55e1": 6560058562233978000,
                "768b8dfd46f4d5b14873b4c3851f2c82d0301f61661347": 4661096764720223000,
                "ab5f4e6486d24ad6744e54f95dd1bc4dbbe721": 3250906605877226500,
                "d47f9b": 1,
                "d6cc1ed8ac3cda": 2,
                "f26fe54468b98902751e8d54553c1878b5f952d486": 1
              },
              "lovelace": 30000000
            }
          }
        }
      }

   ```
   In the provided blueprint tx we specified the outputs we want to deposit. Rendered blueprint tx looks like this:

   ```
   "8451c42d1a1b48fdb1b1d2b014dc700cbba15b3c20addb2076feeceeeb3d22a4"

     == INPUTS (1)
     - 98ce7d553c87837735ce99d0452d6c78c14499a35a2ebe78a61653da422ba06b#0
           ShelleyAddress Testnet (KeyHashObj (KeyHash {unKeyHash = "a665a1ff95fe045c432c1ae8c1694a67d2fbfa373a3eb50db1e53be5"})) StakeRefNull
           30000000 lovelace
           6459586778467727683 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.32
           4726846541838542289 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.38
           2 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.58cfd525f109
           6560058562233977644 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.60c477033b55e1
           4661096764720222855 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.768b8dfd46f4d5b14873b4c3851f2c82d0301f61661347
           3250906605877226653 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.ab5f4e6486d24ad6744e54f95dd1bc4dbbe721
           1 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.d47f9b
           2 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.d6cc1ed8ac3cda
           1 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.f26fe54468b98902751e8d54553c1878b5f952d486
           TxOutDatumNone
           ReferenceScriptNone

     == COLLATERAL INPUTS (0)

     == REFERENCE INPUTS (0)

     == OUTPUTS (1)
     Total number of assets: 5
     - ShelleyAddress Testnet (KeyHashObj (KeyHash {unKeyHash = "a665a1ff95fe045c432c1ae8c1694a67d2fbfa373a3eb50db1e53be5"})) StakeRefNull
           15000000 lovelace
           2 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.58cfd525f109
           6560058562233977644 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.60c477033b55e1
           3250906605877226653 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.ab5f4e6486d24ad6744e54f95dd1bc4dbbe721
           1 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.f26fe54468b98902751e8d54553c1878b5f952d486
           TxOutDatumNone

     == TOTAL COLLATERAL
     TxTotalCollateralNone

     == RETURN COLLATERAL
     TxReturnCollateralNone

     == FEE
     TxFeeExplicit ShelleyBasedEraConway (Coin 0)

     == VALIDITY
     TxValidityNoLowerBound
     TxValidityUpperBound ShelleyBasedEraConway Nothing

     == MINT/BURN
     0 lovelace

     == SCRIPTS (0)
     Total size (bytes):  0

     == DATUMS (0)

     == REDEEMERS (0)

     == REQUIRED SIGNERS
     []

     == METADATA
     TxMetadataNone

   ```
   You can see that the UTxO contains the following assets present in the blueprint input:


   ```json
           30000000 lovelace
           6459586778467727683 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.32
           4726846541838542289 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.38
           2 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.58cfd525f109
           6560058562233977644 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.60c477033b55e1
           4661096764720222855 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.768b8dfd46f4d5b14873b4c3851f2c82d0301f61661347
           3250906605877226653 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.ab5f4e6486d24ad6744e54f95dd1bc4dbbe721
           1 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.d47f9b
           2 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.d6cc1ed8ac3cda
           1 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.f26fe54468b98902751e8d54553c1878b5f952d486

   ```

   But we decided to deposit some of them which we specified in the single blueprint output:

   ```json

           15000000 lovelace
           2 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.58cfd525f109
           6560058562233977644 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.60c477033b55e1
           3250906605877226653 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.ab5f4e6486d24ad6744e54f95dd1bc4dbbe721
           1 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.f26fe54468b98902751e8d54553c1878b5f952d486
   ```

   Hydra node returns a deposit transaction which then needs to be signed and submitted to the network. Deposit looks like this:

   ```

    "ab4adce53699015ca8fd112689906338278f435d436516290c68c679921de8a4"

    == INPUTS (2)
    - 04e36b0eeb5d97be19e543583e3b736640b45ceaf5ece1d37f5db9ac88143f20#1
    - 98ce7d553c87837735ce99d0452d6c78c14499a35a2ebe78a61653da422ba06b#0
          ShelleyAddress Testnet (KeyHashObj (KeyHash {unKeyHash = "a665a1ff95fe045c432c1ae8c1694a67d2fbfa373a3eb50db1e53be5"})) StakeRefNull
          30000000 lovelace
          6459586778467727683 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.32
          4726846541838542289 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.38
          2 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.58cfd525f109
          6560058562233977644 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.60c477033b55e1
          4661096764720222855 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.768b8dfd46f4d5b14873b4c3851f2c82d0301f61661347
          3250906605877226653 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.ab5f4e6486d24ad6744e54f95dd1bc4dbbe721
          1 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.d47f9b
          2 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.d6cc1ed8ac3cda
          1 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.f26fe54468b98902751e8d54553c1878b5f952d486
          TxOutDatumNone
          ReferenceScriptNone

    == COLLATERAL INPUTS (1)
    - 04e36b0eeb5d97be19e543583e3b736640b45ceaf5ece1d37f5db9ac88143f20#1

    == REFERENCE INPUTS (0)

    == OUTPUTS (3)
    Total number of assets: 10
    - ShelleyAddress Testnet (ScriptHashObj (ScriptHash "ae01dade3a9c346d5c93ae3ce339412b90a0b8f83f94ec6baa24e30c")) StakeRefNull
          15000000 lovelace
          2 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.58cfd525f109
          6560058562233977644 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.60c477033b55e1
          3250906605877226653 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.ab5f4e6486d24ad6744e54f95dd1bc4dbbe721
          1 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.f26fe54468b98902751e8d54553c1878b5f952d486
          TxOutDatumInline [0,["0xe9e21b6e4f59b3c62721c99109e76abb46e018e4fc184b16e58d650a",1759768581709,[[0,[[0,["0x8451c42d1a1b48fdb1b1d2b014dc700cbba15b3c20addb2076feeceeeb3d22a4",0]],"0xd8799fd8799fd8799f581ca665a1ff95fe045c432c1ae8c1694a67d2fbfa373a3eb50db1e53be5ffd87a80ffa240a1401a00e4e1c0581cdcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75a44658cfd525f109024760c477033b55e11b5b0a028173b74b2c53ab5f4e6486d24ad6744e54f95dd1bc4dbbe7211b2d1d8a5581486c9d55f26fe54468b98902751e8d54553c1878b5f9
    52d48601d87980d87a80ff"]]]]]
    - ShelleyAddress Testnet (KeyHashObj (KeyHash {unKeyHash = "a665a1ff95fe045c432c1ae8c1694a67d2fbfa373a3eb50db1e53be5"})) StakeRefNull
          15000000 lovelace
          6459586778467727683 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.32
          4726846541838542289 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.38
          4661096764720222855 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.768b8dfd46f4d5b14873b4c3851f2c82d0301f61661347
          1 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.d47f9b
          2 dcf5fdd1d01c04b0e6262bba173a89c4b81b6570211f08bc059c8a75.d6cc1ed8ac3cda
          TxOutDatumNone
    - ShelleyAddress Testnet (KeyHashObj (KeyHash {unKeyHash = "f8a68cd18e59a6ace848155a0e967af64f4d00cf8acee8adc95a6b0d"})) StakeRefNull
          21583379 lovelace
          TxOutDatumNone

    == TOTAL COLLATERAL
    TxTotalCollateralNone

    == RETURN COLLATERAL
    TxReturnCollateralNone

    == FEE
    TxFeeExplicit ShelleyBasedEraConway (Coin 203737)

    == VALIDITY
    TxValidityNoLowerBound
    TxValidityUpperBound ShelleyBasedEraConway (Just (SlotNo 37))

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

   If you take a look at the outputs of this deposit tx you will see that we only locked specified ADA amount + tokens at the deposit address and gave back leftover ADA and assets the provided change address.

</details>

This will result in a deposit being detected by the `hydra-node` and consequently the funds to be deposited to the Head.

### Recover a deposit

Once we deposited funds we should not see the corresponding UTxO on L1.


To inspect the pending deposits you can use `curl`:

```
curl -X GET localhost:4001/commits

```
You should see the tx-id of the deposit transaction in case it wasn't picked up by the hydra-node already. (e.g.`["ab4adce53699015ca8fd112689906338278f435d436516290c68c679921de8a4"]`

To recover, we can use the `/commits` endpoint again using the transaction id of the deposit:

```shell
curl -X DELETE localhost:4001/commits/$(printf "\"ab4adce53699015ca8fd112689906338278f435d436516290c68c679921de8a4"\" | jq -sRr '@uri')
```

If we inspect the address funds again we will see that the locked deposit is now recovered. It is important to mention that recovering works even if the Head is closed already.


