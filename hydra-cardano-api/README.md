# Hydra Cardano API

A Haskell API for Cardano, tailored to the Hydra project. This package provides
a wrapper around the cardano-ledger, cardano-api and plutus libraries with extra
utilities and function commonly used across the Hydra project.

Some of those addition may be likely candidates for upstream change requests,
but having this extra space gives us an opportunity to iterate faster and to
unify not-always-consistent names / approaches across the Cardano ecosystem.

In addition, the top-level module `Hydra.Cardano.Api` does re-export only
specialized types and constructors for all underlying types. Everything is
indeed specialized to the **latest era** (e.g. `Babbage`) and **latest plutus
version** (e.g. `PlutusV2`) making the era type parameter redundant in most
cases. This also removes the need for extra indirections or, for proxy-values
witnessing era types as present in many `cardano-api` constructors. For example,
a vanilla usage of the `cardano-api` would looks like the followings:

```hs
changeOutput :: Address ShelleyAddr -> TxOut CtxUTxO BabbageEra
changeOutput =
  TxOut
    (AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) addr)
    (TxOutValue MultiAssetInBabbageEra (lovelaceToValue $ initialAmount - amount - fee))
    (TxOutDatumHash ScriptDataInBabbageEra (hashScriptData $ fromPlutusData someDatum))
```

...whereas, the wrapped API would offer a more lightweight notation:

```hs
changeOutput :: Address ShelleyAddr -> TxOut CtxUTxO
changeOutput =
  TxOut
    (AddressInEra addr)
    (lovelaceToValue $ initialAmount - amount - fee)
    (TxOutDatumHash $ hashScriptData $ fromPlutusData someDatum)
```

## Organization & naming conventions

This package follows some simple rules to remain tidy and organized: 

1. Modules are scoped to only data-type, and named after it. Examples:
    - `Hydra.Cardano.Api.AddressInEra` / `AddressInEra`
    - `Hydra.Cardano.Api.TxIn` / `TxIn`

2. Functions are placed in a module if their _return type_ matches the module's scope. 

    ```hs
    mkVkAddress 
      :: IsShelleyBasedEra era 
      => NetworkId
      -> VerificationKey PaymentKey
      -> AddressInEra era
    ```

    belongs to `Hydra.Cardano.Api.AddressInEra` but: 

    ```hs
    getPaymentScriptHash ::
      AddressInEra AlonzoEra ->
      Maybe ScriptHash
    ```

    belongs to `Hydra.Cardano.Api.ScriptHash`.

    > NOTE: 3. For conversions functions to ledger / plutus types for which the return type is isomorphic to the module's scope, we place them in that same module. 

3. Builders are prefix with `mk` (as in 'make').

4. Getters are prefix with `get` unless they have an equivalent in `cardano-api`, for which we adopt `cardano-api`'s naming convention as much as possible. 

5. Functions outside of the top-level `Hydra.Cardano.Api` are unspecialized and written for _any era_ (where it does make sense!). Said differently, it should be easy to move those functions upstream if need be. Specialization only happens at the very frontier, in the top-level module.. 
