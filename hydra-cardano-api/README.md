# Hydra Cardano API

A Haskell API for Cardano, tailored to the Hydra project. This package provides a wrapper around the cardano-ledger, cardano-api and plutus libraries with extra utilities and function commonly used across the Hydra project. 

Some of those addition may be likely candidates for upstream change requests, but having this extra space gives us an opportunity to iterate faster and to unify not-always-consistent names / approches across the Cardano ecosystem.

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

4. Getters are prefix with `get` unless they have an equivalent in cardano-api, for which we adopt cardano-api's naming convention as much as possible. 

5. ... to be continued.
