{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | Contract for Hydra controlling the redemption of participation tokens.
module Hydra.Contract.Initial where

import Ledger hiding (validatorHash)
import PlutusTx.Prelude

import qualified Hydra.Contract.Commit as Commit
import Hydra.Contract.Head (Head, Input (..))
import Hydra.OnChain.Util (findUtxo, mkParty, mustRunContract)
import Ledger.Constraints (checkScriptContext)
import Ledger.Constraints.TxConstraints (
  TxConstraints,
  mustBeSignedBy,
  mustPayToOtherScript,
  mustSpendPubKeyOutput,
 )
import Ledger.Typed.Scripts (TypedValidator, ValidatorType, ValidatorTypes (..))
import qualified Ledger.Typed.Scripts as Scripts
import PlutusTx (CompiledCode)
import qualified PlutusTx
import PlutusTx.IsData.Class (ToData (..))

data Initial

instance Scripts.ValidatorTypes Initial where
  type DatumType Initial = (MintingPolicyHash, Dependencies, PubKeyHash)
  type RedeemerType Initial = TxOutRef

-- TODO: We should be able to get rid of this in principle and inject them
-- directly at compile-time (since they are statically known). Somehow, this
-- doesn't work out of the box with the Plutus plugin at the moment and we
-- resort to inject them 'manually'.
data Dependencies = Dependencies
  { headScript :: ValidatorHash
  , commitScript :: ValidatorHash
  }

PlutusTx.makeLift ''Dependencies
PlutusTx.unstableMakeIsData ''Dependencies

validator ::
  (MintingPolicyHash, Dependencies, PubKeyHash) ->
  TxOutRef ->
  ScriptContext ->
  Bool
validator (policyId, Dependencies{headScript, commitScript}, vk) ref ctx =
  consumedByCommit || consumedByAbort
 where
  -- A commit transaction, identified by:
  --    (a) A signature that verifies as valid with verification key defined as datum
  --    (b) Spending a UTxO also referenced as redeemer.
  --    (c) Having the commit validator in its only output, with a valid
  --        participation token for the associated key, and the total value of the
  --        committed UTxO.
  consumedByCommit =
    case findUtxo ref ctx of
      Nothing ->
        False
      Just utxo ->
        let commitDatum = Commit.datum (Commit.Dependencies headScript, snd utxo)
            commitValue = txOutValue (snd utxo) <> mkParty policyId vk
         in checkScriptContext @(RedeemerType Initial) @(DatumType Initial)
              ( mconcat
                  [ mustBeSignedBy vk
                  , mustSpendPubKeyOutput (fst utxo)
                  , mustPayToOtherScript commitScript commitDatum commitValue
                  ]
              )
              ctx

  consumedByAbort =
    mustRunContract @(RedeemerType Head) headScript Abort ctx

compiledValidator :: CompiledCode (ValidatorType Initial)
compiledValidator = $$(PlutusTx.compile [||validator||])

{- ORMOLU_DISABLE -}
typedValidator :: TypedValidator Initial
typedValidator = Scripts.mkTypedValidator @Initial
  compiledValidator
  $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Scripts.wrapValidator @(DatumType Initial) @(RedeemerType Initial)
{- ORMOLU_ENABLE -}

validatorHash :: ValidatorHash
validatorHash = Scripts.validatorHash typedValidator

datum :: DatumType Initial -> Datum
datum a = Datum (toBuiltinData a)

address :: Address
address = scriptHashAddress validatorHash

mustPayToScript ::
  forall i o.
  MintingPolicyHash ->
  Dependencies ->
  PubKeyHash ->
  Value ->
  TxConstraints i o
mustPayToScript policyId dependencies pubKey =
  mustPayToOtherScript validatorHash $ datum (policyId, dependencies, pubKey)

-- | Get the actual plutus script. Mainly used to serialize and use in
-- transactions.
validatorScript :: Script
validatorScript = unValidatorScript $ Scripts.validatorScript typedValidator
