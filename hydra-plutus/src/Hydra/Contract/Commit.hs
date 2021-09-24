{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | Contract for Hydra controlling the redemption of commits from participants.
module Hydra.Contract.Commit where

import Ledger hiding (validatorHash)
import PlutusTx.Prelude

import Hydra.Contract.Head (Head, Input (..))
import Hydra.OnChain.Util (mustReimburse, mustRunContract)
import Ledger.Typed.Scripts (TypedValidator, ValidatorType, ValidatorTypes (..))
import qualified Ledger.Typed.Scripts as Scripts
import PlutusTx (CompiledCode)
import qualified PlutusTx
import PlutusTx.IsData.Class (ToData (..))

data Commit

instance Scripts.ValidatorTypes Commit where
  type DatumType Commit = (Dependencies, TxOut)
  type RedeemerType Commit = ()

-- See note on Hydra.Contract.Initial#Dependencies
data Dependencies = Dependencies
  { headScript :: ValidatorHash
  }

PlutusTx.makeLift ''Dependencies
PlutusTx.unstableMakeIsData ''Dependencies

validator ::
  (Dependencies, TxOut) ->
  () ->
  ScriptContext ->
  Bool
validator (Dependencies{headScript}, committedOut) () ctx =
  consumedByCollectCom || consumedByAbort
 where
  consumedByCollectCom =
    mustRunContract @(RedeemerType Head) headScript CollectCom ctx

  consumedByAbort =
    and
      [ mustRunContract @(RedeemerType Head) headScript Abort ctx
      , mustReimburse committedOut ctx
      ]

compiledValidator :: CompiledCode (ValidatorType Commit)
compiledValidator = $$(PlutusTx.compile [||validator||])

{- ORMOLU_DISABLE -}
typedValidator :: TypedValidator Commit
typedValidator = Scripts.mkTypedValidator @Commit
  compiledValidator
  $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Scripts.wrapValidator @(DatumType Commit) @(RedeemerType Commit)
{- ORMOLU_ENABLE -}

-- | Get the actual plutus script. Mainly used to serialize and use in
-- transactions.
validatorScript :: Script
validatorScript = unValidatorScript $ Scripts.validatorScript typedValidator

validatorHash :: ValidatorHash
validatorHash = Scripts.validatorHash typedValidator

datum :: DatumType Commit -> Datum
datum a = Datum (toBuiltinData a)

address :: Address
address = scriptHashAddress validatorHash
