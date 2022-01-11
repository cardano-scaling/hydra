{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | The Initial validator which allows participants to commit or abort. To
-- focus on the off-chain datum types this is currently 'const True'.
module Hydra.Contract.Initial where

import Ledger hiding (validatorHash)
import PlutusTx.Prelude

import Ledger.Typed.Scripts (TypedValidator, ValidatorType, ValidatorTypes (..))
import qualified Ledger.Typed.Scripts as Scripts
import PlutusTx (CompiledCode)
import qualified PlutusTx
import PlutusTx.IsData.Class (ToData (..))

data Initial

instance Scripts.ValidatorTypes Initial where
  type DatumType Initial = PubKeyHash
  type RedeemerType Initial = ()

validator :: DatumType Initial -> RedeemerType Initial -> ScriptContext -> Bool
validator _datum _redeemer _ctx = True

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

-- | Get the actual plutus script. Mainly used to serialize and use in
-- transactions.
validatorScript :: Script
validatorScript = unValidatorScript $ Scripts.validatorScript typedValidator

validatorHash :: ValidatorHash
validatorHash = Scripts.validatorHash typedValidator

datum :: DatumType Initial -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: RedeemerType Initial -> Redeemer
redeemer a = Redeemer (toBuiltinData a)

address :: Address
address = scriptHashAddress validatorHash
