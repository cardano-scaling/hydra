{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | Contract for Hydra controlling the redemption of participation tokens.
module Hydra.Contract.Initial where

import Ledger hiding (validatorHash)
import PlutusTx.Prelude

import Ledger.Constraints.TxConstraints (TxConstraints, mustPayToOtherScript)
import Ledger.Typed.Scripts (ValidatorType, ValidatorTypes (..))
import qualified Ledger.Typed.Scripts as Scripts
import PlutusTx (CompiledCode)
import qualified PlutusTx
import PlutusTx.IsData.Class (ToData (..))

data Initial

instance Scripts.ValidatorTypes Initial where
  type DatumType Initial = PubKeyHash
  type RedeemerType Initial = ()

validator ::
  PubKeyHash ->
  () ->
  ScriptContext ->
  Bool
validator _ _ _ctx =
  True

compiledValidator :: CompiledCode (ValidatorType Initial)
compiledValidator = $$(PlutusTx.compile [||validator||])

{- ORMOLU_DISABLE -}
typedValidator :: Scripts.TypedValidator Initial
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

mustPayToScript :: forall i o. PubKeyHash -> Value -> TxConstraints i o
mustPayToScript pubKey = mustPayToOtherScript validatorHash (datum pubKey)
