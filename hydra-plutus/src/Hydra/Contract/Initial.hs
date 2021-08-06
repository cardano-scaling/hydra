{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | Contract for Hydra controlling the redemption of participation tokens.
module Hydra.Contract.Initial where

import Ledger
import PlutusTx.Prelude

import Ledger.Typed.Scripts (ValidatorType, ValidatorTypes (..))
import qualified Ledger.Typed.Scripts as Scripts
import PlutusTx (CompiledCode)
import qualified PlutusTx

data Initial

instance Scripts.ValidatorTypes Initial where
  type DatumType Initial = ()
  type RedeemerType Initial = ()

validator ::
  () ->
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
