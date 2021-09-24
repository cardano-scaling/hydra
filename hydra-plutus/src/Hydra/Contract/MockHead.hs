{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | A mock Head contract not using a state machine for testing purpose.
module Hydra.Contract.MockHead where

import Ledger
import PlutusTx.Prelude

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Hydra.Data.ContestationPeriod (ContestationPeriod)
import Hydra.Data.Party (Party)
import Ledger.Typed.Scripts (ValidatorType, ValidatorTypes (DatumType, RedeemerType))
import qualified Ledger.Typed.Scripts as Scripts
import PlutusTx (CompiledCode)
import qualified PlutusTx
import Text.Show (Show)

data Head

data State
  = Initial ContestationPeriod [Party]
  | Open
  | Final
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''State

data Input
  = CollectCom
  | Abort
  deriving (Generic, Show)

PlutusTx.unstableMakeIsData ''Input

instance Scripts.ValidatorTypes Head where
  type DatumType Head = State
  type RedeemerType Head = Input

{-# INLINEABLE validator #-}
validator ::
  MintingPolicyHash ->
  State ->
  Input ->
  ScriptContext ->
  Bool
validator _ _ _ _ctx =
  True

compiledValidator :: MintingPolicyHash -> CompiledCode (ValidatorType Head)
compiledValidator token =
  $$(PlutusTx.compile [||validator||])
    `PlutusTx.applyCode` PlutusTx.liftCode token

{- ORMOLU_DISABLE -}
typedValidator :: MintingPolicyHash -> Scripts.TypedValidator Head
typedValidator token = Scripts.mkTypedValidator @Head
  (compiledValidator token)
  $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Scripts.wrapValidator @(DatumType Head) @(RedeemerType Head)
{- ORMOLU_ENABLE -}

-- | Do not use this outside of plutus land.
validatorHash :: MintingPolicyHash -> ValidatorHash
validatorHash = Scripts.validatorHash . typedValidator

-- | Get the actual plutus script. Mainly used to serialize and use in
-- transactions.
validatorScript :: MintingPolicyHash -> Script
validatorScript = unValidatorScript . Scripts.validatorScript . typedValidator
