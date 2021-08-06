{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | Simplified SM-based contract for the purpose of developing the interface
-- between Node and Chain
module Hydra.ContractSM where

import PlutusTx.Prelude

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import GHC.Show (Show)
import Hydra.Contract.ContestationPeriod (ContestationPeriod)
import Hydra.Contract.Party (Party)
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value (AssetClass)
import Plutus.Contract.StateMachine (StateMachine)
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx

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

{-# INLINEABLE hydraStateMachine #-}
hydraStateMachine :: AssetClass -> StateMachine State Input
hydraStateMachine _threadToken =
  -- XXX(SN): This should actually be '(Just threadToken)' as we wan't to have
  -- "contract continuity" as described in the EUTXO paper. While we do have a
  -- fix for the 'runStep' handling now, the current version of plutus does
  -- forge a given 'ThreadToken' upon 'runInitialise' now.. which is not what we
  -- want as we need additional tokens being forged as well (see 'watchInit').
  SM.mkStateMachine Nothing hydraTransition isFinal
 where
  isFinal Final{} = True
  isFinal _ = False

{-# INLINEABLE hydraTransition #-}
hydraTransition :: SM.State State -> Input -> Maybe (SM.TxConstraints SM.Void SM.Void, SM.State State)
hydraTransition oldState input =
  case (SM.stateData oldState, input) of
    _ -> Nothing

-- | The script instance of the auction state machine. It contains the state
-- machine compiled to a Plutus core validator script. The 'AssetClass' serves
-- two roles here:
--
--   1. Parameterizing the script, such that we get a unique address and allow
--   for multiple instances of it
--
--   2. Identify the 'state thread token', which should be passed in
--   transactions transitioning the state machine and provide "contract
--   continuity"
typedValidator :: AssetClass -> Scripts.TypedValidator (StateMachine State Input)
typedValidator threadToken =
  let val =
        $$(PlutusTx.compile [||validatorParam||])
          `PlutusTx.applyCode` PlutusTx.liftCode threadToken
      validatorParam c = SM.mkValidator (hydraStateMachine c)
      wrap = Scripts.wrapValidator @State @Input
   in Scripts.mkTypedValidator @(StateMachine State Input)
        val
        $$(PlutusTx.compile [||wrap||])
