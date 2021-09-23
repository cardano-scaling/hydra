{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.Head where

import Ledger hiding (validatorHash)
import PlutusTx.Prelude

import Control.Arrow (first)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Hydra.Data.ContestationPeriod (ContestationPeriod)
import Hydra.Data.Party (Party)
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value (AssetClass (..), currencyMPSHash)
import Plutus.Contract.StateMachine (StateMachine, StateMachineClient)
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx
import Text.Show (Show)

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

type Head = StateMachine State Input

{-# INLINEABLE hydraStateMachine #-}
hydraStateMachine :: MintingPolicyHash -> StateMachine State Input
hydraStateMachine _policyId =
  -- XXX(SN): This should actually be '(Just policyId)' as we wan't to have
  -- "contract continuity" as described in the EUTXO paper. While we do have a
  -- fix for the 'runStep' handling now, the current version of plutus does
  -- forge a given 'ThreadToken' upon 'runInitialise' now.. which is not what we
  -- want as we need additional tokens being forged as well (see 'watchInit').
  SM.mkStateMachine Nothing hydraTransition isFinal
 where
  -- XXX(SN): This is currently required to be able to observe the 'Abort'
  -- transition!?
  -- isFinal Final{} = True
  isFinal _ = False

{-# INLINEABLE hydraTransition #-}
hydraTransition :: SM.State State -> Input -> Maybe (SM.TxConstraints SM.Void SM.Void, SM.State State)
hydraTransition oldState input =
  case (SM.stateData oldState, input) of
    (Initial{}, CollectCom) ->
      Just (mempty, oldState{SM.stateData = Open})
    (Initial{}, Abort) ->
      Just (mempty, oldState{SM.stateData = Final})
    _ -> Nothing

-- | The script instance of the auction state machine. It contains the state
-- machine compiled to a Plutus core validator script. The 'MintingPolicyHash' serves
-- two roles here:
--
--   1. Parameterizing the script, such that we get a unique address and allow
--   for multiple instances of it
--
--   2. Identify the 'state thread token', which should be passed in
--   transactions transitioning the state machine and provide "contract
--   continuity"
typedValidator :: MintingPolicyHash -> Scripts.TypedValidator (StateMachine State Input)
typedValidator policyId =
  let val =
        $$(PlutusTx.compile [||validatorParam||])
          `PlutusTx.applyCode` PlutusTx.liftCode policyId
      validatorParam c = SM.mkValidator (hydraStateMachine c)
      wrap = Scripts.wrapValidator @State @Input
   in Scripts.mkTypedValidator @(StateMachine State Input)
        val
        $$(PlutusTx.compile [||wrap||])

validatorHash :: MintingPolicyHash -> ValidatorHash
validatorHash = Scripts.validatorHash . typedValidator

address :: MintingPolicyHash -> Address
address = scriptHashAddress . validatorHash

-- | Get the actual plutus script. Mainly used to serialize and use in
-- transactions.
validatorScript :: MintingPolicyHash -> Script
validatorScript = unValidatorScript . Scripts.validatorScript . typedValidator

-- | The machine client of the hydra state machine. It contains both, the script
-- instance with the on-chain code, and the Haskell definition of the state
-- machine for off-chain use.
machineClient ::
  -- | PolicyId for the head instance
  AssetClass ->
  StateMachineClient State Input
machineClient token =
  let (policyId, _) = first currencyMPSHash (unAssetClass token)
      machine = hydraStateMachine policyId
      inst = typedValidator policyId
   in SM.mkStateMachineClient (SM.StateMachineInstance machine inst)
