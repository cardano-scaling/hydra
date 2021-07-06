{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | Simplified SM-based contract for the purpose of developing the interface
-- between Node and Chain
module Hydra.ContractSM where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger (AssetClass)
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.Contract (BlockchainActions)
import Plutus.Contract.StateMachine (StateMachine)
import qualified Plutus.Contract.StateMachine as SM
import Plutus.Contract.Types (Contract)
import qualified PlutusTx
import PlutusTx.Prelude
import Text.Show (Show)

data State
  = Initial
  | Open
  | Final
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''State
PlutusTx.unstableMakeIsData ''State

data Input
  = CollectCom
  | Abort
  deriving (Generic)

PlutusTx.makeLift ''Input
PlutusTx.unstableMakeIsData ''Input

{-# INLINEABLE hydraStateMachine #-}
hydraStateMachine :: AssetClass -> StateMachine State Input
hydraStateMachine threadToken = SM.mkStateMachine (Just threadToken) hydraTransition isFinal
 where
  isFinal Final{} = True
  isFinal _ = False

{-# INLINEABLE hydraTransition #-}
hydraTransition :: SM.State State -> Input -> Maybe (SM.TxConstraints SM.Void SM.Void, SM.State State)
hydraTransition _state _input = Nothing

-- | The script instance of the auction state machine. It contains the state
--   machine compiled to a Plutus core validator script.
typedValidator :: AssetClass -> Scripts.TypedValidator (StateMachine State Input)
typedValidator currency =
  let val =
        $$(PlutusTx.compile [||validatorParam||])
          `PlutusTx.applyCode` PlutusTx.liftCode currency
      validatorParam c = SM.mkValidator (hydraStateMachine c)
      wrap = Scripts.wrapValidator @State @Input
   in Scripts.mkTypedValidator @(StateMachine State Input)
        val
        $$(PlutusTx.compile [||wrap||])

init :: Contract () BlockchainActions e ()
init = pure ()
