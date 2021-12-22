{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.MockHead where

import Ledger hiding (validatorHash)
import PlutusTx.Prelude

import Control.Monad (guard)
import Data.Aeson (FromJSON, ToJSON)
import Data.Void (Void)
import GHC.Generics (Generic)
import Hydra.Data.ContestationPeriod (ContestationPeriod)
import Hydra.Data.Party (Party (UnsafeParty))
import Ledger.Constraints (TxConstraints)
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.Contract.StateMachine.OnChain (StateMachine)
import qualified Plutus.Contract.StateMachine.OnChain as SM
import qualified PlutusTx
import PlutusTx.Builtins (quotientInteger, remainderInteger)
import Text.Show (Show)

type SnapshotNumber = BuiltinByteString

data State
  = Initial {contestationPeriod :: ContestationPeriod, parties :: [Party]}
  | Open {parties :: [Party]}
  | Closed
  | Final
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''State

data Input
  = -- FIXME(AB): The collected value should not be passed as input but inferred from
    -- collected commits' value
    CollectCom Value
  | Close
      { snapshotNumber :: SnapshotNumber
      , signature :: [Signature]
      }
  | Abort
  | Fanout
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
  isFinal Final{} = True
  isFinal _ = False

{-# INLINEABLE hydraTransition #-}
hydraTransition :: SM.State State -> Input -> Maybe (TxConstraints Void Void, SM.State State)
hydraTransition oldState input =
  case (SM.stateData oldState, input) of
    (Initial{parties}, CollectCom collectedValue) ->
      Just (mempty, oldState{SM.stateData = Open{parties}, SM.stateValue = collectedValue <> SM.stateValue oldState})
    (Initial{}, Abort) ->
      Just (mempty, oldState{SM.stateData = Final, SM.stateValue = mempty})
    (Open{parties}, Close{snapshotNumber, signature}) -> do
      guard $ verifySnapshotSignature parties snapshotNumber signature
      Just (mempty, oldState{SM.stateData = Closed})
    (Closed{}, Fanout{}) ->
      Just (mempty, oldState{SM.stateData = Final, SM.stateValue = mempty})
    _ -> Nothing

{-# INLINEABLE verifySnapshotSignature #-}
verifySnapshotSignature :: [Party] -> SnapshotNumber -> [Signature] -> Bool
verifySnapshotSignature parties snapshotNumber sigs =
  traceIfFalse "signature verification failed" $
    length parties == length sigs
      && all (uncurry $ verifyPartySignature snapshotNumber) (zip parties sigs)

{-# INLINEABLE verifyPartySignature #-}
verifyPartySignature :: BuiltinByteString -> Party -> Signature -> Bool
verifyPartySignature msg (UnsafeParty vkey) signed =
  traceIfFalse "party signature verification failed" $
    mockVerifySignature vkey msg (getSignature signed)

{-# INLINEABLE mockVerifySignature #-}
-- TODO: This really should be the builtin Plutus function 'verifySignature' but as we
-- are using Mock crypto in the Head, so must we use Mock crypto on-chain to verify
-- signatures.
mockVerifySignature :: Integer -> BuiltinByteString -> BuiltinByteString -> Bool
mockVerifySignature vkey msg signed =
  traceIfFalse "mock signed message is not equal to signed" $
    mockSign vkey (hashBytes msg) == signed

{-# INLINEABLE hashBytes #-}
hashBytes :: BuiltinByteString -> BuiltinByteString
hashBytes = sha2_256

{-# INLINEABLE mockSign #-}
mockSign :: Integer -> BuiltinByteString -> BuiltinByteString
mockSign vkey msg = appendByteString (sliceByteString 0 8 msg) (toWord64BE vkey)

{-# INLINEABLE toWord64BE #-}

-- | Encode an Integer into a 8-bytes long Bytestring representing this number in
-- Big-Endian form (eg. most significant bit first).
toWord64BE :: Integer -> BuiltinByteString
toWord64BE = go emptyByteString
 where
  go bs _
    | lengthOfByteString bs == 8 = bs
  go bs n =
    let quot = quotientInteger n 256
        rem = remainderInteger n 256
     in go (consByteString rem bs) quot

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
