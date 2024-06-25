{-# LANGUAGE UndecidableInstances #-}

-- | Error types used in the Hydra.HeadLogic module.
module Hydra.HeadLogic.Error where

import Hydra.Prelude

import Hydra.Crypto (HydraKey, VerificationKey)
import Hydra.HeadId (HeadId)
import Hydra.HeadLogic.Input (Input)
import Hydra.HeadLogic.State (HeadState)
import Hydra.Ledger (IsTx (TxIdType), ValidationError)
import Hydra.Party (Party)
import Hydra.Snapshot (SnapshotNumber)

data LogicError tx
  = UnhandledInput {input :: Input tx, currentHeadState :: HeadState tx}
  | RequireFailed {requirementFailure :: RequirementFailure tx}
  | NotOurHead {ourHeadId :: HeadId, otherHeadId :: HeadId}
  deriving stock (Generic)

instance (Typeable tx, Show (Input tx), Show (HeadState tx), Show (RequirementFailure tx)) => Exception (LogicError tx)

instance (Arbitrary (Input tx), Arbitrary (HeadState tx), Arbitrary (RequirementFailure tx)) => Arbitrary (LogicError tx) where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving stock instance (Eq (HeadState tx), Eq (Input tx), Eq (RequirementFailure tx)) => Eq (LogicError tx)
deriving stock instance (Show (HeadState tx), Show (Input tx), Show (RequirementFailure tx)) => Show (LogicError tx)
deriving anyclass instance (ToJSON (HeadState tx), ToJSON (Input tx), ToJSON (RequirementFailure tx)) => ToJSON (LogicError tx)
deriving anyclass instance (FromJSON (HeadState tx), FromJSON (Input tx), FromJSON (RequirementFailure tx)) => FromJSON (LogicError tx)

data RequirementFailure tx
  = ReqSnNumberInvalid {requestedSn :: SnapshotNumber, lastSeenSn :: SnapshotNumber}
  | ReqSnNotLeader {requestedSn :: SnapshotNumber, leader :: Party}
  | InvalidMultisignature {multisig :: Text, vkeys :: [VerificationKey HydraKey]}
  | SnapshotAlreadySigned {knownSignatures :: [Party], receivedSignature :: Party}
  | AckSnNumberInvalid {requestedSn :: SnapshotNumber, lastSeenSn :: SnapshotNumber}
  | SnapshotDoesNotApply {requestedSn :: SnapshotNumber, txid :: TxIdType tx, error :: ValidationError}
  | DecommitTxInvalid {decommitTx :: tx, error :: ValidationError}
  | DecommitTxInFlight {decommitTx :: tx}
  | DecommitDoesNotApply {decommitTx :: tx, error :: ValidationError}
  deriving stock (Generic)

deriving stock instance (Eq tx, Eq (TxIdType tx)) => Eq (RequirementFailure tx)
deriving stock instance (Show tx, Show (TxIdType tx)) => Show (RequirementFailure tx)
deriving anyclass instance (ToJSON tx, ToJSON (TxIdType tx)) => ToJSON (RequirementFailure tx)
deriving anyclass instance (FromJSON tx, FromJSON (TxIdType tx)) => FromJSON (RequirementFailure tx)

instance (Arbitrary tx, Arbitrary (TxIdType tx)) => Arbitrary (RequirementFailure tx) where
  arbitrary = genericArbitrary
