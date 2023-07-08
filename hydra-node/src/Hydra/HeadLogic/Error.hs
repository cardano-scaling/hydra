{-# LANGUAGE UndecidableInstances #-}
module Hydra.HeadLogic.Error where

import Hydra.Prelude

import Hydra.Chain (HeadId)
import Hydra.HeadLogic.Event (Event)
import Hydra.HeadLogic.State (HeadState)
import Hydra.Ledger (ValidationError, IsTx (TxIdType))
import Hydra.Snapshot (SnapshotNumber)
import Hydra.Party (Party)
import Hydra.Crypto (VerificationKey, HydraKey)

-- | Preliminary type for collecting errors occurring during 'update'.
-- TODO: Try to merge this (back) into 'Outcome'.
data LogicError tx
  = InvalidEvent (Event tx) (HeadState tx)
  | InvalidState (HeadState tx)
  | InvalidSnapshot {expected :: SnapshotNumber, actual :: SnapshotNumber}
  | LedgerError ValidationError
  | RequireFailed (RequirementFailure tx)
  | NotOurHead {ourHeadId :: HeadId, otherHeadId :: HeadId}
  deriving stock (Generic)

instance (Typeable tx, Show (Event tx), Show (HeadState tx), Show (RequirementFailure tx)) => Exception (LogicError tx)

instance (Arbitrary (Event tx), Arbitrary (HeadState tx), Arbitrary (RequirementFailure tx)) => Arbitrary (LogicError tx) where
  arbitrary = genericArbitrary

deriving instance (Eq (HeadState tx), Eq (Event tx), Eq (RequirementFailure tx)) => Eq (LogicError tx)
deriving instance (Show (HeadState tx), Show (Event tx), Show (RequirementFailure tx)) => Show (LogicError tx)
deriving instance (ToJSON (HeadState tx), ToJSON (Event tx), ToJSON (RequirementFailure tx)) => ToJSON (LogicError tx)
deriving instance (FromJSON (HeadState tx), FromJSON (Event tx), FromJSON (RequirementFailure tx)) => FromJSON (LogicError tx)

data RequirementFailure tx
  = ReqSnNumberInvalid {requestedSn :: SnapshotNumber, lastSeenSn :: SnapshotNumber}
  | ReqSnNotLeader {requestedSn :: SnapshotNumber, leader :: Party}
  | InvalidMultisignature {multisig :: Text, vkeys :: [VerificationKey HydraKey]}
  | SnapshotAlreadySigned {knownSignatures :: [Party], receivedSignature :: Party}
  | AckSnNumberInvalid {requestedSn :: SnapshotNumber, lastSeenSn :: SnapshotNumber}
  | SnapshotDoesNotApply {requestedSn :: SnapshotNumber, txid :: TxIdType tx, error :: ValidationError}
  deriving stock (Generic)

deriving instance (Eq (TxIdType tx)) => Eq (RequirementFailure tx)
deriving instance (Show (TxIdType tx)) => Show (RequirementFailure tx)
deriving instance (ToJSON (TxIdType tx)) => ToJSON (RequirementFailure tx)
deriving instance (FromJSON (TxIdType tx)) => FromJSON (RequirementFailure tx)

instance Arbitrary (TxIdType tx) => Arbitrary (RequirementFailure tx) where
  arbitrary = genericArbitrary
