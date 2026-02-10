{-# LANGUAGE UndecidableInstances #-}

-- | Error types used in the Hydra.HeadLogic module.
module Hydra.HeadLogic.Error where

import "hydra-prelude" Hydra.Prelude

import "hydra-node" Hydra.HeadLogic.Input (Input)
import "hydra-node" Hydra.HeadLogic.State (HeadState)
import "hydra-node" Hydra.Ledger (ValidationError)
import "hydra-tx" Hydra.Tx (
  HeadId,
  IsTx (TxIdType, UTxOType),
  Party,
  SnapshotNumber,
  SnapshotVersion,
 )
import "hydra-tx" Hydra.Tx.Crypto (HydraKey, VerificationKey)

data LogicError tx
  = UnhandledInput {input :: Input tx, currentHeadState :: HeadState tx}
  | RequireFailed {requirementFailure :: RequirementFailure tx}
  | AssertionFailed {message :: Text}
  | NotOurHead {ourHeadId :: HeadId, otherHeadId :: HeadId}
  | SideLoadSnapshotFailed {sideLoadRequirementFailure :: SideLoadRequirementFailure tx}
  deriving stock (Generic)

deriving stock instance
  ( Eq (HeadState tx)
  , Eq (Input tx)
  , Eq (RequirementFailure tx)
  , Eq (SideLoadRequirementFailure tx)
  ) =>
  Eq (LogicError tx)

deriving stock instance
  ( Show (HeadState tx)
  , Show (Input tx)
  , Show (RequirementFailure tx)
  , Show (SideLoadRequirementFailure tx)
  ) =>
  Show (LogicError tx)

deriving anyclass instance
  ( ToJSON (HeadState tx)
  , ToJSON (Input tx)
  , ToJSON (RequirementFailure tx)
  , ToJSON (SideLoadRequirementFailure tx)
  ) =>
  ToJSON (LogicError tx)

deriving anyclass instance
  ( FromJSON (HeadState tx)
  , FromJSON (Input tx)
  , FromJSON (RequirementFailure tx)
  , FromJSON (SideLoadRequirementFailure tx)
  ) =>
  FromJSON (LogicError tx)

data RequirementFailure tx
  = ReqSnNumberInvalid {requestedSn :: SnapshotNumber, lastSeenSn :: SnapshotNumber}
  | ReqSvNumberInvalid {requestedSv :: SnapshotVersion, lastSeenSv :: SnapshotVersion}
  | ReqSnNotLeader {requestedSn :: SnapshotNumber, leader :: Party}
  | ReqSnDecommitNotSettled
  | ReqSnCommitNotSettled
  | InvalidMultisignature {multisig :: Text, vkeys :: [VerificationKey HydraKey]}
  | SnapshotAlreadySigned {knownSignatures :: [Party], receivedSignature :: Party}
  | AckSnNumberInvalid {requestedSn :: SnapshotNumber, lastSeenSn :: SnapshotNumber}
  | SnapshotDoesNotApply {requestedSn :: SnapshotNumber, txid :: TxIdType tx, error :: ValidationError}
  | NoMatchingDeposit
  | RequestedDepositExpired {depositTxId :: TxIdType tx}
  deriving stock (Generic)

deriving stock instance Eq (TxIdType tx) => Eq (RequirementFailure tx)
deriving stock instance Show (TxIdType tx) => Show (RequirementFailure tx)
deriving anyclass instance ToJSON (TxIdType tx) => ToJSON (RequirementFailure tx)
deriving anyclass instance FromJSON (TxIdType tx) => FromJSON (RequirementFailure tx)

data SideLoadRequirementFailure tx
  = SideLoadInitialSnapshotMismatch
  | SideLoadSnNumberInvalid {requestedSn :: SnapshotNumber, lastSeenSn :: SnapshotNumber}
  | SideLoadSvNumberInvalid {requestedSv :: SnapshotVersion, lastSeenSv :: SnapshotVersion}
  | SideLoadUTxOToCommitInvalid {requestedSc :: Maybe (UTxOType tx), lastSeenSc :: Maybe (UTxOType tx)}
  | SideLoadUTxOToDecommitInvalid {requestedSd :: Maybe (UTxOType tx), lastSeenSd :: Maybe (UTxOType tx)}
  | SideLoadInvalidMultisignature {multisig :: Text, vkeys :: [VerificationKey HydraKey]}
  deriving stock (Generic)

deriving stock instance Eq (UTxOType tx) => Eq (SideLoadRequirementFailure tx)
deriving stock instance Show (UTxOType tx) => Show (SideLoadRequirementFailure tx)
deriving anyclass instance ToJSON (UTxOType tx) => ToJSON (SideLoadRequirementFailure tx)
deriving anyclass instance FromJSON (UTxOType tx) => FromJSON (SideLoadRequirementFailure tx)
