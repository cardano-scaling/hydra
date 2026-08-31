{-# LANGUAGE UndecidableInstances #-}

-- | Error types used in the Hydra.HeadLogic module.
module Hydra.HeadLogic.Error where

import Hydra.Prelude

import Hydra.HeadLogic.Input (Input)
import Hydra.HeadLogic.State (HeadState)
import Hydra.Ledger (ValidationError)
import Hydra.Tx (
  HeadId,
  IsTx (TxIdType, UTxOType),
  Party,
  SnapshotNumber,
  SnapshotVersion,
 )
import Hydra.Tx.Crypto (HydraKey, VerificationKey)

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

data RequirementFailure tx
  = ReqSnNumberInvalid {requestedSn :: SnapshotNumber, lastSeenSn :: SnapshotNumber}
  | ReqSvNumberInvalid {requestedSv :: SnapshotVersion, lastSeenSv :: SnapshotVersion}
  | ReqSnNotLeader {requestedSn :: SnapshotNumber, leader :: Party}
  | ReqSnDecommitNotSettled
  | ReqSnCommitNotSettled
  | -- | A snapshot may settle a commit or a decommit, never both: close and
    -- fanout carry a single incremental action, so a snapshot with both would
    -- leave the head unclosable.
    ReqSnBothCommitAndDecommit {depositTxId :: TxIdType tx, decommitTxId :: TxIdType tx}
  | -- | A decommit that materializes no output cannot be settled: the decrement
    -- validator requires at least one decommit output.
    ReqSnDecommitNoOutputs {decommitTxId :: TxIdType tx}
  | InvalidMultisignature {multisig :: Text, vkeys :: [VerificationKey HydraKey]}
  | SnapshotAlreadySigned {knownSignatures :: [Party], receivedSignature :: Party}
  | AckSnNumberInvalid {requestedSn :: SnapshotNumber, lastSeenSn :: SnapshotNumber}
  | SnapshotDoesNotApply {requestedSn :: SnapshotNumber, txid :: TxIdType tx, error :: ValidationError}
  | NoMatchingDeposit
  | -- | The deposit is claimed by a signed snapshot whose increment settled
    -- on-chain but was rolled back: its funds are already accounted for in the
    -- head, so recovering them on-chain would corrupt the L2 ledger. The
    -- increment is re-posted instead, see #2741.
    RecoverBlockedByFinalizedCommit {depositTxId :: TxIdType tx}
  | RequestedDepositExpired {depositTxId :: TxIdType tx}
  | RequestedDepositNotFoundLocally {depositTxId :: TxIdType tx}
  | ReqSnUTxOSetTooLarge {utxoCount :: Int, maxAllowed :: Int}
  deriving stock (Generic)

deriving stock instance Eq (TxIdType tx) => Eq (RequirementFailure tx)
deriving stock instance Show (TxIdType tx) => Show (RequirementFailure tx)
deriving anyclass instance ToJSON (TxIdType tx) => ToJSON (RequirementFailure tx)

instance IsTx tx => ToCBOR (RequirementFailure tx) where
  toCBOR = genericToCBOR

instance IsTx tx => FromCBOR (RequirementFailure tx) where
  fromCBOR = genericFromCBOR

data SideLoadRequirementFailure tx
  = SideLoadInitialSnapshotMismatch
  | SideLoadSnNumberInvalid {requestedSn :: SnapshotNumber, lastSeenSn :: SnapshotNumber}
  | SideLoadSvNumberInvalid {requestedSv :: SnapshotVersion, lastSeenSv :: SnapshotVersion}
  | SideLoadUTxOToCommitInvalid {requestedSc :: Maybe (UTxOType tx), lastSeenSc :: Maybe (UTxOType tx)}
  | -- | The side-loaded snapshot commits the same UTxO as the confirmed one, but
    -- from a different deposit. Since the deposit is what identifies a pending
    -- commit, this is a distinct disagreement from 'SideLoadUTxOToCommitInvalid',
    -- whose payload would show two identical UTxO sets here.
    SideLoadDepositTxIdInvalid {requestedDeposit :: Maybe (TxIdType tx), lastSeenDeposit :: Maybe (TxIdType tx)}
  | SideLoadUTxOToDecommitInvalid {requestedSd :: Maybe (UTxOType tx), lastSeenSd :: Maybe (UTxOType tx)}
  | SideLoadInvalidMultisignature {multisig :: Text, vkeys :: [VerificationKey HydraKey]}
  deriving stock (Generic)

deriving stock instance (Eq (UTxOType tx), Eq (TxIdType tx)) => Eq (SideLoadRequirementFailure tx)
deriving stock instance (Show (UTxOType tx), Show (TxIdType tx)) => Show (SideLoadRequirementFailure tx)
deriving anyclass instance (ToJSON (UTxOType tx), ToJSON (TxIdType tx)) => ToJSON (SideLoadRequirementFailure tx)
deriving anyclass instance (FromJSON (UTxOType tx), FromJSON (TxIdType tx)) => FromJSON (SideLoadRequirementFailure tx)

instance IsTx tx => ToCBOR (SideLoadRequirementFailure tx) where
  toCBOR = genericToCBOR

instance IsTx tx => FromCBOR (SideLoadRequirementFailure tx) where
  fromCBOR = genericFromCBOR
