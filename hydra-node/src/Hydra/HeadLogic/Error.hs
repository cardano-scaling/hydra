{-# LANGUAGE UndecidableInstances #-}

-- | Error types used in the Hydra.HeadLogic module.
module Hydra.HeadLogic.Error where

import Hydra.Prelude

import Hydra.Chain.ChainState (IsChainState)
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

instance (IsChainState tx, ToCBOR (Input tx)) => ToCBOR (LogicError tx) where
  toCBOR = \case
    UnhandledInput{input, currentHeadState} ->
      toCBOR ("UnhandledInput" :: Text) <> toCBOR input <> toCBOR currentHeadState
    RequireFailed{requirementFailure} ->
      toCBOR ("RequireFailed" :: Text) <> toCBOR requirementFailure
    AssertionFailed{message} ->
      toCBOR ("AssertionFailed" :: Text) <> toCBOR message
    NotOurHead{ourHeadId, otherHeadId} ->
      toCBOR ("NotOurHead" :: Text) <> toCBOR ourHeadId <> toCBOR otherHeadId
    SideLoadSnapshotFailed{sideLoadRequirementFailure} ->
      toCBOR ("SideLoadSnapshotFailed" :: Text) <> toCBOR sideLoadRequirementFailure

instance (IsChainState tx, FromCBOR (Input tx)) => FromCBOR (LogicError tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("UnhandledInput" :: Text) -> UnhandledInput <$> fromCBOR <*> fromCBOR
      "RequireFailed" -> RequireFailed <$> fromCBOR
      "AssertionFailed" -> AssertionFailed <$> fromCBOR
      "NotOurHead" -> NotOurHead <$> fromCBOR <*> fromCBOR
      "SideLoadSnapshotFailed" -> SideLoadSnapshotFailed <$> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded LogicError"

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
  | RequestedDepositNotFoundLocally {depositTxId :: TxIdType tx}
  | ReqSnUTxOSetTooLarge {utxoCount :: Int, maxAllowed :: Int}
  deriving stock (Generic)

deriving stock instance Eq (TxIdType tx) => Eq (RequirementFailure tx)
deriving stock instance Show (TxIdType tx) => Show (RequirementFailure tx)
deriving anyclass instance ToJSON (TxIdType tx) => ToJSON (RequirementFailure tx)

instance IsTx tx => ToCBOR (RequirementFailure tx) where
  toCBOR = \case
    ReqSnNumberInvalid{requestedSn, lastSeenSn} ->
      toCBOR ("ReqSnNumberInvalid" :: Text) <> toCBOR requestedSn <> toCBOR lastSeenSn
    ReqSvNumberInvalid{requestedSv, lastSeenSv} ->
      toCBOR ("ReqSvNumberInvalid" :: Text) <> toCBOR requestedSv <> toCBOR lastSeenSv
    ReqSnNotLeader{requestedSn, leader} ->
      toCBOR ("ReqSnNotLeader" :: Text) <> toCBOR requestedSn <> toCBOR leader
    ReqSnDecommitNotSettled ->
      toCBOR ("ReqSnDecommitNotSettled" :: Text)
    ReqSnCommitNotSettled ->
      toCBOR ("ReqSnCommitNotSettled" :: Text)
    InvalidMultisignature{multisig, vkeys} ->
      toCBOR ("InvalidMultisignature" :: Text) <> toCBOR multisig <> toCBOR vkeys
    SnapshotAlreadySigned{knownSignatures, receivedSignature} ->
      toCBOR ("SnapshotAlreadySigned" :: Text) <> toCBOR knownSignatures <> toCBOR receivedSignature
    AckSnNumberInvalid{requestedSn, lastSeenSn} ->
      toCBOR ("AckSnNumberInvalid" :: Text) <> toCBOR requestedSn <> toCBOR lastSeenSn
    SnapshotDoesNotApply{requestedSn, txid, error = validationError} ->
      toCBOR ("SnapshotDoesNotApply" :: Text) <> toCBOR requestedSn <> toCBOR txid <> toCBOR validationError
    NoMatchingDeposit ->
      toCBOR ("NoMatchingDeposit" :: Text)
    RequestedDepositExpired{depositTxId} ->
      toCBOR ("RequestedDepositExpired" :: Text) <> toCBOR depositTxId
    RequestedDepositNotFoundLocally{depositTxId} ->
      toCBOR ("RequestedDepositNotFoundLocally" :: Text) <> toCBOR depositTxId
    ReqSnUTxOSetTooLarge{utxoCount, maxAllowed} ->
      toCBOR ("ReqSnUTxOSetTooLarge" :: Text) <> toCBOR utxoCount <> toCBOR maxAllowed

instance IsTx tx => FromCBOR (RequirementFailure tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("ReqSnNumberInvalid" :: Text) -> ReqSnNumberInvalid <$> fromCBOR <*> fromCBOR
      "ReqSvNumberInvalid" -> ReqSvNumberInvalid <$> fromCBOR <*> fromCBOR
      "ReqSnNotLeader" -> ReqSnNotLeader <$> fromCBOR <*> fromCBOR
      "ReqSnDecommitNotSettled" -> pure ReqSnDecommitNotSettled
      "ReqSnCommitNotSettled" -> pure ReqSnCommitNotSettled
      "InvalidMultisignature" -> InvalidMultisignature <$> fromCBOR <*> fromCBOR
      "SnapshotAlreadySigned" -> SnapshotAlreadySigned <$> fromCBOR <*> fromCBOR
      "AckSnNumberInvalid" -> AckSnNumberInvalid <$> fromCBOR <*> fromCBOR
      "SnapshotDoesNotApply" -> SnapshotDoesNotApply <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "NoMatchingDeposit" -> pure NoMatchingDeposit
      "RequestedDepositExpired" -> RequestedDepositExpired <$> fromCBOR
      "RequestedDepositNotFoundLocally" -> RequestedDepositNotFoundLocally <$> fromCBOR
      "ReqSnUTxOSetTooLarge" -> ReqSnUTxOSetTooLarge <$> fromCBOR <*> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded RequirementFailure"

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

instance IsTx tx => ToCBOR (SideLoadRequirementFailure tx) where
  toCBOR = \case
    SideLoadInitialSnapshotMismatch ->
      toCBOR ("SideLoadInitialSnapshotMismatch" :: Text)
    SideLoadSnNumberInvalid{requestedSn, lastSeenSn} ->
      toCBOR ("SideLoadSnNumberInvalid" :: Text) <> toCBOR requestedSn <> toCBOR lastSeenSn
    SideLoadSvNumberInvalid{requestedSv, lastSeenSv} ->
      toCBOR ("SideLoadSvNumberInvalid" :: Text) <> toCBOR requestedSv <> toCBOR lastSeenSv
    SideLoadUTxOToCommitInvalid{requestedSc, lastSeenSc} ->
      toCBOR ("SideLoadUTxOToCommitInvalid" :: Text) <> toCBOR requestedSc <> toCBOR lastSeenSc
    SideLoadUTxOToDecommitInvalid{requestedSd, lastSeenSd} ->
      toCBOR ("SideLoadUTxOToDecommitInvalid" :: Text) <> toCBOR requestedSd <> toCBOR lastSeenSd
    SideLoadInvalidMultisignature{multisig, vkeys} ->
      toCBOR ("SideLoadInvalidMultisignature" :: Text) <> toCBOR multisig <> toCBOR vkeys

instance IsTx tx => FromCBOR (SideLoadRequirementFailure tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("SideLoadInitialSnapshotMismatch" :: Text) -> pure SideLoadInitialSnapshotMismatch
      "SideLoadSnNumberInvalid" -> SideLoadSnNumberInvalid <$> fromCBOR <*> fromCBOR
      "SideLoadSvNumberInvalid" -> SideLoadSvNumberInvalid <$> fromCBOR <*> fromCBOR
      "SideLoadUTxOToCommitInvalid" -> SideLoadUTxOToCommitInvalid <$> fromCBOR <*> fromCBOR
      "SideLoadUTxOToDecommitInvalid" -> SideLoadUTxOToDecommitInvalid <$> fromCBOR <*> fromCBOR
      "SideLoadInvalidMultisignature" -> SideLoadInvalidMultisignature <$> fromCBOR <*> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded SideLoadRequirementFailure"
