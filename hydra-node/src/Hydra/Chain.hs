{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Specifies the /Head-Chain Interaction/ part of the protocol
--
-- Incoming and outgoing on-chain transactions are modelled respectively as `OnChainTx`
-- and `PostChainTx` which are data type that abstracts away the details of the structure
-- of the transaction.
module Hydra.Chain where

import Hydra.Prelude

import Cardano.Ledger.Core (PParams)
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NE
import Hydra.Cardano.Api (
  Address,
  AddressInEra,
  ByronAddr,
  Coin (..),
  LedgerEra,
  PolicyAssets,
  PolicyId,
  Value,
 )
import Hydra.Chain.ChainState (ChainSlot, IsChainState (..), chainStateSlot)
import Hydra.Tx (
  CommitBlueprintTx,
  ConfirmedSnapshot,
  HeadId,
  HeadParameters (..),
  HeadSeed,
  IsTx (..),
  SnapshotNumber,
  SnapshotVersion,
  UTxOType,
 )
import Hydra.Tx.OnChainId (OnChainId)

-- | Hardcoded limit for maximum number of parties in a head protocol. A too
-- high number would be detected by property and acceptance tests.
maximumNumberOfParties :: Int
maximumNumberOfParties = 29

-- | Data type used to post transactions on chain. It holds everything to
-- construct corresponding Head protocol transactions.
-- TODO: somehow merge HeadSeed/HeadId
data PostChainTx tx
  = InitTx {participants :: [OnChainId], headParameters :: HeadParameters}
  | IncrementTx
      { headSeed :: HeadSeed
      , headId :: HeadId
      , headParameters :: HeadParameters
      , incrementingSnapshot :: ConfirmedSnapshot tx
      , depositTxId :: TxIdType tx
      }
  | RecoverTx
      { headId :: HeadId
      , recoverTxId :: TxIdType tx
      , deadline :: ChainSlot
      , recoverUTxO :: UTxOType tx
      }
  | DecrementTx
      { headSeed :: HeadSeed
      , headId :: HeadId
      , headParameters :: HeadParameters
      , decrementingSnapshot :: ConfirmedSnapshot tx
      }
  | CloseTx
      { headId :: HeadId
      , headParameters :: HeadParameters
      , openVersion :: SnapshotVersion
      , closingSnapshot :: ConfirmedSnapshot tx
      }
  | ContestTx
      { headId :: HeadId
      , headParameters :: HeadParameters
      , openVersion :: SnapshotVersion
      , contestingSnapshot :: ConfirmedSnapshot tx
      }
  | FanoutTx
      { utxo :: UTxOType tx
      , utxoToCommit :: Maybe (UTxOType tx)
      , utxoToDecommit :: Maybe (UTxOType tx)
      , utxoForProof :: UTxOType tx
      , headSeed :: HeadSeed
      , contestationDeadline :: UTCTime
      }
  | FinalPartialFanoutTx
      { utxoToDistribute :: UTxOType tx
      , presettledUTxO :: UTxOType tx
      , headSeed :: HeadSeed
      , contestationDeadline :: UTCTime
      }
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (PostChainTx tx)
deriving stock instance IsTx tx => Show (PostChainTx tx)
deriving anyclass instance IsTx tx => ToJSON (PostChainTx tx)
deriving anyclass instance IsTx tx => FromJSON (PostChainTx tx)

instance IsTx tx => ToCBOR (PostChainTx tx) where
  toCBOR = \case
    InitTx{participants, headParameters} ->
      toCBOR ("InitTx" :: Text) <> toCBOR participants <> toCBOR headParameters
    IncrementTx{headSeed, headId, headParameters, incrementingSnapshot, depositTxId} ->
      toCBOR ("IncrementTx" :: Text)
        <> toCBOR headSeed
        <> toCBOR headId
        <> toCBOR headParameters
        <> toCBOR incrementingSnapshot
        <> toCBOR depositTxId
    RecoverTx{headId, recoverTxId, deadline, recoverUTxO} ->
      toCBOR ("RecoverTx" :: Text)
        <> toCBOR headId
        <> toCBOR recoverTxId
        <> toCBOR deadline
        <> toCBOR recoverUTxO
    DecrementTx{headSeed, headId, headParameters, decrementingSnapshot} ->
      toCBOR ("DecrementTx" :: Text)
        <> toCBOR headSeed
        <> toCBOR headId
        <> toCBOR headParameters
        <> toCBOR decrementingSnapshot
    CloseTx{headId, headParameters, openVersion, closingSnapshot} ->
      toCBOR ("CloseTx" :: Text)
        <> toCBOR headId
        <> toCBOR headParameters
        <> toCBOR openVersion
        <> toCBOR closingSnapshot
    ContestTx{headId, headParameters, openVersion, contestingSnapshot} ->
      toCBOR ("ContestTx" :: Text)
        <> toCBOR headId
        <> toCBOR headParameters
        <> toCBOR openVersion
        <> toCBOR contestingSnapshot
    FanoutTx{utxo, utxoToCommit, utxoToDecommit, utxoForProof, headSeed, contestationDeadline} ->
      toCBOR ("FanoutTx" :: Text)
        <> toCBOR utxo
        <> toCBOR utxoToCommit
        <> toCBOR utxoToDecommit
        <> toCBOR utxoForProof
        <> toCBOR headSeed
        <> toCBOR contestationDeadline
    FinalPartialFanoutTx{utxoToDistribute, presettledUTxO, headSeed, contestationDeadline} ->
      toCBOR ("FinalPartialFanoutTx" :: Text)
        <> toCBOR utxoToDistribute
        <> toCBOR presettledUTxO
        <> toCBOR headSeed
        <> toCBOR contestationDeadline

instance IsTx tx => FromCBOR (PostChainTx tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("InitTx" :: Text) -> InitTx <$> fromCBOR <*> fromCBOR
      "IncrementTx" -> IncrementTx <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "RecoverTx" -> RecoverTx <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "DecrementTx" -> DecrementTx <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "CloseTx" -> CloseTx <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "ContestTx" -> ContestTx <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "FanoutTx" -> FanoutTx <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "FinalPartialFanoutTx" -> FinalPartialFanoutTx <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded PostChainTx"

-- | Describes transactions as seen on chain. Holds as minimal information as
-- possible to simplify observing the chain.
data OnChainTx tx
  = OnInitTx
      { headId :: HeadId
      , headSeed :: HeadSeed
      , headParameters :: HeadParameters
      , participants :: [OnChainId]
      }
  | OnDepositTx
      { headId :: HeadId
      , depositTxId :: TxIdType tx
      , deposited :: UTxOType tx
      , created :: UTCTime
      , deadline :: UTCTime
      }
  | OnRecoverTx
      { headId :: HeadId
      , recoveredTxId :: TxIdType tx
      , recoveredUTxO :: UTxOType tx
      }
  | OnIncrementTx
      { headId :: HeadId
      , newVersion :: SnapshotVersion
      , depositTxId :: TxIdType tx
      }
  | OnDecrementTx
      { headId :: HeadId
      , newVersion :: SnapshotVersion
      , distributedUTxO :: UTxOType tx
      }
  | OnCloseTx
      { headId :: HeadId
      , snapshotNumber :: SnapshotNumber
      , contestationDeadline :: UTCTime
      }
  | OnContestTx
      { headId :: HeadId
      , snapshotNumber :: SnapshotNumber
      , contestationDeadline :: UTCTime
      }
  | OnFanoutTx {headId :: HeadId, fanoutUTxO :: UTxOType tx}
  | OnPartialFanoutTx {headId :: HeadId, distributedOutputs :: UTxOType tx}
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (OnChainTx tx)
deriving stock instance IsTx tx => Show (OnChainTx tx)
deriving anyclass instance IsTx tx => ToJSON (OnChainTx tx)
deriving anyclass instance IsTx tx => FromJSON (OnChainTx tx)

instance IsTx tx => ToCBOR (OnChainTx tx) where
  toCBOR = \case
    OnInitTx{headId, headSeed, headParameters, participants} ->
      toCBOR ("OnInitTx" :: Text)
        <> toCBOR headId
        <> toCBOR headSeed
        <> toCBOR headParameters
        <> toCBOR participants
    OnDepositTx{headId, depositTxId, deposited, created, deadline} ->
      toCBOR ("OnDepositTx" :: Text)
        <> toCBOR headId
        <> toCBOR depositTxId
        <> toCBOR deposited
        <> toCBOR created
        <> toCBOR deadline
    OnRecoverTx{headId, recoveredTxId, recoveredUTxO} ->
      toCBOR ("OnRecoverTx" :: Text)
        <> toCBOR headId
        <> toCBOR recoveredTxId
        <> toCBOR recoveredUTxO
    OnIncrementTx{headId, newVersion, depositTxId} ->
      toCBOR ("OnIncrementTx" :: Text)
        <> toCBOR headId
        <> toCBOR newVersion
        <> toCBOR depositTxId
    OnDecrementTx{headId, newVersion, distributedUTxO} ->
      toCBOR ("OnDecrementTx" :: Text)
        <> toCBOR headId
        <> toCBOR newVersion
        <> toCBOR distributedUTxO
    OnCloseTx{headId, snapshotNumber, contestationDeadline} ->
      toCBOR ("OnCloseTx" :: Text)
        <> toCBOR headId
        <> toCBOR snapshotNumber
        <> toCBOR contestationDeadline
    OnContestTx{headId, snapshotNumber, contestationDeadline} ->
      toCBOR ("OnContestTx" :: Text)
        <> toCBOR headId
        <> toCBOR snapshotNumber
        <> toCBOR contestationDeadline
    OnFanoutTx{headId, fanoutUTxO} ->
      toCBOR ("OnFanoutTx" :: Text) <> toCBOR headId <> toCBOR fanoutUTxO
    OnPartialFanoutTx{headId, distributedOutputs} ->
      toCBOR ("OnPartialFanoutTx" :: Text) <> toCBOR headId <> toCBOR distributedOutputs

instance IsTx tx => FromCBOR (OnChainTx tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("OnInitTx" :: Text) -> OnInitTx <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "OnDepositTx" -> OnDepositTx <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "OnRecoverTx" -> OnRecoverTx <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "OnIncrementTx" -> OnIncrementTx <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "OnDecrementTx" -> OnDecrementTx <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "OnCloseTx" -> OnCloseTx <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "OnContestTx" -> OnContestTx <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "OnFanoutTx" -> OnFanoutTx <$> fromCBOR <*> fromCBOR
      "OnPartialFanoutTx" -> OnPartialFanoutTx <$> fromCBOR <*> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded OnChainTx"

-- | Exceptions thrown by 'postTx'.
data PostTxError tx
  = NoSeedInput
  | InvalidSeed {headSeed :: HeadSeed}
  | InvalidHeadId {headId :: HeadId}
  | -- | Committing byron addresses is not supported.
    UnsupportedLegacyOutput {byronAddress :: Address ByronAddr}
  | DepositTooLow {providedValue :: Coin, minimumValue :: Coin}
  | InvalidStateToPost {txTried :: PostChainTx tx, chainState :: ChainStateType tx}
  | NotEnoughFuel {failingTx :: tx}
  | NoFuelUTXOFound {failingTx :: tx}
  | -- | Script execution failed when finalizing a transaction in the wallet.
    -- XXX: Ideally we want a cardano-api type with corresponding JSON instance
    -- here. But the wallet still uses ledger types and we don't want to copy the
    -- conversion from ledger 'TransactionScriptFailure' to the cardano-api
    -- 'ScriptExecutionError' type.
    ScriptFailedInWallet {redeemerPtr :: Text, failureReason :: Text, failingTx :: tx}
  | -- | A generic error happened when finalizing a transaction in the wallet.
    InternalWalletError {headUTxO :: UTxOType tx, reason :: Text, failingTx :: tx}
  | -- | An error occurred when submitting a transaction to the cardano-node.
    FailedToPostTx {failureReason :: Text, failingTx :: tx}
  | FailedToConstructCloseTx
  | FailedToConstructContestTx
  | FailedToConstructDepositTx {failureReason :: Text}
  | FailedToConstructRecoverTx {failureReason :: Text}
  | FailedToConstructIncrementTx {failureReason :: Text}
  | FailedToConstructDecrementTx {failureReason :: Text}
  | FailedToConstructFanoutTx
  | FailedToConstructPartialFanoutTx
  | -- | Another node already posted this partial fanout step; the chain
    -- observation loop will emit the correct next step automatically.
    StalePartialFanoutTx
  | ContestationDeadlineOutsideTimeHorizon {failureReason :: Text}
  | InvalidTokenRequest [(PolicyId, PolicyAssets)]
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (PostTxError tx)
deriving stock instance IsChainState tx => Show (PostTxError tx)
deriving anyclass instance IsChainState tx => ToJSON (PostTxError tx)
deriving anyclass instance IsChainState tx => FromJSON (PostTxError tx)

instance IsChainState tx => Exception (PostTxError tx)

instance IsChainState tx => ToCBOR (PostTxError tx) where
  toCBOR = \case
    NoSeedInput ->
      toCBOR ("NoSeedInput" :: Text)
    InvalidSeed{headSeed} ->
      toCBOR ("InvalidSeed" :: Text) <> toCBOR headSeed
    InvalidHeadId{headId} ->
      toCBOR ("InvalidHeadId" :: Text) <> toCBOR headId
    UnsupportedLegacyOutput{byronAddress} ->
      toCBOR ("UnsupportedLegacyOutput" :: Text) <> toCBOR byronAddress
    DepositTooLow{providedValue, minimumValue} ->
      toCBOR ("DepositTooLow" :: Text) <> toCBOR providedValue <> toCBOR minimumValue
    InvalidStateToPost{txTried, chainState} ->
      toCBOR ("InvalidStateToPost" :: Text) <> toCBOR txTried <> toCBOR chainState
    NotEnoughFuel{failingTx} ->
      toCBOR ("NotEnoughFuel" :: Text) <> toCBOR failingTx
    NoFuelUTXOFound{failingTx} ->
      toCBOR ("NoFuelUTXOFound" :: Text) <> toCBOR failingTx
    ScriptFailedInWallet{redeemerPtr, failureReason, failingTx} ->
      toCBOR ("ScriptFailedInWallet" :: Text)
        <> toCBOR redeemerPtr
        <> toCBOR failureReason
        <> toCBOR failingTx
    InternalWalletError{headUTxO, reason, failingTx} ->
      toCBOR ("InternalWalletError" :: Text)
        <> toCBOR headUTxO
        <> toCBOR reason
        <> toCBOR failingTx
    FailedToPostTx{failureReason, failingTx} ->
      toCBOR ("FailedToPostTx" :: Text) <> toCBOR failureReason <> toCBOR failingTx
    FailedToConstructCloseTx ->
      toCBOR ("FailedToConstructCloseTx" :: Text)
    FailedToConstructContestTx ->
      toCBOR ("FailedToConstructContestTx" :: Text)
    FailedToConstructDepositTx{failureReason} ->
      toCBOR ("FailedToConstructDepositTx" :: Text) <> toCBOR failureReason
    FailedToConstructRecoverTx{failureReason} ->
      toCBOR ("FailedToConstructRecoverTx" :: Text) <> toCBOR failureReason
    FailedToConstructIncrementTx{failureReason} ->
      toCBOR ("FailedToConstructIncrementTx" :: Text) <> toCBOR failureReason
    FailedToConstructDecrementTx{failureReason} ->
      toCBOR ("FailedToConstructDecrementTx" :: Text) <> toCBOR failureReason
    FailedToConstructFanoutTx ->
      toCBOR ("FailedToConstructFanoutTx" :: Text)
    FailedToConstructPartialFanoutTx ->
      toCBOR ("FailedToConstructPartialFanoutTx" :: Text)
    StalePartialFanoutTx ->
      toCBOR ("StalePartialFanoutTx" :: Text)
    ContestationDeadlineOutsideTimeHorizon{failureReason} ->
      toCBOR ("ContestationDeadlineOutsideTimeHorizon" :: Text) <> toCBOR failureReason
    InvalidTokenRequest tokens ->
      toCBOR ("InvalidTokenRequest" :: Text) <> toCBOR tokens

instance IsChainState tx => FromCBOR (PostTxError tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("NoSeedInput" :: Text) -> pure NoSeedInput
      "InvalidSeed" -> InvalidSeed <$> fromCBOR
      "InvalidHeadId" -> InvalidHeadId <$> fromCBOR
      "UnsupportedLegacyOutput" -> UnsupportedLegacyOutput <$> fromCBOR
      "DepositTooLow" -> DepositTooLow <$> fromCBOR <*> fromCBOR
      "InvalidStateToPost" -> InvalidStateToPost <$> fromCBOR <*> fromCBOR
      "NotEnoughFuel" -> NotEnoughFuel <$> fromCBOR
      "NoFuelUTXOFound" -> NoFuelUTXOFound <$> fromCBOR
      "ScriptFailedInWallet" -> ScriptFailedInWallet <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "InternalWalletError" -> InternalWalletError <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "FailedToPostTx" -> FailedToPostTx <$> fromCBOR <*> fromCBOR
      "FailedToConstructCloseTx" -> pure FailedToConstructCloseTx
      "FailedToConstructContestTx" -> pure FailedToConstructContestTx
      "FailedToConstructDepositTx" -> FailedToConstructDepositTx <$> fromCBOR
      "FailedToConstructRecoverTx" -> FailedToConstructRecoverTx <$> fromCBOR
      "FailedToConstructIncrementTx" -> FailedToConstructIncrementTx <$> fromCBOR
      "FailedToConstructDecrementTx" -> FailedToConstructDecrementTx <$> fromCBOR
      "FailedToConstructFanoutTx" -> pure FailedToConstructFanoutTx
      "FailedToConstructPartialFanoutTx" -> pure FailedToConstructPartialFanoutTx
      "StalePartialFanoutTx" -> pure StalePartialFanoutTx
      "ContestationDeadlineOutsideTimeHorizon" -> ContestationDeadlineOutsideTimeHorizon <$> fromCBOR
      "InvalidTokenRequest" -> InvalidTokenRequest <$> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded PostTxError"

-- | A non empty sequence of chain states that can be rolled back.
-- This is expected to be constructed by using the smart constructor
-- 'initHistory'.
data ChainStateHistory tx = UnsafeChainStateHistory
  { history :: NonEmpty (ChainStateType tx)
  -- ^ The sequence of known chain states, ordered from most recent to oldest.
  -- These contain notable state observed on-chain to be able to interact with
  -- Hydra heads.
  , lastKnown :: ChainPointType tx
  -- ^ The last known chain point, which may be used to continue observing the
  -- chain.
  , defaultChainState :: ChainStateType tx
  -- ^ The default chain state to fall back to when rolling back beyond known
  -- history.
  }
  deriving stock (Generic)

-- Fetches the last updated chain state from history.
currentState :: ChainStateHistory tx -> ChainStateType tx
currentState UnsafeChainStateHistory{history} = head history

-- | Record a new chain state in history. Also ensures the 'lastKnown' point is
-- updated accordingly.
pushNewState :: IsChainState tx => ChainStateType tx -> ChainStateHistory tx -> ChainStateHistory tx
pushNewState cs h@UnsafeChainStateHistory{history, lastKnown} =
  h
    { history = cs <| history
    , lastKnown = max lastKnown (chainStatePoint cs)
    }

-- | Update the last known chain point. Use 'pushNewState' if you have a full 'ChainStateType tx'.
setLastKnown :: ChainPointType tx -> ChainStateHistory tx -> ChainStateHistory tx
setLastKnown cp h = h{lastKnown = cp}

initHistory :: IsChainState tx => ChainStateType tx -> ChainStateHistory tx
initHistory cs =
  UnsafeChainStateHistory
    { history = cs :| []
    , lastKnown = chainStatePoint cs
    , defaultChainState = cs
    }

rollbackHistory :: IsChainState tx => ChainSlot -> ChainStateHistory tx -> ChainStateHistory tx
rollbackHistory rollbackChainSlot h@UnsafeChainStateHistory{history, defaultChainState} =
  h
    { history = rolledBack
    , lastKnown = chainStatePoint (head rolledBack)
    }
 where
  rolledBack =
    fromMaybe (defaultChainState :| []) . nonEmpty $
      NE.dropWhile
        (\cs -> chainStateSlot cs > rollbackChainSlot)
        history

-- | Get the known prefix of all the ChainStateHistory.
prefixOf :: IsChainState tx => ChainStateHistory tx -> NonEmpty (ChainPointType tx)
prefixOf ch@UnsafeChainStateHistory{history, lastKnown}
  | lastKnown == chainStatePoint (currentState ch) = historyPoints
  | otherwise = lastKnown <| historyPoints
 where
  historyPoints = chainStatePoint <$> history

deriving stock instance
  ( Eq (ChainPointType tx)
  , Eq (ChainStateType tx)
  ) =>
  Eq (ChainStateHistory tx)

deriving stock instance
  ( Show (ChainPointType tx)
  , Show (ChainStateType tx)
  ) =>
  Show (ChainStateHistory tx)

-- | Handle to interface with the main chain network
data Chain tx m = Chain
  { postTx :: MonadThrow m => PostChainTx tx -> m ()
  -- ^ Construct and send a transaction to the main chain corresponding to the
  -- given 'PostChainTx' description.
  -- This function is not expected to block, so it is only responsible for
  -- submitting, but it should validate the created transaction against a
  -- reasonable local view of the chain and throw an exception when invalid.
  --
  -- Does at least throw 'PostTxError'.
  , draftDepositTx ::
      MonadThrow m =>
      HeadId ->
      PParams LedgerEra ->
      CommitBlueprintTx tx ->
      UTCTime ->
      Maybe AddressInEra ->
      m (Either (PostTxError tx) tx)
  -- ^ Create a deposit transaction using user provided utxos (zero or many) ,
  -- _blueprint_ transaction which spends these outputs and a deadline for
  -- their inclusion into L2. Errors are handled at the call site.
  , submitTx :: MonadThrow m => tx -> m ()
  -- ^ Submit a cardano transaction.
  --
  -- Throws at least 'PostTxError'.
  --
  -- XXX: While technically they could be any of 'PostTxError tx', only
  -- `FailedToPostTx` errors are expected here.
  , checkNonADAAssets :: ConfirmedSnapshot tx -> Either Value ()
  }

data ChainEvent tx
  = -- | Indicates a head protocol transaction has been observed.
    Observation
      { observedTx :: OnChainTx tx
      , newChainState :: ChainStateType tx
      }
  | Rollback
      { chainTime :: UTCTime
      , rolledBackChainState :: ChainStateType tx
      }
  | -- | Indicate time has advanced on the chain. This is deliberately not a
    -- ChainStateType because state updates are only expected upon 'Observation'
    -- or'Rollback'.
    --
    -- NOTE: While the type does not guarantee that the UTCTime and the slot in
    -- ChainPointType tx are consistent the alternative would be provide the
    -- means to do the conversion. For Cardano, this would be a systemStart and
    -- eraHistory.. which is annoying and if it's kept in the chain layer, it
    -- would mean another round trip / state to keep there.
    Tick
      { chainTime :: UTCTime
      , chainPoint :: ChainPointType tx
      }
  | -- | Event to re-ingest errors from 'postTx' for further processing.
    PostTxError {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx, failingTx :: Maybe tx}
  deriving stock (Generic)

deriving stock instance (IsTx tx, IsChainState tx) => Eq (ChainEvent tx)
deriving stock instance (IsTx tx, IsChainState tx) => Show (ChainEvent tx)
deriving anyclass instance (IsTx tx, IsChainState tx) => ToJSON (ChainEvent tx)
deriving anyclass instance (IsTx tx, IsChainState tx) => FromJSON (ChainEvent tx)

instance IsChainState tx => ToCBOR (ChainEvent tx) where
  toCBOR = \case
    Observation{observedTx, newChainState} ->
      toCBOR ("Observation" :: Text) <> toCBOR observedTx <> toCBOR newChainState
    Rollback{chainTime, rolledBackChainState} ->
      toCBOR ("Rollback" :: Text) <> toCBOR chainTime <> toCBOR rolledBackChainState
    Tick{chainTime, chainPoint} ->
      toCBOR ("Tick" :: Text) <> toCBOR chainTime <> toCBOR chainPoint
    PostTxError{postChainTx, postTxError, failingTx} ->
      toCBOR ("PostTxError" :: Text)
        <> toCBOR postChainTx
        <> toCBOR postTxError
        <> toCBOR failingTx

instance IsChainState tx => FromCBOR (ChainEvent tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("Observation" :: Text) -> Observation <$> fromCBOR <*> fromCBOR
      "Rollback" -> Rollback <$> fromCBOR <*> fromCBOR
      "Tick" -> Tick <$> fromCBOR <*> fromCBOR
      "PostTxError" -> PostTxError <$> fromCBOR <*> fromCBOR <*> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded ChainEvent"

-- | A callback indicating a 'ChainEvent tx' happened. Most importantly the
-- 'Observation' of a relevant Hydra transaction.
type ChainCallback tx m = ChainEvent tx -> m ()

-- | A type tying both posting and observing transactions into a single /Component/.
type ChainComponent tx m a = ChainCallback tx m -> (Chain tx m -> m a) -> m a
