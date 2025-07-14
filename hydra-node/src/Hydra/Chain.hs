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
import Hydra.Cardano.Api (
  Address (..),
  AsType (AsAddress, AsByronAddr),
  ByronAddr,
  Coin (..),
  LedgerEra,
  deserialiseFromRawBytes,
  serialiseToRawBytes,
 )
import Hydra.Chain.ChainState (ChainSlot, IsChainState (..))
import Hydra.Tx (
  CommitBlueprintTx,
  ConfirmedSnapshot,
  HeadId,
  HeadParameters (..),
  HeadSeed,
  IsTx (..),
  Party,
  SnapshotNumber,
  SnapshotVersion,
  UTxOType,
 )
import Hydra.Tx.IsTx (ArbitraryIsTx)
import Hydra.Tx.OnChainId (OnChainId (..))
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.QuickCheck.Instances.Semigroup ()
import Test.QuickCheck.Instances.Time ()

-- | Hardcoded limit for commit tx on mainnet
maxMainnetLovelace :: Coin
maxMainnetLovelace = Coin 100_000_000

-- | Hardcoded limit for maximum number of parties in a head protocol The value
-- is obtained from calculating the costs of running the scripts and on-chan
-- validators (see 'computeCollectComCost' 'computeAbortCost'). A too high
-- enough number would be detected by property and acceptance tests.
maximumNumberOfParties :: Int
maximumNumberOfParties = 7

-- | Data type used to post transactions on chain. It holds everything to
-- construct corresponding Head protocol transactions.
data PostChainTx tx
  = InitTx {participants :: [OnChainId], headParameters :: HeadParameters}
  | AbortTx {utxo :: UTxOType tx, headSeed :: HeadSeed}
  | CollectComTx {utxo :: UTxOType tx, headId :: HeadId, headParameters :: HeadParameters}
  | IncrementTx
      { headId :: HeadId
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
      { headId :: HeadId
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
  | FanoutTx {utxo :: UTxOType tx, utxoToCommit :: Maybe (UTxOType tx), utxoToDecommit :: Maybe (UTxOType tx), headSeed :: HeadSeed, contestationDeadline :: UTCTime}
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (PostChainTx tx)
deriving stock instance IsTx tx => Show (PostChainTx tx)
deriving anyclass instance IsTx tx => ToJSON (PostChainTx tx)
deriving anyclass instance IsTx tx => FromJSON (PostChainTx tx)

instance IsTx tx => ToCBOR (PostChainTx tx) where
  toCBOR = \case
    InitTx participants headParameters ->
      toCBOR ("InitTx" :: Text) <> toCBOR participants <> toCBOR headParameters
    AbortTx utxo headSeed ->
      toCBOR ("AbortTx" :: Text) <> toCBOR utxo <> toCBOR headSeed
    CollectComTx utxo headId headParameters ->
      toCBOR ("CollectComTx" :: Text) <> toCBOR utxo <> toCBOR headId <> toCBOR headParameters
    IncrementTx headId headParameters incrementingSnapshot depositTxId ->
      toCBOR ("IncrementTx" :: Text) <> toCBOR headId <> toCBOR headParameters <> toCBOR incrementingSnapshot <> toCBOR depositTxId
    RecoverTx headId recoverTxId deadline recoverUTxO ->
      toCBOR ("RecoverTx" :: Text) <> toCBOR headId <> toCBOR recoverTxId <> toCBOR deadline <> toCBOR recoverUTxO
    DecrementTx headId headParameters decrementingSnapshot ->
      toCBOR ("DecrementTx" :: Text) <> toCBOR headId <> toCBOR headParameters <> toCBOR decrementingSnapshot
    CloseTx headId headParameters openVersion closingSnapshot ->
      toCBOR ("CloseTx" :: Text) <> toCBOR headId <> toCBOR headParameters <> toCBOR openVersion <> toCBOR closingSnapshot
    ContestTx headId headParameters openVersion contestingSnapshot ->
      toCBOR ("ContestTx" :: Text) <> toCBOR headId <> toCBOR headParameters <> toCBOR openVersion <> toCBOR contestingSnapshot
    FanoutTx utxo utxoToCommit utxoToDecommit headSeed contestationDeadline ->
      toCBOR ("FanoutTx" :: Text)
        <> toCBOR utxo
        <> toCBOR utxoToCommit
        <> toCBOR utxoToDecommit
        <> toCBOR headSeed
        <> toCBOR contestationDeadline

instance IsTx tx => FromCBOR (PostChainTx tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("InitTx" :: Text) -> InitTx <$> fromCBOR <*> fromCBOR
      ("AbortTx" :: Text) -> AbortTx <$> fromCBOR <*> fromCBOR
      ("CollectComTx" :: Text) -> CollectComTx <$> fromCBOR <*> fromCBOR <*> fromCBOR
      ("IncrementTx" :: Text) -> IncrementTx <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      ("RecoverTx" :: Text) -> RecoverTx <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      ("DecrementTx" :: Text) -> DecrementTx <$> fromCBOR <*> fromCBOR <*> fromCBOR
      ("CloseTx" :: Text) -> CloseTx <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      ("ContestTx" :: Text) -> ContestTx <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      ("FanoutTx" :: Text) ->
        FanoutTx <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      msg -> fail $ show msg <> " is not a proper CBOR-encoded Message"

instance ArbitraryIsTx tx => Arbitrary (PostChainTx tx) where
  arbitrary = genericArbitrary
  shrink = \case
    InitTx{participants, headParameters} -> InitTx <$> shrink participants <*> shrink headParameters
    AbortTx{utxo, headSeed} -> AbortTx <$> shrink utxo <*> shrink headSeed
    CollectComTx{utxo, headId, headParameters} -> CollectComTx <$> shrink utxo <*> shrink headId <*> shrink headParameters
    IncrementTx{headId, headParameters, incrementingSnapshot, depositTxId} ->
      IncrementTx <$> shrink headId <*> shrink headParameters <*> shrink incrementingSnapshot <*> shrink depositTxId
    RecoverTx{headId, recoverTxId, deadline, recoverUTxO} ->
      RecoverTx <$> shrink headId <*> shrink recoverTxId <*> shrink deadline <*> shrink recoverUTxO
    DecrementTx{headId, headParameters, decrementingSnapshot} -> DecrementTx <$> shrink headId <*> shrink headParameters <*> shrink decrementingSnapshot
    CloseTx{headId, headParameters, openVersion, closingSnapshot} -> CloseTx <$> shrink headId <*> shrink headParameters <*> shrink openVersion <*> shrink closingSnapshot
    ContestTx{headId, headParameters, openVersion, contestingSnapshot} -> ContestTx <$> shrink headId <*> shrink headParameters <*> shrink openVersion <*> shrink contestingSnapshot
    FanoutTx{utxo, utxoToCommit, utxoToDecommit, headSeed, contestationDeadline} -> FanoutTx <$> shrink utxo <*> shrink utxoToCommit <*> shrink utxoToDecommit <*> shrink headSeed <*> shrink contestationDeadline

-- | Describes transactions as seen on chain. Holds as minimal information as
-- possible to simplify observing the chain.
data OnChainTx tx
  = OnInitTx
      { headId :: HeadId
      , headSeed :: HeadSeed
      , headParameters :: HeadParameters
      , participants :: [OnChainId]
      }
  | OnCommitTx
      { headId :: HeadId
      , party :: Party
      , committed :: UTxOType tx
      }
  | OnAbortTx {headId :: HeadId}
  | OnCollectComTx {headId :: HeadId}
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
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (OnChainTx tx)
deriving stock instance IsTx tx => Show (OnChainTx tx)
deriving anyclass instance IsTx tx => ToJSON (OnChainTx tx)

instance IsTx tx => ToCBOR (OnChainTx tx) where
  toCBOR = \case
    OnInitTx headId headSeed headParameters participants ->
      toCBOR ("OnInitTx" :: Text) <> toCBOR headId <> toCBOR headSeed <> toCBOR headParameters <> toCBOR participants
    OnCommitTx headId party committed ->
      toCBOR ("OnCommitTx" :: Text) <> toCBOR headId <> toCBOR party <> toCBOR committed
    OnAbortTx headId ->
      toCBOR ("OnAbortTx" :: Text) <> toCBOR headId
    OnRecoverTx headId recoveredTxId recoveredUTxO ->
      toCBOR ("OnRecoverTx" :: Text) <> toCBOR headId <> toCBOR recoveredTxId <> toCBOR recoveredUTxO
    OnCollectComTx headId ->
      toCBOR ("OnCollectComTx" :: Text) <> toCBOR headId
    OnDepositTx headId depositTxId deposited created deadline ->
      toCBOR ("OnDepositTx" :: Text) <> toCBOR headId <> toCBOR depositTxId <> toCBOR deposited <> toCBOR created <> toCBOR deadline
    OnIncrementTx headId newVersion depositTxId ->
      toCBOR ("OnIncrementTx" :: Text) <> toCBOR headId <> toCBOR newVersion <> toCBOR depositTxId
    OnDecrementTx headId newVersion distributedUTxO ->
      toCBOR ("OnDecrementTx" :: Text) <> toCBOR headId <> toCBOR newVersion <> toCBOR distributedUTxO
    OnCloseTx headId snapshotNumber contestationDeadline ->
      toCBOR ("OnCloseTx" :: Text) <> toCBOR headId <> toCBOR snapshotNumber <> toCBOR contestationDeadline
    OnContestTx headId snapshotNumber contestationDeadline ->
      toCBOR ("OnContestTx" :: Text) <> toCBOR headId <> toCBOR snapshotNumber <> toCBOR contestationDeadline
    OnFanoutTx headId fanoutUTxO ->
      toCBOR ("OnFanoutTx" :: Text) <> toCBOR headId <> toCBOR fanoutUTxO

instance IsTx tx => FromCBOR (OnChainTx tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("OnInitTx" :: Text) -> OnInitTx <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      ("OnCommitTx" :: Text) -> OnCommitTx <$> fromCBOR <*> fromCBOR <*> fromCBOR
      ("OnAbortTx" :: Text) -> OnAbortTx <$> fromCBOR
      ("OnRecoverTx" :: Text) -> OnRecoverTx <$> fromCBOR <*> fromCBOR <*> fromCBOR
      ("OnCollectComTx" :: Text) -> OnCollectComTx <$> fromCBOR
      ("OnDepositComTx" :: Text) -> OnDepositTx <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      ("OnIncrementTx" :: Text) -> OnIncrementTx <$> fromCBOR <*> fromCBOR <*> fromCBOR
      ("OnDecrementTx" :: Text) -> OnDecrementTx <$> fromCBOR <*> fromCBOR <*> fromCBOR
      ("OnCloseTx" :: Text) -> OnCloseTx <$> fromCBOR <*> fromCBOR <*> fromCBOR
      ("OnContestTx" :: Text) -> OnContestTx <$> fromCBOR <*> fromCBOR <*> fromCBOR
      ("OnFanoutTx" :: Text) -> OnFanoutTx <$> fromCBOR <*> fromCBOR
      msg -> fail $ show msg <> " is not a proper CBOR-encoded Message"

instance ArbitraryIsTx tx => Arbitrary (OnChainTx tx) where
  arbitrary = genericArbitrary

-- | Exceptions thrown by 'postTx'.
data PostTxError tx
  = NoSeedInput
  | InvalidSeed {headSeed :: HeadSeed}
  | InvalidHeadId {headId :: HeadId}
  | CannotFindOwnInitial {knownUTxO :: UTxOType tx}
  | -- | Committing byron addresses is not supported.
    UnsupportedLegacyOutput {byronAddress :: Address ByronAddr}
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
  | -- | User tried to commit more than 'maxMainnetLovelace' hardcoded limit on mainnet
    -- we keep track of both the hardcoded limit and what the user originally tried to commit
    CommittedTooMuchADAForMainnet {userCommittedLovelace :: Coin, mainnetLimitLovelace :: Coin}
  | -- | We can only draft commit tx for the user when in Initializing state
    FailedToDraftTxNotInitializing
  | FailedToConstructAbortTx
  | FailedToConstructCloseTx
  | FailedToConstructContestTx
  | FailedToConstructCollectTx
  | FailedToConstructDepositTx {failureReason :: Text}
  | FailedToConstructRecoverTx {failureReason :: Text}
  | FailedToConstructIncrementTx {failureReason :: Text}
  | FailedToConstructDecrementTx {failureReason :: Text}
  | FailedToConstructFanoutTx
  | DepositTooLow {providedValue :: Coin, minimumValue :: Coin}
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (PostTxError tx)
deriving stock instance IsChainState tx => Show (PostTxError tx)
deriving anyclass instance IsChainState tx => ToJSON (PostTxError tx)
deriving anyclass instance IsChainState tx => FromJSON (PostTxError tx)

instance IsChainState tx => Exception (PostTxError tx)

instance ToCBOR (Address ByronAddr) where
  toCBOR = toCBOR . serialiseToRawBytes

instance FromCBOR (Address ByronAddr) where
  fromCBOR = do
    bs <- fromCBOR
    case deserialiseFromRawBytes (AsAddress AsByronAddr) bs of
      Left err -> fail (show err)
      Right v -> pure v

instance IsChainState tx => ToCBOR (PostTxError tx) where
  toCBOR = \case
    NoSeedInput -> toCBOR ("NoSeedInput" :: Text)
    InvalidSeed seed -> toCBOR ("InvalidSeed" :: Text) <> toCBOR seed
    InvalidHeadId headId -> toCBOR ("InvalidHeadId" :: Text) <> toCBOR headId
    CannotFindOwnInitial knownUTxO -> toCBOR ("CannotFindOwnInitial" :: Text) <> toCBOR knownUTxO
    UnsupportedLegacyOutput byronAddress -> toCBOR ("UnsupportedLegacyOutput" :: Text) <> toCBOR byronAddress
    InvalidStateToPost txTried chainState ->
      toCBOR ("InvalidStateToPost" :: Text) <> toCBOR txTried <> toCBOR chainState
    NotEnoughFuel failingTx -> toCBOR ("NotEnoughFuel" :: Text) <> toCBOR failingTx
    NoFuelUTXOFound failingTx -> toCBOR ("NoFuelUTXOFound" :: Text) <> toCBOR failingTx
    ScriptFailedInWallet redeemerPtr failureReason failingTx ->
      toCBOR ("ScriptFailedInWallet" :: Text) <> toCBOR redeemerPtr <> toCBOR failureReason <> toCBOR failingTx
    InternalWalletError headUTxO reason failingTx ->
      toCBOR ("InternalWalletError" :: Text) <> toCBOR headUTxO <> toCBOR reason <> toCBOR failingTx
    FailedToPostTx failureReason failingTx ->
      toCBOR ("FailedToPostTx" :: Text) <> toCBOR failureReason <> toCBOR failingTx
    CommittedTooMuchADAForMainnet userCommittedLovelace mainnetLimitLovelace ->
      toCBOR ("CommittedTooMuchADAForMainnet" :: Text)
        <> toCBOR userCommittedLovelace
        <> toCBOR mainnetLimitLovelace
    FailedToDraftTxNotInitializing -> toCBOR ("FailedToDraftTxNotInitializing" :: Text)
    FailedToConstructAbortTx -> toCBOR ("FailedToConstructAbortTx" :: Text)
    FailedToConstructCloseTx -> toCBOR ("FailedToConstructCloseTx" :: Text)
    FailedToConstructContestTx -> toCBOR ("FailedToConstructContestTx" :: Text)
    FailedToConstructCollectTx -> toCBOR ("FailedToConstructCollectTx" :: Text)
    FailedToConstructDepositTx failureReason ->
      toCBOR ("FailedToConstructDepositTx" :: Text) <> toCBOR failureReason
    FailedToConstructRecoverTx failureReason ->
      toCBOR ("FailedToConstructRecoverTx" :: Text) <> toCBOR failureReason
    FailedToConstructIncrementTx failureReason ->
      toCBOR ("FailedToConstructIncrementTx" :: Text) <> toCBOR failureReason
    FailedToConstructDecrementTx failureReason ->
      toCBOR ("FailedToConstructDecrementTx" :: Text) <> toCBOR failureReason
    FailedToConstructFanoutTx -> toCBOR ("FailedToConstructFanoutTx" :: Text)
    DepositTooLow providedValue minimumValue ->
      toCBOR ("DepositTooLow" :: Text) <> toCBOR providedValue <> toCBOR minimumValue

instance IsChainState tx => FromCBOR (PostTxError tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("NoSeedInput" :: Text) -> pure NoSeedInput
      ("InvalidSeed" :: Text) -> InvalidSeed <$> fromCBOR
      ("InvalidHeadId" :: Text) -> InvalidHeadId <$> fromCBOR
      ("CannotFindOwnInitial" :: Text) -> CannotFindOwnInitial <$> fromCBOR
      ("UnsupportedLegacyOutput" :: Text) -> UnsupportedLegacyOutput <$> fromCBOR
      ("InvalidStateToPost" :: Text) -> InvalidStateToPost <$> fromCBOR <*> fromCBOR
      ("NotEnoughFuel" :: Text) -> NotEnoughFuel <$> fromCBOR
      ("NoFuelUTXOFound" :: Text) -> NoFuelUTXOFound <$> fromCBOR
      ("ScriptFailedInWallet" :: Text) ->
        ScriptFailedInWallet <$> fromCBOR <*> fromCBOR <*> fromCBOR
      ("InternalWalletError" :: Text) ->
        InternalWalletError <$> fromCBOR <*> fromCBOR <*> fromCBOR
      ("FailedToPostTx" :: Text) -> FailedToPostTx <$> fromCBOR <*> fromCBOR
      ("CommittedTooMuchADAForMainnet" :: Text) ->
        CommittedTooMuchADAForMainnet <$> fromCBOR <*> fromCBOR
      ("FailedToDraftTxNotInitializing" :: Text) -> pure FailedToDraftTxNotInitializing
      ("FailedToConstructAbortTx" :: Text) -> pure FailedToConstructAbortTx
      ("FailedToConstructCloseTx" :: Text) -> pure FailedToConstructCloseTx
      ("FailedToConstructContestTx" :: Text) -> pure FailedToConstructContestTx
      ("FailedToConstructCollectTx" :: Text) -> pure FailedToConstructCollectTx
      ("FailedToConstructDepositTx" :: Text) -> FailedToConstructDepositTx <$> fromCBOR
      ("FailedToConstructRecoverTx" :: Text) -> FailedToConstructRecoverTx <$> fromCBOR
      ("FailedToConstructIncrementTx" :: Text) -> FailedToConstructIncrementTx <$> fromCBOR
      ("FailedToConstructDecrementTx" :: Text) -> FailedToConstructDecrementTx <$> fromCBOR
      ("FailedToConstructFanoutTx" :: Text) -> pure FailedToConstructFanoutTx
      ("DepositTooLow" :: Text) -> DepositTooLow <$> fromCBOR <*> fromCBOR
      msg -> fail $ show msg <> " is not a proper CBOR-encoded Message"

instance (ArbitraryIsTx tx, IsChainState tx) => Arbitrary (PostTxError tx) where
  arbitrary = genericArbitrary

-- | A non empty sequence of chain states that can be rolled back.
-- This is expected to be constructed by using the smart constructor
-- 'initHistory'.
data ChainStateHistory tx = UnsafeChainStateHistory
  { history :: NonEmpty (ChainStateType tx)
  , defaultChainState :: ChainStateType tx
  }
  deriving stock (Generic)

currentState :: ChainStateHistory tx -> ChainStateType tx
currentState UnsafeChainStateHistory{history} = head history

pushNewState :: ChainStateType tx -> ChainStateHistory tx -> ChainStateHistory tx
pushNewState cs h@UnsafeChainStateHistory{history} = h{history = cs <| history}

initHistory :: ChainStateType tx -> ChainStateHistory tx
initHistory cs = UnsafeChainStateHistory{history = cs :| [], defaultChainState = cs}

rollbackHistory :: IsChainState tx => ChainSlot -> ChainStateHistory tx -> ChainStateHistory tx
rollbackHistory rollbackChainSlot h@UnsafeChainStateHistory{history, defaultChainState} =
  h{history = fromMaybe (defaultChainState :| []) (nonEmpty rolledBack)}
 where
  rolledBack =
    dropWhile
      (\cs -> chainStateSlot cs > rollbackChainSlot)
      (toList history)

deriving stock instance Eq (ChainStateType tx) => Eq (ChainStateHistory tx)
deriving stock instance Show (ChainStateType tx) => Show (ChainStateHistory tx)

instance Arbitrary (ChainStateType tx) => Arbitrary (ChainStateHistory tx) where
  arbitrary = genericArbitrary

-- | Handle to interface with the main chain network
data Chain tx m = Chain
  { mkChainState :: ChainStateType tx
  -- ^ Provide an initial chain state that may be evolved through 'ChainEvent'.
  , postTx :: MonadThrow m => PostChainTx tx -> m ()
  -- ^ Construct and send a transaction to the main chain corresponding to the
  -- given 'PostChainTx' description.
  -- This function is not expected to block, so it is only responsible for
  -- submitting, but it should validate the created transaction against a
  -- reasonable local view of the chain and throw an exception when invalid.
  --
  -- Does at least throw 'PostTxError'.
  , draftCommitTx ::
      MonadThrow m =>
      HeadId ->
      CommitBlueprintTx tx ->
      m (Either (PostTxError tx) tx)
  -- ^ Create a commit transaction using user provided utxos (zero or many) and
  -- a _blueprint_ transaction which spends these outputs.
  -- Errors are handled at the call site.
  , draftDepositTx ::
      MonadThrow m =>
      HeadId ->
      PParams LedgerEra ->
      CommitBlueprintTx tx ->
      UTCTime ->
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
  }

data ChainEvent tx
  = -- | Indicates a head protocol transaction has been observed.
    Observation
      { observedTx :: OnChainTx tx
      , newChainState :: ChainStateType tx
      }
  | Rollback
      { rolledBackChainState :: ChainStateType tx
      }
  | -- | Indicate time has advanced on the chain.
    --
    -- NOTE: While the type does not guarantee that the UTCTime and ChainSlot
    -- are consistent the alternative would be provide the means to do the
    -- conversion. For Cardano, this would be a systemStart and eraHistory..
    -- which is annoying and if it's kept in the chain layer, it would mean
    -- another round trip / state to keep there.
    Tick
      { chainTime :: UTCTime
      , chainSlot :: ChainSlot
      }
  | -- | Event to re-ingest errors from 'postTx' for further processing.
    PostTxError {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx, failingTx :: Maybe tx}
  deriving stock (Generic)

deriving stock instance (IsTx tx, IsChainState tx) => Eq (ChainEvent tx)
deriving stock instance (IsTx tx, IsChainState tx) => Show (ChainEvent tx)
deriving anyclass instance (IsTx tx, IsChainState tx) => ToJSON (ChainEvent tx)

instance IsChainState tx => ToCBOR (ChainEvent tx) where
  toCBOR = \case
    Observation observed newState -> toCBOR ("Observation" :: Text) <> toCBOR observed <> toCBOR newState
    Rollback st -> toCBOR ("Rollback" :: Text) <> toCBOR st
    Tick time slot -> toCBOR ("Tick" :: Text) <> toCBOR time <> toCBOR slot
    PostTxError postChainTx postError tx -> toCBOR ("PostTxError" :: Text) <> toCBOR postChainTx <> toCBOR postError <> toCBOR tx

instance IsChainState tx => FromCBOR (ChainEvent tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("Observation" :: Text) -> Observation <$> fromCBOR <*> fromCBOR
      "Rollback" -> Rollback <$> fromCBOR
      "Tick" -> Tick <$> fromCBOR <*> fromCBOR
      "PostTxError" -> PostTxError <$> fromCBOR <*> fromCBOR <*> fromCBOR
      msg -> fail $ show msg <> " is not a proper CBOR-encoded Message"

instance (ArbitraryIsTx tx, IsChainState tx) => Arbitrary (ChainEvent tx) where
  arbitrary = genericArbitrary

-- | A callback indicating a 'ChainEvent tx' happened. Most importantly the
-- 'Observation' of a relevant Hydra transaction.
type ChainCallback tx m = ChainEvent tx -> m ()

-- | A type tying both posting and observing transactions into a single /Component/.
type ChainComponent tx m a = ChainCallback tx m -> (Chain tx m -> m a) -> m a
