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

import "hydra-prelude" Hydra.Prelude
import "base" Data.List.NonEmpty ((<|))
import "base" Data.List.NonEmpty qualified as NE
import "cardano-ledger-core" Cardano.Ledger.Core (PParams)
import "hydra-cardano-api" Hydra.Cardano.Api (
  Address,
  AddressInEra,
  ByronAddr,
  Coin (..),
  LedgerEra,
  PolicyAssets,
  PolicyId,
  Value,
 )
import "hydra-tx" Hydra.Chain.ChainState (ChainSlot, IsChainState (..), chainStateSlot)
import "hydra-tx" Hydra.Tx (
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
import "hydra-tx" Hydra.Tx.OnChainId (OnChainId)

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
deriving anyclass instance IsTx tx => FromJSON (OnChainTx tx)

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
  | AmountTooLow {providedValue :: Coin, totalUTxOValue :: Coin}
  | InvalidTokenRequest [(PolicyId, PolicyAssets)]
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (PostTxError tx)
deriving stock instance IsChainState tx => Show (PostTxError tx)
deriving anyclass instance IsChainState tx => ToJSON (PostTxError tx)
deriving anyclass instance IsChainState tx => FromJSON (PostTxError tx)

instance IsChainState tx => Exception (PostTxError tx)

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

-- | A callback indicating a 'ChainEvent tx' happened. Most importantly the
-- 'Observation' of a relevant Hydra transaction.
type ChainCallback tx m = ChainEvent tx -> m ()

-- | A type tying both posting and observing transactions into a single /Component/.
type ChainComponent tx m a = ChainCallback tx m -> (Chain tx m -> m a) -> m a
