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

import Data.List (nub)
import Data.List.NonEmpty ((<|))
import Hydra.Cardano.Api (
  Address,
  ByronAddr,
  CtxUTxO,
  Lovelace (..),
  Tx,
  TxOut,
  UTxO',
  WitCtxTxIn,
  Witness,
 )
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.HeadId (HeadId, HeadSeed)
import Hydra.Ledger (ChainSlot, IsTx, TxIdType, UTxOType)
import Hydra.OnChainId (OnChainId)
import Hydra.Party (Party)
import Hydra.Snapshot (ConfirmedSnapshot, SnapshotNumber)
import Test.QuickCheck (scale, suchThat)
import Test.QuickCheck.Instances.Semigroup ()
import Test.QuickCheck.Instances.Time ()

-- | Hardcoded limit for commit tx on mainnet
maxMainnetLovelace :: Lovelace
maxMainnetLovelace = 100_000_000

-- | Hardcoded limit for maximum number of parties in a head protocol
-- The value is obtained from calculating the costs of running the scripts
-- and on-chan validators (see 'computeCollectComCost' 'computeAbortCost')
maximumNumberOfParties :: Int
maximumNumberOfParties = 5

-- | Contains the head's parameters as established in the initial transaction.
data HeadParameters = HeadParameters
  { contestationPeriod :: ContestationPeriod
  , parties :: [Party] -- NOTE(SN): The order of this list is important for leader selection.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary HeadParameters where
  arbitrary = dedupParties <$> genericArbitrary
   where
    dedupParties HeadParameters{contestationPeriod, parties} =
      HeadParameters{contestationPeriod, parties = nub parties}

-- | Data type used to post transactions on chain. It holds everything to
-- construct corresponding Head protocol transactions.
data PostChainTx tx
  = InitTx {participants :: [OnChainId], headParameters :: HeadParameters}
  | AbortTx {utxo :: UTxOType tx, headSeed :: HeadSeed}
  | CollectComTx {utxo :: UTxOType tx, headId :: HeadId, headParameters :: HeadParameters}
  | CloseTx {headId :: HeadId, headParameters :: HeadParameters, confirmedSnapshot :: ConfirmedSnapshot tx}
  | ContestTx {headId :: HeadId, headParameters :: HeadParameters, confirmedSnapshot :: ConfirmedSnapshot tx}
  | FanoutTx {utxo :: UTxOType tx, headSeed :: HeadSeed, contestationDeadline :: UTCTime}
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (PostChainTx tx)
deriving stock instance IsTx tx => Show (PostChainTx tx)
deriving anyclass instance IsTx tx => ToJSON (PostChainTx tx)
deriving anyclass instance IsTx tx => FromJSON (PostChainTx tx)

instance IsTx tx => Arbitrary (PostChainTx tx) where
  arbitrary = genericArbitrary
  shrink = \case
    InitTx{participants, headParameters} -> InitTx <$> shrink participants <*> shrink headParameters
    AbortTx{utxo, headSeed} -> AbortTx <$> shrink utxo <*> shrink headSeed
    CollectComTx{utxo, headId, headParameters} -> CollectComTx <$> shrink utxo <*> shrink headId <*> shrink headParameters
    CloseTx{headId, headParameters, confirmedSnapshot} -> CloseTx <$> shrink headId <*> shrink headParameters <*> shrink confirmedSnapshot
    ContestTx{headId, headParameters, confirmedSnapshot} -> ContestTx <$> shrink headId <*> shrink headParameters <*> shrink confirmedSnapshot
    FanoutTx{utxo, headSeed, contestationDeadline} -> FanoutTx <$> shrink utxo <*> shrink headSeed <*> shrink contestationDeadline

-- | Describes transactions as seen on chain. Holds as minimal information as
-- possible to simplify observing the chain.
data OnChainTx tx
  = OnInitTx
      { headId :: HeadId
      , headSeed :: HeadSeed
      , headParameters :: HeadParameters
      , participants :: [OnChainId]
      }
  | OnCommitTx {headId :: HeadId, party :: Party, committed :: UTxOType tx}
  | OnAbortTx {headId :: HeadId}
  | OnCollectComTx {headId :: HeadId}
  | OnCloseTx
      { headId :: HeadId
      , snapshotNumber :: SnapshotNumber
      , contestationDeadline :: UTCTime
      }
  | OnContestTx {headId :: HeadId, snapshotNumber :: SnapshotNumber}
  | OnFanoutTx {headId :: HeadId}
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (OnChainTx tx)
deriving stock instance IsTx tx => Show (OnChainTx tx)
deriving anyclass instance IsTx tx => ToJSON (OnChainTx tx)
deriving anyclass instance IsTx tx => FromJSON (OnChainTx tx)

instance (Arbitrary tx, Arbitrary (UTxOType tx)) => Arbitrary (OnChainTx tx) where
  arbitrary = genericArbitrary

-- | Exceptions thrown by 'postTx'.
data PostTxError tx
  = NoSeedInput
  | InvalidSeed {headSeed :: HeadSeed}
  | InvalidHeadId {headId :: HeadId}
  | CannotFindOwnInitial {knownUTxO :: UTxOType tx}
  | -- | Comitting byron addresses is not supported.
    UnsupportedLegacyOutput {byronAddress :: Address ByronAddr}
  | -- | Comitting reference scripts is not supported right now.
    CannotCommitReferenceScript
  | InvalidStateToPost {txTried :: PostChainTx tx, chainState :: ChainStateType tx}
  | NotEnoughFuel
  | NoFuelUTXOFound
  | -- | Script execution failed when finalizing a transaction in the wallet.
    -- XXX: Ideally we want a cardano-api type with corresonding JSON instance
    -- here. But the wallet still uses ledger types and we don't want to copy the
    -- conversion from ledger 'TransactionScriptFailure' to the cardano-api
    -- 'ScriptExecutionError' type.
    ScriptFailedInWallet {redeemerPtr :: Text, failureReason :: Text}
  | -- | A generic error happened when finalizing a transction in the wallet.
    InternalWalletError {headUTxO :: UTxOType tx, reason :: Text, tx :: tx}
  | -- | An error occurred when submitting a transaction to the cardano-node.
    FailedToPostTx {failureReason :: Text}
  | -- | A plutus script failed in a transaction submitted to the cardano-node.
    -- NOTE: PlutusDebugInfo does not have much available instances so we put it
    -- in Text form but it's lame
    PlutusValidationFailed {plutusFailure :: Text, plutusDebugInfo :: Text}
  | -- | User tried to commit more than 'maxMainnetLovelace' hardcoded limit on mainnet
    -- we keep track of both the hardcoded limit and what the user originally tried to commit
    CommittedTooMuchADAForMainnet {userCommittedLovelace :: Lovelace, mainnetLimitLovelace :: Lovelace}
  | -- | We can only draft commit tx for the user when in Initializing state
    FailedToDraftTxNotInitializing
  | -- | We cannot draft a commit tx in offline mode
    FailedToDraftTxOffline
  | -- | Committing UTxO addressed to the internal wallet is forbidden.
    SpendingNodeUtxoForbidden
  | FailedToConstructAbortTx
  | FailedToConstructCloseTx
  | FailedToConstructContestTx
  | FailedToConstructCollectTx
  | FailedToConstructFanoutTx
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (PostTxError tx)
deriving stock instance IsChainState tx => Show (PostTxError tx)
deriving anyclass instance IsChainState tx => ToJSON (PostTxError tx)
deriving anyclass instance IsChainState tx => FromJSON (PostTxError tx)

instance IsChainState tx => Exception (PostTxError tx)

instance Arbitrary Lovelace where
  arbitrary = Lovelace <$> scale (* 8) arbitrary `suchThat` (> 0)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (PostTxError tx) where
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
deriving anyclass instance ToJSON (ChainStateType tx) => ToJSON (ChainStateHistory tx)
deriving anyclass instance FromJSON (ChainStateType tx) => FromJSON (ChainStateHistory tx)

instance Arbitrary (ChainStateType tx) => Arbitrary (ChainStateHistory tx) where
  arbitrary = genericArbitrary

-- | Interface available from a chain state. Expected to be instantiated by all
-- 'ChainStateType tx'.
class
  ( IsTx tx
  , Eq (ChainStateType tx)
  , Show (ChainStateType tx)
  , FromJSON (ChainStateType tx)
  , ToJSON (ChainStateType tx)
  ) =>
  IsChainState tx
  where
  -- | Types of what to keep as L1 chain state.
  type ChainStateType tx = c | c -> tx

  -- | Get the chain slot for a chain state. NOTE: For any sequence of 'a'
  -- encountered, we assume monotonically increasing slots.
  chainStateSlot :: ChainStateType tx -> ChainSlot

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
      UTxO' (TxOut CtxUTxO, Witness WitCtxTxIn) ->
      m (Either (PostTxError Tx) Tx)
  -- ^ Create a commit transaction using user provided utxos (zero or many) and
  -- information to spend from a script. Errors are handled at the call site.
  , submitTx :: MonadThrow m => Tx -> m ()
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
  deriving stock (Generic)

deriving stock instance (IsTx tx, Eq (ChainStateType tx)) => Eq (ChainEvent tx)
deriving stock instance (IsTx tx, Show (ChainStateType tx)) => Show (ChainEvent tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (ChainEvent tx)
deriving anyclass instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (ChainEvent tx)

instance
  ( Arbitrary tx
  , Arbitrary (UTxOType tx)
  , Arbitrary (TxIdType tx)
  , Arbitrary (ChainStateType tx)
  ) =>
  Arbitrary (ChainEvent tx)
  where
  arbitrary = genericArbitrary

-- | A callback indicating a 'ChainEvent tx' happened. Most importantly the
-- 'Observation' of a relevant Hydra transaction.
type ChainCallback tx m = ChainEvent tx -> m ()

-- | A type tying both posting and observing transactions into a single /Component/.
type ChainComponent tx m a = ChainCallback tx m -> (Chain tx m -> m a) -> m a
