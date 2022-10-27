{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Specifies the /Head-Chain Interaction/ part of the protocol
--
-- Incoming and outgoing on-chain transactions are modelled respectively as `OnChainTx`
-- and `PostChainTx` which are data type that abstracts away the details of the structure
-- of the transaction.
module Hydra.Chain where

import Hydra.Prelude

import Data.List (nub)
import Hydra.Cardano.Api (
  Address,
  ByronAddr,
  HasTypeProxy (..),
  SerialiseAsRawBytes (..),
  UsingRawBytesHex (..),
 )
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Ledger (IsTx, TxIdType, UTxOType)
import Hydra.Party (Party)
import Hydra.Snapshot (ConfirmedSnapshot, SnapshotNumber)
import Test.QuickCheck.Instances.Time ()

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
  = InitTx {headParameters :: HeadParameters}
  | CommitTx {party :: Party, committed :: UTxOType tx}
  | AbortTx {utxo :: UTxOType tx}
  | CollectComTx {utxo :: UTxOType tx}
  | CloseTx {confirmedSnapshot :: ConfirmedSnapshot tx}
  | ContestTx {confirmedSnapshot :: ConfirmedSnapshot tx}
  | FanoutTx {utxo :: UTxOType tx, contestationDeadline :: UTCTime}
  deriving stock (Generic)

deriving instance IsTx tx => Eq (PostChainTx tx)
deriving instance IsTx tx => Show (PostChainTx tx)
deriving instance IsTx tx => ToJSON (PostChainTx tx)
deriving instance IsTx tx => FromJSON (PostChainTx tx)

instance IsTx tx => Arbitrary (PostChainTx tx) where
  arbitrary = genericArbitrary

-- REVIEW(SN): There is a similarly named type in plutus-ledger, so we might
-- want to rename this

-- | Uniquely identifies a Hydra Head.
newtype HeadId = HeadId ByteString
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (UsingRawBytesHex HeadId)

instance SerialiseAsRawBytes HeadId where
  serialiseToRawBytes (HeadId bytes) = bytes
  deserialiseFromRawBytes _ = Just . HeadId

instance HasTypeProxy HeadId where
  data AsType HeadId = AsHeadId
  proxyToAsType _ = AsHeadId

instance Arbitrary HeadId where
  arbitrary = genericArbitrary

-- | Describes transactions as seen on chain. Holds as minimal information as
-- possible to simplify observing the chain.
data OnChainTx tx
  = OnInitTx {contestationPeriod :: ContestationPeriod, parties :: [Party]}
  | OnCommitTx {party :: Party, committed :: UTxOType tx}
  | OnAbortTx
  | OnCollectComTx
  | OnCloseTx
      { snapshotNumber :: SnapshotNumber
      , contestationDeadline :: UTCTime
      }
  | OnContestTx {snapshotNumber :: SnapshotNumber}
  | OnFanoutTx
  deriving (Generic)

deriving instance IsTx tx => Eq (OnChainTx tx)
deriving instance IsTx tx => Show (OnChainTx tx)
deriving instance IsTx tx => ToJSON (OnChainTx tx)
deriving instance IsTx tx => FromJSON (OnChainTx tx)

instance (Arbitrary tx, Arbitrary (UTxOType tx)) => Arbitrary (OnChainTx tx) where
  arbitrary = genericArbitrary

-- | Exceptions thrown by 'postTx'.
data PostTxError tx
  = MoreThanOneUTxOCommitted
  | InternalWalletError {headUTxO :: UTxOType tx, reason :: Text, tx :: tx}
  | NotEnoughFuel
  | CannotFindOwnInitial {knownUTxO :: UTxOType tx}
  | FailedToPostTx {failureReason :: Text}
  | -- NOTE: PlutusDebugInfo does not have much available instances so we put it in Text
    -- form but it's lame
    PlutusValidationFailed {plutusFailure :: Text, plutusDebugInfo :: Text}
  | NoSeedInput
  | NoPaymentInput
  | InvalidStateToPost {txTried :: PostChainTx tx, chainState :: ChainStateType tx}
  | UnsupportedLegacyOutput {byronAddress :: Address ByronAddr}
  deriving (Generic)

deriving instance (IsTx tx, IsChainState (ChainStateType tx)) => Eq (PostTxError tx)
deriving instance (IsTx tx, IsChainState (ChainStateType tx)) => Show (PostTxError tx)
deriving instance (IsTx tx, IsChainState (ChainStateType tx)) => ToJSON (PostTxError tx)
deriving instance (IsTx tx, IsChainState (ChainStateType tx)) => FromJSON (PostTxError tx)

instance (IsTx tx, IsChainState (ChainStateType tx)) => Exception (PostTxError tx)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (PostTxError tx) where
  arbitrary = genericArbitrary

-- | Types of what to keep as L1 chain state.
type family ChainStateType tx

-- | Interface available from a chain state. Expected to be instantiated by all
-- 'ChainStateType tx'.
class (Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a) => IsChainState a where
  -- | Get the chain slot for a chain state. NOTE: For any sequence of 'a'
  -- encountered, we assume monotonically increasing slots.
  chainStateSlot :: a -> ChainSlot

-- | A generic description for a chain slot all implementions need to use.
newtype ChainSlot = ChainSlot Natural
  deriving (Ord, Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Get the next chain slot. Use this instead of giving 'Enum' or 'Num'
-- instances to 'ChainSlot'.
nextChainSlot :: ChainSlot -> ChainSlot
nextChainSlot (ChainSlot n) = ChainSlot (n + 1)

instance Arbitrary ChainSlot where
  arbitrary = genericArbitrary

-- | Handle to interface with the main chain network
newtype Chain tx m = Chain
  { -- | Construct and send a transaction to the main chain corresponding to the
    -- given 'PostChainTx' description and the current 'ChainState'. This
    -- function is not expected to block, so it is only responsible for
    -- submitting, but it should validate the created transaction against a
    -- reasonable local view of the chain and throw an exception when invalid.
    --
    -- Does at least throw 'PostTxError'.
    postTx :: MonadThrow m => ChainStateType tx -> PostChainTx tx -> m ()
  }

data ChainEvent tx
  = Observation
      { observedTx :: OnChainTx tx
      , newChainState :: ChainStateType tx
      }
  | Rollback ChainSlot
  | Tick UTCTime
  deriving (Generic)

deriving instance (IsTx tx, Eq (ChainStateType tx)) => Eq (ChainEvent tx)
deriving instance (IsTx tx, Show (ChainStateType tx)) => Show (ChainEvent tx)
deriving instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (ChainEvent tx)
deriving instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (ChainEvent tx)

instance
  ( Arbitrary tx
  , Arbitrary (UTxOType tx)
  , Arbitrary (TxIdType tx)
  , Arbitrary (ChainStateType tx)
  ) =>
  Arbitrary (ChainEvent tx)
  where
  arbitrary = genericArbitrary

-- | A callback indicating receival of a potential Hydra transaction which is Maybe
-- observing a relevant 'ChainEvent tx' paired with a (potentially updated)
-- 'ChainStateType tx'.
type ChainCallback tx m = (ChainStateType tx -> Maybe (ChainEvent tx)) -> m ()

-- | A type tying both posting and observing transactions into a single /Component/.
type ChainComponent tx m a = ChainCallback tx m -> (Chain tx m -> m a) -> m a
