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
  | CannotSpendInput {input :: Text, walletUTxO :: UTxOType tx, headUTxO :: UTxOType tx}
  | CannotCoverFees {walletUTxO :: UTxOType tx, headUTxO :: UTxOType tx, reason :: Text, tx :: tx}
  | CannotFindOwnInitial {knownUTxO :: UTxOType tx}
  | FailedToPostTx {failureReason :: Text}
  | -- NOTE: PlutusDebugInfo does not have much available instances so we put it in Text
    -- form but it's lame
    PlutusValidationFailed {plutusFailure :: Text, plutusDebugInfo :: Text}
  | NoSeedInput
  | NoPaymentInput
  | InvalidStateToPost {txTried :: PostChainTx tx}
  | UnsupportedLegacyOutput {byronAddress :: Address ByronAddr}
  deriving (Exception, Generic, ToJSON, FromJSON)

deriving instance IsTx tx => Eq (PostTxError tx)
deriving instance IsTx tx => Show (PostTxError tx)

instance IsTx tx => Arbitrary (PostTxError tx) where
  arbitrary = genericArbitrary

-- | Handle to interface with the main chain network
newtype Chain tx m = Chain
  { -- | Construct and send a transaction to the main chain corresponding to the
    -- given 'OnChainTx' event. This function is not expected to block, so it is
    -- only responsible for submitting, but it should validate the created
    -- transaction against a reasonable local view of the chain and throw an
    -- exception when invalid.
    --
    -- Does at least throw 'PostTxError'.
    postTx :: MonadThrow m => PostChainTx tx -> m ()
  }

data ChainEvent tx
  = Observation (OnChainTx tx)
  | Rollback Word
  | Tick UTCTime
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance
  ( Arbitrary tx
  , Arbitrary (UTxOType tx)
  , Arbitrary (TxIdType tx)
  ) =>
  Arbitrary (ChainEvent tx)
  where
  arbitrary = genericArbitrary

-- | Handle to interface observed transactions.
type ChainCallback tx m = ChainEvent tx -> m ()

-- | A type tying both posting and observing transactions into a single /Component/.
type ChainComponent tx m a = ChainCallback tx m -> (Chain tx m -> m a) -> m a
