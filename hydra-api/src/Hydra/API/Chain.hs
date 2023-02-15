{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Specifies the /Head-Chain Interaction/ part of the protocol
--
-- Incoming and outgoing on-chain transactions are modelled respectively as `OnChainTx`
-- and `PostChainTx` which are data type that abstracts away the details of the structure
-- of the transaction.
module Hydra.API.Chain where

import Cardano.Crypto.Util (SignableRepresentation)
import Control.Exception (Exception)
import Control.Monad.Class.MonadTime (UTCTime)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (nub)
import Data.Text (Text)
import GHC.Generics (Generic)
import Generic.Random (genericArbitrary, uniform)
import Hydra.API.ContestationPeriod (ContestationPeriod)
import Hydra.API.Ledger (IsTx, UTxOType)
import Hydra.API.Party (Party)
import Hydra.API.Snapshot (ConfirmedSnapshot, Snapshot, SnapshotNumber)
import Hydra.Cardano.Api (
  Address,
  ByronAddr,
  HasTypeProxy (..),
  SerialiseAsRawBytes (..),
  UsingRawBytesHex (..),
 )
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary, arbitrary, vectorOf)
import Test.QuickCheck.Instances.Time ()

-- | Contains the head's parameters as established in the initial transaction.
data HeadParameters = HeadParameters
  { contestationPeriod :: ContestationPeriod
  , parties :: [Party] -- NOTE(SN): The order of this list is important for leader selection.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary HeadParameters where
  arbitrary = dedupParties <$> genericArbitrary uniform
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

instance (IsTx tx, SignableRepresentation (Snapshot tx)) => Arbitrary (PostChainTx tx) where
  arbitrary = genericArbitrary uniform

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
  arbitrary = HeadId . BS.pack <$> vectorOf 16 arbitrary

-- | Describes transactions as seen on chain. Holds as minimal information as
-- possible to simplify observing the chain.
data OnChainTx tx
  = OnInitTx {headId :: HeadId, contestationPeriod :: ContestationPeriod, parties :: [Party]}
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
  arbitrary = genericArbitrary uniform

-- | Exceptions thrown by 'postTx'.
data PostTxError tx
  = NoSeedInput
  | MoreThanOneUTxOCommitted
  | CannotFindOwnInitial {knownUTxO :: UTxOType tx}
  | UnsupportedLegacyOutput {byronAddress :: Address ByronAddr}
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
  deriving (Generic)

deriving instance (IsTx tx, IsChainState tx) => Eq (PostTxError tx)
deriving instance (IsTx tx, IsChainState tx) => Show (PostTxError tx)
deriving instance (IsTx tx, IsChainState tx) => ToJSON (PostTxError tx)
deriving instance (IsTx tx, IsChainState tx) => FromJSON (PostTxError tx)

instance (IsTx tx, IsChainState tx) => Exception (PostTxError tx)

instance (IsTx tx, Arbitrary (ChainStateType tx), SignableRepresentation (Snapshot tx)) => Arbitrary (PostTxError tx) where
  arbitrary = genericArbitrary uniform

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

-- | A generic description for a chain slot all implementions need to use.
newtype ChainSlot = ChainSlot Natural
  deriving (Ord, Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Get the next chain slot. Use this instead of giving 'Enum' or 'Num'
-- instances to 'ChainSlot'.
nextChainSlot :: ChainSlot -> ChainSlot
nextChainSlot (ChainSlot n) = ChainSlot (n + 1)

instance Arbitrary ChainSlot where
  arbitrary = genericArbitrary uniform
