{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Arbitrary instances for the log message types, used to test their CBOR
-- codecs (see 'Test.Hydra.CBOR').
module Test.Hydra.Logging.Messages where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
import Hydra.Chain.Direct.Handlers (CardanoChainLog (..), StartingDecision (..))
import Hydra.Chain.Direct.Wallet (TinyWalletLog (..))
import Hydra.Events.SQLiteBased (SQLiteLog (..))
import Hydra.HeadLogic.Error (LogicError (..), RequirementFailure (..))
import Hydra.HeadLogic.Outcome (Effect (..), Outcome (..), WaitReason (..))
import Hydra.Ledger.Cardano (Tx)
import Hydra.Logging (Envelope (..))
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Network.Authenticate (AuthLog)
import Hydra.Network.Etcd (EtcdLog (..))
import Hydra.Node (HydraNodeLog (..))
import Hydra.Node.Network (NetworkLog (..))
import Hydra.Node.ParameterMismatch (ParamMismatch (..))
import Test.Hydra.API.ServerOutput ()
import Test.Hydra.Chain ()
import Test.Hydra.Chain.Direct.State ()
import Test.Hydra.HeadLogic.Input ()
import Test.Hydra.HeadLogic.Outcome ()
import Test.Hydra.Ledger.Cardano ()
import Test.Hydra.Network ()
import Test.Hydra.Network.Authenticate ()
import Test.Hydra.Node.State ()
import Test.Hydra.Options ()
import Test.QuickCheck (listOf, oneof)
import Test.QuickCheck.Instances ()

-- | Generate a simple JSON 'Aeson.Value'. Numbers are kept integral so that
-- values roundtrip exactly through the JSON-in-CBOR mapping.
genJsonValue :: Gen Aeson.Value
genJsonValue =
  oneof
    [ Aeson.String <$> arbitrary
    , Aeson.Number . fromInteger <$> arbitrary
    , Aeson.Bool <$> arbitrary
    , pure Aeson.Null
    , Aeson.object . fmap (bimap Key.fromText Aeson.String) <$> reasonablySized (listOf arbitrary)
    ]

instance Arbitrary a => Arbitrary (Envelope a) where
  arbitrary = Envelope <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (HydraLog Tx) where
  arbitrary =
    oneof
      [ DirectChain <$> arbitrary
      , APIServer <$> arbitrary
      , Network <$> arbitrary
      , Node <$> arbitrary
      , NodeOptions <$> arbitrary
      , SQLite <$> arbitrary
      , pure EnteringMainloop
      , pure NodeHydrated
      , pure ChainBackendStarted
      , pure NetworkStarted
      ]

instance Arbitrary (Effect Tx) where
  arbitrary =
    oneof
      [ ClientEffect <$> arbitrary
      , NetworkEffect <$> arbitrary
      , OnChainEffect <$> arbitrary
      ]

instance Arbitrary (Outcome Tx) where
  arbitrary =
    oneof
      [ Continue <$> reasonablySized arbitrary <*> reasonablySized arbitrary
      , Wait <$> arbitrary <*> reasonablySized arbitrary
      , Error <$> arbitrary
      ]

instance Arbitrary (WaitReason Tx) where
  arbitrary =
    oneof
      [ WaitOnNotApplicableTx <$> arbitrary
      , WaitOnSnapshotNumber <$> arbitrary
      , WaitOnSnapshotVersion <$> arbitrary
      , pure WaitOnSeenSnapshot
      , WaitOnTxs <$> reasonablySized arbitrary
      , pure WaitOnContestationDeadline
      , WaitOnNotApplicableDecommitTx <$> arbitrary
      , WaitOnUnresolvedCommit <$> arbitrary
      , WaitOnUnresolvedDecommit <$> arbitrary
      , WaitOnDepositObserved <$> arbitrary
      , WaitOnDepositActivation <$> arbitrary
      , WaitOnNodeInSync <$> arbitrary
      ]

instance Arbitrary (LogicError Tx) where
  arbitrary =
    oneof
      [ UnhandledInput <$> arbitrary <*> arbitrary
      , RequireFailed <$> arbitrary
      , AssertionFailed <$> arbitrary
      , NotOurHead <$> arbitrary <*> arbitrary
      , SideLoadSnapshotFailed <$> arbitrary
      ]

instance Arbitrary (RequirementFailure Tx) where
  arbitrary =
    oneof
      [ ReqSnNumberInvalid <$> arbitrary <*> arbitrary
      , ReqSvNumberInvalid <$> arbitrary <*> arbitrary
      , ReqSnNotLeader <$> arbitrary <*> arbitrary
      , pure ReqSnDecommitNotSettled
      , pure ReqSnCommitNotSettled
      , InvalidMultisignature <$> arbitrary <*> reasonablySized arbitrary
      , SnapshotAlreadySigned <$> reasonablySized arbitrary <*> arbitrary
      , AckSnNumberInvalid <$> arbitrary <*> arbitrary
      , SnapshotDoesNotApply <$> arbitrary <*> arbitrary <*> arbitrary
      , pure NoMatchingDeposit
      , RequestedDepositExpired <$> arbitrary
      , RequestedDepositNotFoundLocally <$> arbitrary
      , ReqSnUTxOSetTooLarge <$> arbitrary <*> arbitrary
      ]

instance Arbitrary (HydraNodeLog Tx) where
  arbitrary =
    oneof
      [ BeginInput <$> arbitrary <*> arbitrary <*> arbitrary
      , EndInput <$> arbitrary <*> arbitrary
      , BeginEffect <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , EndEffect <$> arbitrary <*> arbitrary <*> arbitrary
      , LogicOutcome <$> arbitrary <*> arbitrary
      , DroppedFromQueue <$> arbitrary <*> arbitrary
      , pure LoadingState
      , LoadedState . Last <$> arbitrary <*> arbitrary
      , LoadedChainState <$> arbitrary
      , pure ReplayingState
      , Misconfiguration <$> reasonablySized arbitrary
      ]

instance Arbitrary CardanoChainLog where
  arbitrary =
    oneof
      [ ToPost <$> arbitrary
      , PostingTx <$> arbitrary
      , PostedTx <$> arbitrary
      , PostingFailed <$> arbitrary <*> arbitrary
      , RolledForward <$> arbitrary <*> reasonablySized arbitrary
      , RolledBackward <$> arbitrary
      , Wallet <$> arbitrary
      , StartingChainDecision <$> arbitrary
      , BlockfrostTransientError <$> arbitrary <*> arbitrary
      , PartialFanoutFailed <$> arbitrary
      ]

instance Arbitrary TinyWalletLog where
  arbitrary =
    oneof
      [ pure BeginInitialize
      , EndInitialize <$> arbitrary <*> arbitrary
      , BeginUpdate <$> arbitrary
      , EndUpdate <$> arbitrary
      , SkipUpdate <$> arbitrary
      ]

instance Arbitrary StartingDecision where
  arbitrary =
    oneof
      [ FromProvided <$> arbitrary
      , FromTip <$> arbitrary
      , FromPersisted <$> arbitrary <*> arbitrary
      ]

instance Arbitrary ParamMismatch where
  arbitrary =
    oneof
      [ ContestationPeriodMismatch <$> arbitrary <*> arbitrary
      , PartiesMismatch <$> reasonablySized arbitrary <*> reasonablySized arbitrary
      , SavedNetworkPartiesInconsistent <$> arbitrary
      ]

instance Arbitrary APIServerLog where
  arbitrary =
    oneof
      [ APIServerStarted <$> arbitrary
      , pure NewAPIConnection
      , APIOutputSent <$> genJsonValue
      , APIInputReceived <$> genJsonValue
      , APIInvalidInput <$> arbitrary <*> arbitrary
      , APIConnectionError <$> arbitrary
      , APIHTTPRequestReceived . Method <$> arbitrary <*> (PathInfo <$> arbitrary)
      , APITransactionSubmitted <$> arbitrary
      , APIReturnedError <$> arbitrary
      ]

instance Arbitrary NetworkLog where
  arbitrary =
    oneof
      [ Authenticate <$> (arbitrary :: Gen AuthLog)
      , Etcd <$> arbitrary
      ]

instance Arbitrary EtcdLog where
  arbitrary =
    oneof
      [ EtcdLog <$> genJsonValue
      , pure Reconnecting
      , BroadcastFailed <$> arbitrary
      , FailedToDecodeLog <$> arbitrary <*> arbitrary
      , FailedToDecodeValue <$> arbitrary <*> arbitrary <*> arbitrary
      , CreatedLease <$> arbitrary
      , LowLeaseTTL <$> arbitrary
      , pure NoKeepAliveResponse
      , MatchingProtocolVersion <$> arbitrary
      , WatchMessagesStartRevision <$> arbitrary
      , WatchMessagesFallbackTo <$> arbitrary
      , BroadcastDeduped <$> arbitrary <*> arbitrary
      ]

instance Arbitrary SQLiteLog where
  arbitrary =
    oneof
      [ MigratingFromFileBased <$> arbitrary
      , MigrationSkipped <$> arbitrary
      , MigrationComplete <$> arbitrary
      ]
