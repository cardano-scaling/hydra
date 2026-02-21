{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.HeadLogic.Outcome where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Tx.ChainState (ChainStateType (..), IsChainState)
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.Node.Environment (Environment (..), mkHeadParameters)
import Test.Hydra.API.ServerOutput ()
import Test.Hydra.Chain ()
import Test.Hydra.Tx.Gen (ArbitraryIsTx)
import Test.QuickCheck (oneof)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

instance
  ( ArbitraryIsTx tx
  , Arbitrary (ChainPointType tx)
  , Arbitrary (ChainStateType tx)
  , IsChainState tx
  ) =>
  Arbitrary (StateChanged tx)
  where
  arbitrary = arbitrary >>= genStateChanged

instance
  ( ArbitraryIsTx tx
  , Arbitrary (ChainPointType tx)
  , Arbitrary (ChainStateType tx)
  , IsChainState tx
  ) =>
  ToADTArbitrary (StateChanged tx)

-- REVIEW: why are we missing Checkpoint and other events ?
genStateChanged :: (ArbitraryIsTx tx, Arbitrary (ChainStateType tx)) => Environment -> Gen (StateChanged tx)
genStateChanged env =
  oneof
    [ HeadInitialized (mkHeadParameters env) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , CommittedUTxO <$> arbitrary <*> pure party <*> arbitrary <*> arbitrary
    , HeadAborted <$> arbitrary <*> arbitrary <*> arbitrary
    , HeadOpened <$> arbitrary <*> arbitrary <*> arbitrary
    , TransactionReceived <$> arbitrary
    , TransactionAppliedToLocalUTxO <$> arbitrary <*> arbitrary <*> arbitrary
    , SnapshotRequestDecided <$> arbitrary
    , SnapshotRequested <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , PartySignedSnapshot <$> arbitrary <*> arbitrary <*> arbitrary
    , SnapshotConfirmed <$> arbitrary <*> arbitrary <*> arbitrary
    , DepositRecorded <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , DepositActivated <$> arbitrary <*> arbitrary <*> arbitrary
    , DepositExpired <$> arbitrary <*> arbitrary <*> arbitrary
    , DepositRecovered <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , CommitApproved <$> arbitrary <*> arbitrary
    , CommitFinalized <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , DecommitRecorded <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , DecommitApproved <$> arbitrary <*> arbitrary <*> arbitrary
    , DecommitInvalid <$> arbitrary <*> arbitrary <*> arbitrary
    , DecommitFinalized <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , HeadClosed <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , HeadContested <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , HeadIsReadyToFanout <$> arbitrary
    , HeadFannedOut <$> arbitrary <*> arbitrary <*> arbitrary
    , LocalStateCleared <$> arbitrary <*> arbitrary
    , NodeUnsynced <$> arbitrary <*> arbitrary <*> arbitrary
    , NodeSynced <$> arbitrary <*> arbitrary <*> arbitrary
    ]
 where
  Environment{party} = env
