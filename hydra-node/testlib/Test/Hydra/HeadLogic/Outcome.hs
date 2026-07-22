{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.HeadLogic.Outcome where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Chain.ChainState (ChainStateType (..), IsChainState)
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.Node.Environment (Environment (..), mkHeadParameters)
import Test.Hydra.API.ServerOutput ()
import Test.Hydra.Chain ()
import Test.Hydra.HeadLogic.State ()
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

-- | Needed for the per-constructor golden test of the persisted 'StateChanged'
-- event format (see 'Hydra.Events.SQLiteBasedSpec'). Covers every constructor
-- generically from its field 'Arbitrary's, independent of 'genStateChanged'.
instance
  ( ArbitraryIsTx tx
  , Arbitrary (ChainPointType tx)
  , Arbitrary (ChainStateType tx)
  , IsChainState tx
  ) =>
  ToADTArbitrary (StateChanged tx)

-- | Generate a 'StateChanged' event. Covers every constructor except
-- 'Checkpoint': callers (e.g. 'Hydra.NodeSpec') feed these through 'hydrate',
-- which runs 'checkHeadState' against the supplied 'Environment'. All events
-- here therefore keep the head state consistent with @env@ — head-opening uses
-- @mkHeadParameters env@ and the rest leave the state 'Idle'. 'Checkpoint'
-- embeds an arbitrary full 'NodeState' (with random parties) that would fail
-- that check, so it is intentionally omitted; the persisted-format coverage of
-- 'Checkpoint' is instead provided by the derived 'ToADTArbitrary' instance.
genStateChanged ::
  ( ArbitraryIsTx tx
  , Arbitrary (ChainPointType tx)
  , Arbitrary (ChainStateType tx)
  ) =>
  Environment ->
  Gen (StateChanged tx)
genStateChanged env =
  oneof
    [ pure NetworkConnected
    , pure NetworkDisconnected
    , PeerConnected <$> arbitrary
    , PeerDisconnected <$> arbitrary
    , NetworkVersionMismatch <$> arbitrary <*> arbitrary
    , NetworkClusterIDMismatch <$> arbitrary <*> arbitrary
    , HeadOpened (mkHeadParameters env) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , TransactionReceived <$> arbitrary
    , TransactionAppliedToLocalUTxO <$> arbitrary <*> arbitrary
    , SnapshotRequestDecided <$> arbitrary
    , SnapshotRequested <$> arbitrary <*> arbitrary <*> arbitrary
    , PartySignedSnapshot <$> arbitrary <*> arbitrary <*> arbitrary
    , SnapshotConfirmed <$> arbitrary <*> (Just <$> arbitrary) <*> arbitrary
    , DepositRecorded <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , DepositActivated <$> arbitrary <*> arbitrary <*> arbitrary
    , DepositExpired <$> arbitrary <*> arbitrary <*> arbitrary
    , DepositRecovered <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , CommitApproved <$> arbitrary <*> arbitrary
    , CommitFinalized <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , DecommitRecorded <$> arbitrary <*> arbitrary
    , DecommitApproved <$> arbitrary <*> arbitrary <*> arbitrary
    , DecommitInvalid <$> arbitrary <*> arbitrary <*> arbitrary
    , DecommitFinalized <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , HeadClosed <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , HeadContested <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , HeadIsReadyToFanout <$> arbitrary
    , HeadFanoutInitiated <$> arbitrary <*> arbitrary
    , HeadPartialFanoutSelected <$> arbitrary <*> arbitrary <*> arbitrary
    , HeadFanoutReverted <$> arbitrary
    , HeadFannedOut <$> arbitrary <*> arbitrary <*> arbitrary
    , HeadPartialFannedOut <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , ChainRolledBack <$> arbitrary
    , TickObserved <$> arbitrary
    , IgnoredHeadInitializing <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , TxInvalid <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , LocalStateCleared <$> arbitrary <*> arbitrary
    , NodeUnsynced <$> arbitrary <*> arbitrary <*> arbitrary
    , NodeSynced <$> arbitrary <*> arbitrary <*> arbitrary
    ]
