{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic.Outcome where

import Hydra.Prelude

import Hydra.API.ServerOutput (DecommitInvalidReason, ServerOutput)
import Hydra.Chain (PostChainTx)
import Hydra.Chain.ChainState (ChainSlot, ChainStateType, IsChainState)
import Hydra.HeadLogic.Error (LogicError)
import Hydra.HeadLogic.State (HeadState)
import Hydra.Ledger (ValidationError)
import Hydra.Network.Message (Message)
import Hydra.Tx (
  HeadId,
  HeadParameters,
  HeadSeed,
  IsTx,
  Party,
  Snapshot,
  SnapshotNumber,
  SnapshotVersion,
  TxIdType,
  UTxOType,
  mkHeadParameters,
 )
import Hydra.Tx.Crypto (MultiSignature, Signature)
import Hydra.Tx.Environment (Environment (..))
import Hydra.Tx.IsTx (ArbitraryIsTx)
import Test.QuickCheck (oneof)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

-- | Analogous to inputs, the pure head logic "core" can have effects emited to
-- the "shell" layers and we distinguish the same: effects onto the client, the
-- network and the chain.
data Effect tx
  = -- | Effect to be handled by the "Hydra.API", results in sending this 'ServerOutput'.
    ClientEffect {serverOutput :: ServerOutput tx}
  | -- | Effect to be handled by a "Hydra.Network", results in a 'Hydra.Network.broadcast'.
    NetworkEffect {message :: Message tx}
  | -- | Effect to be handled by a "Hydra.Chain", results in a 'Hydra.Chain.postTx'.
    OnChainEffect {postChainTx :: PostChainTx tx}
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (Effect tx)
deriving stock instance IsChainState tx => Show (Effect tx)
deriving anyclass instance IsChainState tx => ToJSON (Effect tx)
deriving anyclass instance IsChainState tx => FromJSON (Effect tx)

instance (ArbitraryIsTx tx, IsChainState tx) => Arbitrary (Effect tx) where
  arbitrary = genericArbitrary

-- | Head state changed event. These events represent all the internal state
-- changes, get persisted and processed in an event sourcing manner.
data StateChanged tx
  = HeadInitialized
      { parameters :: HeadParameters
      , chainState :: ChainStateType tx
      , headId :: HeadId
      , headSeed :: HeadSeed
      }
  | CommittedUTxO
      { party :: Party
      , committedUTxO :: UTxOType tx
      , chainState :: ChainStateType tx
      }
  | HeadAborted {chainState :: ChainStateType tx}
  | HeadOpened {chainState :: ChainStateType tx, initialUTxO :: UTxOType tx}
  | TransactionReceived {tx :: tx}
  | TransactionAppliedToLocalUTxO
      { tx :: tx
      , newLocalUTxO :: UTxOType tx
      }
  | CommitRecorded {depositScriptUTxO :: UTxOType tx, commitUTxO :: UTxOType tx, newLocalUTxO :: UTxOType tx}
  | CommitRecovered {recoveredUTxO :: UTxOType tx, newLocalUTxO :: UTxOType tx}
  | DecommitRecorded {decommitTx :: tx, newLocalUTxO :: UTxOType tx}
  | SnapshotRequestDecided {snapshotNumber :: SnapshotNumber}
  | -- | A snapshot was requested by some party.
    -- NOTE: We deliberately already include an updated local ledger state to
    -- not need a ledger to interpret this event.
    SnapshotRequested
      { snapshot :: Snapshot tx
      , requestedTxIds :: [TxIdType tx]
      , newLocalUTxO :: UTxOType tx
      , newLocalTxs :: [tx]
      }
  | CommitFinalized {newVersion :: SnapshotVersion}
  | DecommitFinalized {newVersion :: SnapshotVersion}
  | PartySignedSnapshot {snapshot :: Snapshot tx, party :: Party, signature :: Signature (Snapshot tx)}
  | SnapshotConfirmed {snapshot :: Snapshot tx, signatures :: MultiSignature (Snapshot tx)}
  | HeadClosed {chainState :: ChainStateType tx, contestationDeadline :: UTCTime}
  | HeadContested {chainState :: ChainStateType tx, contestationDeadline :: UTCTime}
  | HeadIsReadyToFanout
  | HeadFannedOut {chainState :: ChainStateType tx}
  | ChainRolledBack {chainState :: ChainStateType tx}
  | TickObserved {chainSlot :: ChainSlot}
  deriving stock (Generic)

deriving stock instance (IsTx tx, Eq (HeadState tx), Eq (ChainStateType tx)) => Eq (StateChanged tx)
deriving stock instance (IsTx tx, Show (HeadState tx), Show (ChainStateType tx)) => Show (StateChanged tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (StateChanged tx)
deriving anyclass instance (IsTx tx, FromJSON (HeadState tx), FromJSON (ChainStateType tx)) => FromJSON (StateChanged tx)

instance (ArbitraryIsTx tx, IsChainState tx) => Arbitrary (StateChanged tx) where
  arbitrary = arbitrary >>= genStateChanged

instance (ArbitraryIsTx tx, IsChainState tx) => ToADTArbitrary (StateChanged tx)

genStateChanged :: (ArbitraryIsTx tx, IsChainState tx) => Environment -> Gen (StateChanged tx)
genStateChanged env =
  oneof
    [ HeadInitialized (mkHeadParameters env) <$> arbitrary <*> arbitrary <*> arbitrary
    , CommittedUTxO party <$> arbitrary <*> arbitrary
    , HeadAborted <$> arbitrary
    , HeadOpened <$> arbitrary <*> arbitrary
    , TransactionReceived <$> arbitrary
    , TransactionAppliedToLocalUTxO <$> arbitrary <*> arbitrary
    , DecommitRecorded <$> arbitrary <*> arbitrary
    , SnapshotRequestDecided <$> arbitrary
    , SnapshotRequested <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , PartySignedSnapshot <$> arbitrary <*> arbitrary <*> arbitrary
    , SnapshotConfirmed <$> arbitrary <*> arbitrary
    , DecommitFinalized <$> arbitrary
    , HeadClosed <$> arbitrary <*> arbitrary
    , HeadContested <$> arbitrary <*> arbitrary
    , pure HeadIsReadyToFanout
    , HeadFannedOut <$> arbitrary
    , ChainRolledBack <$> arbitrary
    , TickObserved <$> arbitrary
    ]
 where
  Environment{party} = env

data Outcome tx
  = -- | Continue with the given state updates and side effects.
    Continue {stateChanges :: [StateChanged tx], effects :: [Effect tx]}
  | -- | Wait for some condition to be met with optional state updates.
    Wait {reason :: WaitReason tx, stateChanges :: [StateChanged tx]}
  | -- | Processing resulted in an error.
    Error {error :: LogicError tx}
  deriving stock (Generic)

instance Semigroup (Outcome tx) where
  e@Error{} <> _ = e
  _ <> e@Error{} = e
  Continue scA _ <> Wait r scB = Wait r (scA <> scB)
  Wait r scA <> _ = Wait r scA
  Continue scA efA <> Continue scB efB = Continue (scA <> scB) (efA <> efB)

deriving stock instance IsChainState tx => Eq (Outcome tx)
deriving stock instance IsChainState tx => Show (Outcome tx)
deriving anyclass instance IsChainState tx => ToJSON (Outcome tx)
deriving anyclass instance IsChainState tx => FromJSON (Outcome tx)

instance (ArbitraryIsTx tx, IsChainState tx) => Arbitrary (Outcome tx) where
  arbitrary = genericArbitrary
  shrink = genericShrink

noop :: Outcome tx
noop = Continue [] []

wait :: WaitReason tx -> Outcome tx
wait reason = Wait reason []

newState :: StateChanged tx -> Outcome tx
newState change = Continue [change] []

cause :: Effect tx -> Outcome tx
cause e = Continue [] [e]

causes :: [Effect tx] -> Outcome tx
causes = Continue []

data WaitReason tx
  = WaitOnNotApplicableTx {validationError :: ValidationError}
  | WaitOnSnapshotNumber {waitingForNumber :: SnapshotNumber}
  | WaitOnSnapshotVersion {waitingForVersion :: SnapshotVersion}
  | WaitOnSeenSnapshot
  | WaitOnTxs {waitingForTxIds :: [TxIdType tx]}
  | WaitOnContestationDeadline
  | WaitOnNotApplicableDecommitTx {notApplicableReason :: DecommitInvalidReason tx}
  | WaitOnUnresolvedCommit {commitUTxO :: UTxOType tx}
  | WaitOnUnresolvedDecommit {decommitTx :: tx}
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (WaitReason tx)
deriving stock instance IsTx tx => Show (WaitReason tx)
deriving anyclass instance IsTx tx => ToJSON (WaitReason tx)
deriving anyclass instance IsTx tx => FromJSON (WaitReason tx)

instance ArbitraryIsTx tx => Arbitrary (WaitReason tx) where
  arbitrary = genericArbitrary
