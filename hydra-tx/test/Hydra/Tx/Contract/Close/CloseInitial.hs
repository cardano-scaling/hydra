{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.Contract.Close.CloseInitial where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO qualified as UTxO
import Data.Maybe (fromJust)
import Hydra.Contract.Error (ToErrorCode (..))
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadState qualified as HeadState
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx (
  ConfirmedSnapshot (..),
  Snapshot (utxoToCommit, utxoToDecommit),
  SnapshotVersion,
  hashUTxO,
  mkHeadId,
  registryUTxO,
 )
import Hydra.Tx.Close (OpenThreadOutput (..), closeTx)
import Hydra.Tx.Contract.Close.Healthy (
  healthyCloseLowerBoundSlot,
  healthyCloseUpperBoundPointInTime,
  healthyContestationDeadline,
  healthyContestationPeriod,
  healthyOnChainParties,
  healthyOpenHeadTxIn,
  healthyOpenHeadTxOut,
  healthyUTxO,
  somePartyCardanoVerificationKey,
 )
import Hydra.Tx.Snapshot (getSnapshot)
import Hydra.Tx.Utils (IncrementalAction (..), setIncrementalActionMaybe)
import PlutusLedgerApi.V3 (POSIXTime, toBuiltin)
import Test.Hydra.Tx.Fixture qualified as Fixture
import Test.Hydra.Tx.Gen (genScriptRegistry)
import Test.Hydra.Tx.Mutation (Mutation (..), SomeMutation (..), modifyInlineDatum, replaceContestationDeadline)
import Test.QuickCheck (oneof, suchThat)
import Test.QuickCheck.Instances ()

data CloseInitialMutation
  = MutateCloseContestationDeadline'
  deriving stock (Generic, Show, Enum, Bounded)

healthyCloseSnapshotVersion :: SnapshotVersion
healthyCloseSnapshotVersion = 0

-- | Healthy close transaction for the specific case were we close a head
--   with the initial UtxO, that is, no snapshot have been agreed upon and
--   signed by the head members yet.
healthyCloseInitialTx :: IO (Tx, UTxO)
healthyCloseInitialTx = do
  tx <-
    closeTx
      scriptRegistry
      somePartyCardanoVerificationKey
      headId
      healthyCloseSnapshotVersion
      closingSnapshot
      healthyCloseLowerBoundSlot
      healthyCloseUpperBoundPointInTime
      openThreadOutput
      incrementalAction
  pure (tx, lookupUTxO)
 where
  incrementalAction =
    fromMaybe NoThing $
      setIncrementalActionMaybe (utxoToCommit $ getSnapshot closingSnapshot) (utxoToDecommit $ getSnapshot closingSnapshot)

  initialDatum :: TxOutDatum CtxUTxO
  initialDatum = mkTxOutDatumInline healthyInitialOpenDatum

  lookupUTxO :: UTxO
  lookupUTxO =
    UTxO.singleton healthyOpenHeadTxIn (healthyOpenHeadTxOut initialDatum)
      <> registryUTxO scriptRegistry

  scriptRegistry = genScriptRegistry `generateWith` 42

  openThreadOutput :: OpenThreadOutput
  openThreadOutput =
    OpenThreadOutput
      { openThreadUTxO = (healthyOpenHeadTxIn, healthyOpenHeadTxOut initialDatum)
      , openParties = healthyOnChainParties
      , openContestationPeriod = healthyContestationPeriod
      }

  headId = mkHeadId Fixture.testPolicyId

  closingSnapshot = InitialSnapshot{headId, initialUTxO = healthyUTxO}

healthyInitialOpenDatum :: HeadState.State
healthyInitialOpenDatum =
  Head.Open
    Head.OpenDatum
      { parties = healthyOnChainParties
      , utxoHash = toBuiltin $ hashUTxO @Tx healthyUTxO
      , contestationPeriod = healthyContestationPeriod
      , headId = toPlutusCurrencySymbol Fixture.testPolicyId
      , version = 0
      }

--- | Mutations for the specific case of closing with the initial state.
--- We should probably validate all the mutation to this initial state but at
--- least we keep this regression test as we stumbled upon problems with the following case.
--- The nice thing to do would probably to generate either "normal" healthyCloseTx or
--- or healthyCloseInitialTx and apply all the mutations to it but we didn't manage to do that
--- right away.
genCloseInitialMutation :: (Tx, UTxO) -> Gen SomeMutation
genCloseInitialMutation (tx, _utxo) =
  SomeMutation (pure $ toErrorCode IncorrectClosedContestationDeadline) MutateCloseContestationDeadline' <$> do
    mutatedDeadline <- genMutatedDeadline
    pure $ ChangeOutput 0 $ modifyInlineDatum (replaceContestationDeadline mutatedDeadline) headTxOut
 where
  headTxOut = fromJust $ txOuts' tx !!? 0

-- | Generate not acceptable, but interesting deadlines.
genMutatedDeadline :: Gen POSIXTime
genMutatedDeadline = do
  oneof
    [ valuesAroundZero
    , valuesAroundDeadline
    ]
 where
  valuesAroundZero = arbitrary `suchThat` (/= deadline)

  valuesAroundDeadline = arbitrary `suchThat` (/= 0) <&> (+ deadline)

  deadline = posixFromUTCTime healthyContestationDeadline
