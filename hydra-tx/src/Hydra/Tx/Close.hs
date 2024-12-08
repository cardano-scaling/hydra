{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Close where

import Hydra.Cardano.Api
import Hydra.Prelude

import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Data.ContestationPeriod (addContestationPeriod)
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Data.Party qualified as OnChain
import Hydra.Ledger.Cardano.Builder (
  unsafeBuildTransaction,
 )
import Hydra.Plutus.Extras.Time (posixFromUTCTime)
import Hydra.Tx (
  ConfirmedSnapshot (..),
  HeadId,
  ScriptRegistry (headReference),
  Snapshot (..),
  SnapshotVersion,
  getSnapshot,
  hashUTxO,
  headIdToCurrencySymbol,
  headReference,
 )
import Hydra.Tx.Contest (PointInTime)
import Hydra.Tx.Crypto (toPlutusSignatures)
import Hydra.Tx.Utils (IncrementalAction (..), mkHydraHeadV1TxName)
import PlutusLedgerApi.V3 (toBuiltin)

-- | Representation of the Head output after a CollectCom transaction.
data OpenThreadOutput = OpenThreadOutput
  { openThreadUTxO :: (TxIn, TxOut CtxUTxO)
  , openContestationPeriod :: OnChain.ContestationPeriod
  , openParties :: [OnChain.Party]
  }
  deriving stock (Eq, Show, Generic)

-- | Create a transaction closing a head with either the initial snapshot or
-- with a multi-signed confirmed snapshot.
closeTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | Head identifier
  HeadId ->
  -- | Last known version of the open head.
  SnapshotVersion ->
  -- | Snapshot with instructions how to close the head.
  ConfirmedSnapshot Tx ->
  -- | Lower validity slot number, usually a current or quite recent slot number.
  SlotNo ->
  -- | Upper validity slot and UTC time to compute the contestation deadline time.
  PointInTime ->
  -- | Everything needed to spend the Head state-machine output.
  OpenThreadOutput ->
  IncrementalAction ->
  Tx
closeTx scriptRegistry vk headId openVersion confirmedSnapshot startSlotNo (endSlotNo, utcTime) openThreadOutput incrementalAction =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxIns [(headInput, headWitness)]
      & addTxInsReference [headScriptRef]
      & addTxOuts [headOutputAfter]
      & addTxExtraKeyWits [verificationKeyHash vk]
      & setTxValidityLowerBound (TxValidityLowerBound startSlotNo)
      & setTxValidityUpperBound (TxValidityUpperBound endSlotNo)
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "CloseTx")
 where
  OpenThreadOutput
    { openThreadUTxO = (headInput, headOutputBefore)
    , openContestationPeriod
    , openParties
    } = openThreadOutput

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript InlineScriptDatum headRedeemer

  headScriptRef =
    fst (headReference scriptRegistry)

  headScript =
    fromPlutusScript @PlutusScriptV3 Head.validatorScript

  headRedeemer = toScriptData $ Head.Close closeRedeemer

  closeRedeemer =
    case confirmedSnapshot of
      InitialSnapshot{} -> Head.CloseInitial
      ConfirmedSnapshot{signatures, snapshot = Snapshot{version}} ->
        case incrementalAction of
          ToCommit utxo' ->
            if version == openVersion
              then
                Head.CloseUnusedInc
                  { signature = toPlutusSignatures signatures
                  , alreadyCommittedUTxOHash = toBuiltin $ hashUTxO utxo'
                  }
              else
                Head.CloseUsedInc
                  { signature = toPlutusSignatures signatures
                  , alreadyCommittedUTxOHash = toBuiltin $ hashUTxO utxo'
                  }
          ToDecommit utxo' ->
            if version == openVersion
              then Head.CloseUnusedDec{signature = toPlutusSignatures signatures}
              else
                Head.CloseUsedDec
                  { signature = toPlutusSignatures signatures
                  , alreadyDecommittedUTxOHash = toBuiltin $ hashUTxO utxo'
                  }
          NoThing -> Head.CloseAny{signature = toPlutusSignatures signatures}

  headOutputAfter =
    modifyTxOutDatum (const headDatumAfter) headOutputBefore

  headDatumAfter =
    mkTxOutDatumInline $
      Head.Closed
        Head.ClosedDatum
          { snapshotNumber =
              fromIntegral . number $ getSnapshot confirmedSnapshot
          , utxoHash =
              toBuiltin . hashUTxO $ utxo (getSnapshot confirmedSnapshot)
          , alphaUTxOHash =
              case closeRedeemer of
                Head.CloseUsedInc{} ->
                  toBuiltin . hashUTxO @Tx . fromMaybe mempty . utxoToCommit $ getSnapshot confirmedSnapshot
                _ -> toBuiltin $ hashUTxO @Tx mempty
          , omegaUTxOHash =
              case closeRedeemer of
                Head.CloseUnusedDec{} ->
                  toBuiltin . hashUTxO @Tx . fromMaybe mempty . utxoToDecommit $ getSnapshot confirmedSnapshot
                _ -> toBuiltin $ hashUTxO @Tx mempty
          , parties = openParties
          , contestationDeadline
          , contestationPeriod = openContestationPeriod
          , headId = headIdToCurrencySymbol headId
          , contesters = []
          , version = fromIntegral openVersion
          }

  contestationDeadline =
    addContestationPeriod (posixFromUTCTime utcTime) openContestationPeriod
