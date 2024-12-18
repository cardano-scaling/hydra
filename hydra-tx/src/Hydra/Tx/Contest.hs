module Hydra.Tx.Contest where

import Hydra.Cardano.Api
import Hydra.Prelude

import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Data.ContestationPeriod (addContestationPeriod)
import Hydra.Data.Party qualified as OnChain
import Hydra.Ledger.Cardano.Builder (
  addExtraRequiredSigners,
  addInputs,
  addOutputs,
  addReferenceInputs,
  emptyTxBody,
  setValidityUpperBound,
  unsafeBuildTransaction,
 )
import Hydra.Plutus.Orphans ()
import Hydra.Tx.ContestationPeriod (ContestationPeriod, toChain)
import Hydra.Tx.Crypto (MultiSignature (..), toPlutusSignatures)
import Hydra.Tx.HeadId (HeadId, headIdToCurrencySymbol)
import Hydra.Tx.IsTx (hashUTxO)
import Hydra.Tx.ScriptRegistry (ScriptRegistry, headReference)
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotVersion)

import Hydra.Tx.Utils (IncrementalAction (..), mkHydraHeadV1TxName)
import PlutusLedgerApi.V3 (toBuiltin)
import PlutusLedgerApi.V3 qualified as Plutus

type PointInTime = (SlotNo, UTCTime)

data ClosedThreadOutput = ClosedThreadOutput
  { closedThreadUTxO :: (TxIn, TxOut CtxUTxO)
  , closedParties :: [OnChain.Party]
  , closedContestationDeadline :: Plutus.POSIXTime
  , closedContesters :: [Plutus.PubKeyHash]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- XXX: This function is VERY similar to the 'closeTx' function (only notable
-- difference being the redeemer, which is in itself also the same structure as
-- the close's one. We could potentially refactor this to avoid repetition or do
-- something more principled at the protocol level itself and "merge" close and
-- contest as one operation.
contestTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  HeadId ->
  ContestationPeriod ->
  SnapshotVersion ->
  -- | Contested snapshot number (i.e. the one we contest to)
  Snapshot Tx ->
  -- | Multi-signature of the whole snapshot
  MultiSignature (Snapshot Tx) ->
  -- | Current slot and posix time to be used as the contestation time.
  PointInTime ->
  -- | Everything needed to spend the Head state-machine output.
  ClosedThreadOutput ->
  IncrementalAction ->
  Tx
contestTx scriptRegistry vk headId contestationPeriod openVersion snapshot sig (slotNo, _) closedThreadOutput incrementalAction =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(headInput, headWitness)]
      & addReferenceInputs [headScriptRef]
      & addOutputs [headOutputAfter]
      & addExtraRequiredSigners [verificationKeyHash vk]
      & setValidityUpperBound slotNo
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "ContestTx")
 where
  Snapshot{number, version, utxo, utxoToCommit, utxoToDecommit} = snapshot

  ClosedThreadOutput
    { closedThreadUTxO = (headInput, headOutputBefore)
    , closedParties
    , closedContestationDeadline
    , closedContesters
    } = closedThreadOutput

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript InlineScriptDatum headRedeemer

  headScriptRef =
    fst (headReference scriptRegistry)

  headScript =
    fromPlutusScript @PlutusScriptV3 Head.validatorScript

  contestRedeemer =
    case incrementalAction of
      ToCommit utxo' ->
        if version == openVersion
          then
            Head.ContestUnusedInc
              { signature = toPlutusSignatures sig
              , alreadyCommittedUTxOHash = toBuiltin $ hashUTxO utxo'
              }
          else
            Head.ContestUsedInc
              { signature = toPlutusSignatures sig
              }
      ToDecommit utxo' ->
        if version == openVersion
          then
            Head.ContestUnusedDec
              { signature = toPlutusSignatures sig
              }
          else
            Head.ContestUsedDec
              { signature = toPlutusSignatures sig
              , alreadyDecommittedUTxOHash = toBuiltin $ hashUTxO utxo'
              }
      NoThing -> Head.ContestCurrent{signature = toPlutusSignatures sig}

  headRedeemer = toScriptData $ Head.Contest contestRedeemer

  headOutputAfter =
    modifyTxOutDatum (const headDatumAfter) headOutputBefore

  contester = toPlutusKeyHash (verificationKeyHash vk)

  onChainConstestationPeriod = toChain contestationPeriod

  newContestationDeadline =
    if length (contester : closedContesters) == length closedParties
      then closedContestationDeadline
      else addContestationPeriod closedContestationDeadline onChainConstestationPeriod

  headDatumAfter =
    mkTxOutDatumInline $
      Head.Closed
        Head.ClosedDatum
          { snapshotNumber = toInteger number
          , utxoHash = toBuiltin $ hashUTxO @Tx utxo
          , alphaUTxOHash =
              case contestRedeemer of
                Head.ContestUsedInc{} ->
                  toBuiltin $ hashUTxO @Tx $ fromMaybe mempty utxoToCommit
                Head.ContestUnusedInc{} ->
                  toBuiltin $ hashUTxO @Tx mempty
                _ -> toBuiltin $ hashUTxO @Tx mempty
          , omegaUTxOHash =
              case contestRedeemer of
                Head.ContestUsedDec{} ->
                  toBuiltin $ hashUTxO @Tx mempty
                Head.ContestUnusedDec{} ->
                  toBuiltin $ hashUTxO @Tx $ fromMaybe mempty utxoToDecommit
                _ -> toBuiltin $ hashUTxO @Tx mempty
          , parties = closedParties
          , contestationDeadline = newContestationDeadline
          , contestationPeriod = onChainConstestationPeriod
          , headId = headIdToCurrencySymbol headId
          , contesters = contester : closedContesters
          , version = toInteger openVersion
          }
