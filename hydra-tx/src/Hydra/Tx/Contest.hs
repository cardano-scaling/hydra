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
import Hydra.Tx.Utils (mkHydraHeadV1TxName)
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
  Tx
contestTx scriptRegistry vk headId contestationPeriod openVersion Snapshot{number, utxo, utxoToDecommit, version} sig (slotNo, _) closedThreadOutput =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(headInput, headWitness)]
      & addReferenceInputs [headScriptRef]
      & addOutputs [headOutputAfter]
      & addExtraRequiredSigners [verificationKeyHash vk]
      & setValidityUpperBound slotNo
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "ContestTx")
 where
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

  headRedeemer = toScriptData $ Head.Contest contestRedeemer

  contestRedeemer
    | version == openVersion =
        Head.ContestCurrent
          { signature = toPlutusSignatures sig
          }
    | otherwise =
        -- NOTE: This will only work for version == openVersion - 1
        Head.ContestOutdated
          { signature = toPlutusSignatures sig
          , alreadyDecommittedUTxOHash = toBuiltin $ hashUTxO @Tx $ fromMaybe mempty utxoToDecommit
          }

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
          , deltaUTxOHash =
              case contestRedeemer of
                Head.ContestCurrent{} ->
                  toBuiltin $ hashUTxO @Tx $ fromMaybe mempty utxoToDecommit
                _ -> toBuiltin $ hashUTxO @Tx mempty
          , parties = closedParties
          , contestationDeadline = newContestationDeadline
          , contestationPeriod = onChainConstestationPeriod
          , headId = headIdToCurrencySymbol headId
          , contesters = contester : closedContesters
          , version = toInteger openVersion
          }
