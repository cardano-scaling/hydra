module Hydra.Tx.Contest where

import Hydra.Cardano.Api
import Hydra.Prelude

import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Data.ContestationPeriod (addContestationPeriod)
import Hydra.Ledger.Cardano.Builder (unsafeBuildTransaction)
import Hydra.Plutus.Extras (posixToUTCTime)
import Hydra.Tx.Close (ClosedThreadOutput (..), PointInTime)
import Hydra.Tx.ContestationPeriod (ContestationPeriod, toChain)
import Hydra.Tx.Crypto (MultiSignature (..), toPlutusSignatures)
import Hydra.Tx.HeadId (HeadId, headIdToCurrencySymbol)
import Hydra.Tx.IsTx (hashUTxO)
import Hydra.Tx.ScriptRegistry (ScriptRegistry, headReference)
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotNumber, SnapshotVersion, fromChainSnapshotNumber)
import Hydra.Tx.Utils (IncrementalAction (..), findStateToken, mkHydraHeadV1TxName)
import PlutusLedgerApi.V3 (toBuiltin)
import PlutusLedgerApi.V3 qualified as Plutus

-- * Construction

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
    defaultTxBodyContent
      & addTxIns [(headInput, headWitness)]
      & addTxInsReference [headScriptRef] mempty
      & addTxOuts [headOutputAfter]
      & addTxExtraKeyWits [verificationKeyHash vk]
      & setTxValidityUpperBound (TxValidityUpperBound slotNo)
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
        mkScriptReference headScriptRef Head.validatorScript InlineScriptDatum headRedeemer

  headScriptRef =
    fst (headReference scriptRegistry)

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
                _ -> toBuiltin $ hashUTxO @Tx mempty
          , omegaUTxOHash =
              case contestRedeemer of
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

-- * Observation

data ContestObservation = ContestObservation
  { contestedThreadOutput :: (TxIn, TxOut CtxUTxO)
  , headId :: HeadId
  , snapshotNumber :: SnapshotNumber
  , contestationDeadline :: UTCTime
  , contesters :: [Plutus.PubKeyHash]
  }
  deriving stock (Show, Eq, Generic)

-- | Identify a close tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeContestTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe ContestObservation
observeContestTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript inputUTxO Head.validatorScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ fromCtxUTxOTxOut headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Closed Head.ClosedDatum{}, Head.Contest{}) -> do
      (newHeadInput, newHeadOutput) <- findTxOutByScript (utxoFromTx tx) Head.validatorScript
      newHeadDatum <- txOutScriptData $ fromCtxUTxOTxOut newHeadOutput
      let (onChainSnapshotNumber, contestationDeadline, contesters) = decodeDatum newHeadDatum
      pure
        ContestObservation
          { contestedThreadOutput = (newHeadInput, newHeadOutput)
          , headId
          , snapshotNumber = fromChainSnapshotNumber onChainSnapshotNumber
          , contestationDeadline = posixToUTCTime contestationDeadline
          , contesters
          }
    _ -> Nothing
 where
  decodeDatum headDatum =
    case fromScriptData headDatum of
      Just (Head.Closed Head.ClosedDatum{snapshotNumber, contestationDeadline, contesters}) ->
        (snapshotNumber, contestationDeadline, contesters)
      _ -> error "wrong state in output datum"
