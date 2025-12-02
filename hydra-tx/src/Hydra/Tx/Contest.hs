module Hydra.Tx.Contest where

import Hydra.Cardano.Api
import Hydra.Prelude

import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Data.ContestationPeriod (addContestationPeriod)
import Hydra.Data.Party qualified as OnChain
import Hydra.Ledger.Cardano.Builder (unsafeBuildTransaction)
import Hydra.Plutus.Extras (posixToUTCTime)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Close (PointInTime)
import Hydra.Tx.ContestationPeriod (ContestationPeriod, toChain)
import Hydra.Tx.Crypto (MultiSignature (..), toPlutusSignatures)
import Hydra.Tx.HeadId (HeadId, headIdToCurrencySymbol)
import Hydra.Tx.IsTx (hashUTxO)
import Hydra.Tx.ScriptRegistry (ScriptRegistry, headReference)
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotNumber, SnapshotVersion, fromChainSnapshotNumber)
import Hydra.Tx.Utils (IncrementalAction (..), findStateToken, mkHydraHeadV1TxName)
import PlutusLedgerApi.V1.Crypto qualified as Plutus
import PlutusLedgerApi.V3 (toBuiltin)
import PlutusLedgerApi.V3 qualified as Plutus
import PlutusTx.Builtins (bls12_381_G2_uncompress)

import Hydra.Plutus.Orphans ()

-- * Construction

data ClosedThreadOutput = ClosedThreadOutput
  { closedThreadUTxO :: (TxIn, TxOut CtxUTxO)
  , closedParties :: [OnChain.Party]
  , closedContestationDeadline :: Plutus.POSIXTime
  , closedContesters :: [Plutus.PubKeyHash]
  }
  deriving stock (Eq, Show, Generic)

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
  Snapshot{number, version, utxo, utxoToCommit, utxoToDecommit, accumulator} = snapshot

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

  proof =
    let snapshotUTxO =
          utxo
            <> case contestRedeemer of
              Head.ContestUsedInc{} ->
                fromMaybe mempty utxoToCommit
              Head.ContestUnusedDec{} ->
                fromMaybe mempty utxoToDecommit
              _ -> mempty
     in bls12_381_G2_uncompress $
          toBuiltin $
            Accumulator.createMembershipProofFromUTxO @Tx snapshotUTxO accumulator Accumulator.defaultCRS

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
          , accumulatorHash = toBuiltin contestAccumulatorHash
          , proof
          }
   where
    contestAccumulatorHash = Accumulator.getAccumulatorHash accumulator

-- * Observation

data ContestObservation = ContestObservation
  { headId :: HeadId
  , snapshotNumber :: SnapshotNumber
  , contestationDeadline :: UTCTime
  , contesters :: [Plutus.PubKeyHash]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
      (_, newHeadOutput) <- findTxOutByScript (utxoFromTx tx) Head.validatorScript
      newHeadDatum <- txOutScriptData $ fromCtxUTxOTxOut newHeadOutput
      let (onChainSnapshotNumber, contestationDeadline, contesters) = decodeDatum newHeadDatum
      pure
        ContestObservation
          { headId
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
