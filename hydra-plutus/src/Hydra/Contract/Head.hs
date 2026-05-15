{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoPolyKinds #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Hydra.Contract.Head where

import PlutusTx.Prelude

import GHC.ByteOrder (ByteOrder (BigEndian))
import Hydra.Cardano.Api (
  PlutusScript,
  pattern PlutusScriptSerialised,
 )
import Hydra.Contract.CRS (CRSDatum, checkMembershipPairing)
import Hydra.Contract.Commit (Commit (..))
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Contract.HeadError (HeadError (..), errorCode)
import Hydra.Contract.HeadState (
  CloseRedeemer (..),
  ClosedDatum (..),
  ContestRedeemer (..),
  DecrementRedeemer (..),
  FanoutProgressDatum (..),
  Hash,
  IncrementRedeemer (..),
  Input (..),
  OpenDatum (..),
  Signature,
  SnapshotNumber,
  SnapshotVersion,
  State (..),
  progressFromClosed,
 )
import Hydra.Contract.Util (hasST, hashPreSerializedCommits, hashTxOuts, hashTxRefAndOuts, mustBurnAllHeadTokens, mustNotMintOrBurn, mustPreserveHeadValue)
import Hydra.Data.ContestationPeriod (ContestationPeriod, addContestationPeriod, milliseconds)
import Hydra.Data.Party (Party (vkey))
import Hydra.Plutus.Extras (ValidatorType, wrapValidator)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.V1.Time (fromMilliSeconds)
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  CurrencySymbol,
  Datum (..),
  Extended (Finite),
  Interval (..),
  LowerBound (LowerBound),
  OutputDatum (..),
  POSIXTime,
  PubKeyHash (getPubKeyHash),
  ScriptContext (..),
  TokenName (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef,
  UpperBound (..),
  Value (Value),
 )
import PlutusLedgerApi.V3.Contexts (findOwnInput, findTxInByTxOutRef)
import PlutusTx (CompiledCode)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Foldable qualified as F
import PlutusTx.List qualified as L

type DatumType = State
type RedeemerType = Input

--------------------------------------------------------------------------------
-- Validators
--------------------------------------------------------------------------------

{-# INLINEABLE headValidator #-}
headValidator ::
  State ->
  Input ->
  ScriptContext ->
  Bool
headValidator oldState input ctx =
  case (oldState, input) of
    (Open openDatum, Increment redeemer) ->
      checkIncrement ctx openDatum redeemer
    (Open openDatum, Decrement redeemer) ->
      checkDecrement ctx openDatum redeemer
    (Open openDatum, Close redeemer) ->
      checkClose ctx openDatum redeemer
    (Closed closedDatum, Contest redeemer) ->
      checkContest ctx closedDatum redeemer
    (Closed closedDatum, Fanout{numberOfFanoutOutputs, numberOfCommitOutputs, numberOfDecommitOutputs, subsetTxInRefs, proof, crsRef}) ->
      headIsFinalizedWith ctx closedDatum numberOfFanoutOutputs numberOfCommitOutputs numberOfDecommitOutputs subsetTxInRefs proof crsRef
    (Closed closedDatum, PartialFanout{numberOfPartialOutputs, subsetTxInRefs, crsRef}) ->
      checkPartialFanout ctx (progressFromClosed closedDatum) numberOfPartialOutputs subsetTxInRefs crsRef
    (FanoutProgress progressDatum, PartialFanout{numberOfPartialOutputs, subsetTxInRefs, crsRef}) ->
      checkPartialFanout ctx progressDatum numberOfPartialOutputs subsetTxInRefs crsRef
    (FanoutProgress progressDatum, FinalPartialFanout{numberOfPartialOutputs, subsetTxInRefs, proof, crsRef}) ->
      checkFinalPartialFanout ctx progressDatum numberOfPartialOutputs subsetTxInRefs proof crsRef
    _ ->
      traceError $(errorCode InvalidHeadStateTransition)

-- | Try to find the deposit datum in the input and, if it is there,
-- return the head id the deposit was created for and the committed utxo.
depositDatum :: TxOut -> Maybe (CurrencySymbol, [Commit])
depositDatum input = do
  let datum = getTxOutDatum input
  case fromBuiltinData @Deposit.DepositDatum $ getDatum datum of
    Just (headId, _deadline, commits) ->
      Just (headId, commits)
    Nothing -> Nothing
{-# INLINEABLE depositDatum #-}

-- | Verify a increment transaction.
checkIncrement ::
  ScriptContext ->
  -- | Open state before the increment
  OpenDatum ->
  IncrementRedeemer ->
  Bool
checkIncrement ctx@ScriptContext{scriptContextTxInfo = txInfo} openBefore redeemer =
  mustNotChangeParameters (prevParties, nextParties) (prevCperiod, nextCperiod) (prevHeadId, nextHeadId)
    && mustIncreaseVersion
    && mustPreserveValue
    && mustBeSignedByParticipant ctx prevHeadId
    && checkSnapshotSignature
    && claimedDepositIsSpent
 where
  inputs = txInfoInputs txInfo

  depositInput =
    case findTxInByTxOutRef increment txInfo of
      Nothing -> traceError $(errorCode DepositInputNotFound)
      Just i -> i

  (_, commits) =
    case depositDatum (txInInfoResolved depositInput) of
      Just (h, cs) -> (h, cs)
      Nothing -> traceError $(errorCode DepositInputNotFound)

  depositHash = hashPreSerializedCommits commits

  headTxIn =
    case L.find (hasST prevHeadId . txOutValue . txInInfoResolved) inputs of
      Nothing -> traceError $(errorCode HeadInputNotFound)
      Just i -> i

  headInValue = txOutValue (txInInfoResolved headTxIn)
  headOutValue = txOutValue $ L.head $ txInfoOutputs txInfo

  -- Sum of every script input that is not the head input itself.
  -- Pub-key inputs (e.g. fee payers) are excluded so they don't inflate the
  -- expected head output value.
  totalNonHeadInputValue =
    F.foldMap (txOutValue . txInInfoResolved) $
      L.filter (\i -> txInInfoOutRef i /= txInInfoOutRef headTxIn && isScriptInput (txInInfoResolved i)) inputs

  isScriptInput txOut =
    case addressCredential (txOutAddress txOut) of
      ScriptCredential _ -> True
      PubKeyCredential _ -> False

  IncrementRedeemer{signature, snapshotNumber, increment, accumulatorHash} = redeemer

  claimedDepositIsSpent =
    traceIfFalse $(errorCode DepositNotSpent) $
      increment `L.elem` (txInInfoOutRef <$> txInfoInputs txInfo)

  checkSnapshotSignature =
    verifySnapshotSignature nextParties (nextHeadId, prevVersion, snapshotNumber, nextUtxoHash, depositHash, emptyHash, accumulatorHash) signature

  mustIncreaseVersion =
    traceIfFalse $(errorCode VersionNotIncremented) $
      nextVersion == prevVersion + 1

  -- TODO: This is not as flexible as it could be and rejects deposits
  -- that are smaller than what the deposit output's min utxo value is.
  -- For example: a 1 ADA utxo can be deposited, but the deposit tx's
  -- output will require ~1.5 ADA because of the inline datum on it. An
  -- increment of that deposit will fail because the sum here is not
  -- exact.
  mustPreserveValue =
    traceIfFalse $(errorCode HeadValueIsNotPreserved) $
      headInValue <> totalNonHeadInputValue == headOutValue

  OpenDatum
    { parties = prevParties
    , contestationPeriod = prevCperiod
    , headId = prevHeadId
    , version = prevVersion
    } = openBefore

  OpenDatum
    { utxoHash = nextUtxoHash
    , parties = nextParties
    , contestationPeriod = nextCperiod
    , headId = nextHeadId
    , version = nextVersion
    } = decodeHeadOutputOpenDatum ctx
{-# INLINEABLE checkIncrement #-}

-- | Verify a decrement transaction.
checkDecrement ::
  ScriptContext ->
  -- | Open state before the decrement
  OpenDatum ->
  DecrementRedeemer ->
  Bool
checkDecrement ctx openBefore redeemer =
  mustNotChangeParameters (prevParties, nextParties) (prevCperiod, nextCperiod) (prevHeadId, nextHeadId)
    && mustIncreaseVersion
    && checkSnapshotSignature
    -- && mustPreserveValue
    && mustDecreaseValue
    && mustBeSignedByParticipant ctx prevHeadId
 where
  checkSnapshotSignature =
    verifySnapshotSignature nextParties (nextHeadId, prevVersion, snapshotNumber, nextUtxoHash, emptyHash, decommitUtxoHash, accumulatorHash) signature

  mustDecreaseValue =
    traceIfFalse $(errorCode HeadValueIsNotPreserved) $
      headInValue == headOutValue <> F.foldMap txOutValue decommitOutputs

  mustIncreaseVersion =
    traceIfFalse $(errorCode VersionNotIncremented) $
      nextVersion == prevVersion + 1

  decommitUtxoHash = hashTxOuts decommitOutputs

  DecrementRedeemer{signature, snapshotNumber, numberOfDecommitOutputs, accumulatorHash} = redeemer

  OpenDatum
    { parties = prevParties
    , contestationPeriod = prevCperiod
    , headId = prevHeadId
    , version = prevVersion
    } = openBefore

  OpenDatum
    { utxoHash = nextUtxoHash
    , parties = nextParties
    , contestationPeriod = nextCperiod
    , headId = nextHeadId
    , version = nextVersion
    } = decodeHeadOutputOpenDatum ctx

  -- NOTE: head output + whatever is decommitted needs to be equal to the head input.
  headOutValue = txOutValue $ L.head outputs
  headInValue = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  -- NOTE: we always assume Head output is the first one so we pick all other
  -- outputs of a decommit tx to calculate the expected hash.
  decommitOutputs = L.take numberOfDecommitOutputs (L.tail outputs)

  outputs = txInfoOutputs txInfo

  ScriptContext{scriptContextTxInfo = txInfo} = ctx
{-# INLINEABLE checkDecrement #-}

-- | Check that the G1 commitment stored in the output datum is consistent with
-- the hash that parties signed. Prevents a malicious closer from storing a wrong
-- commitment while providing a valid signature over a correct hash.
mustMatchAccumulatorCommitmentHash :: BuiltinBLS12_381_G1_Element -> Hash -> Bool
mustMatchAccumulatorCommitmentHash commitment hash =
  traceIfFalse $(errorCode AccumulatorCommitmentHashMismatch) $
    Builtins.blake2b_256 (Builtins.bls12_381_G1_compress commitment) == hash
{-# INLINEABLE mustMatchAccumulatorCommitmentHash #-}

-- | Verify a close transaction.
checkClose ::
  ScriptContext ->
  -- | Open state before the close
  OpenDatum ->
  -- | Type of close transition.
  CloseRedeemer ->
  Bool
checkClose ctx openBefore redeemer =
  mustNotMintOrBurn txInfo
    && hasBoundedValidity
    && checkDeadline
    && mustBeSignedByParticipant ctx headId
    && mustNotChangeVersion
    && mustBeValidSnapshot
    && mustInitializeContesters
    && mustPreserveHeadValue ctx
    && mustNotChangeParameters (parties', parties) (cperiod', cperiod) (headId', headId)
    && mustBindAccumulatorCommitment
 where
  OpenDatum
    { parties
    , utxoHash = initialUtxoHash
    , contestationPeriod = cperiod
    , headId
    , version
    } = openBefore

  hasBoundedValidity =
    traceIfFalse $(errorCode HasBoundedValidityCheckFailed) $
      tMax - tMin <= cp

  ClosedDatum
    { snapshotNumber = snapshotNumber'
    , utxoHash = utxoHash'
    , alphaUTxOHash = alphaUTxOHash'
    , omegaUTxOHash = omegaUTxOHash'
    , parties = parties'
    , contestationDeadline = deadline
    , contestationPeriod = cperiod'
    , headId = headId'
    , contesters = contesters'
    , version = version'
    , accumulatorCommitment = accumulatorCommitment'
    } = decodeHeadOutputClosedDatum ctx

  mustNotChangeVersion =
    traceIfFalse $(errorCode MustNotChangeVersion) $
      version' == version

  mustBeValidSnapshot =
    case redeemer of
      CloseInitial ->
        traceIfFalse $(errorCode FailedCloseInitial) $
          version == 0
            && snapshotNumber' == 0
            && utxoHash' == initialUtxoHash
      CloseAny{signature, accumulatorHash} ->
        traceIfFalse $(errorCode FailedCloseAny) $
          snapshotNumber' > 0
            && alphaUTxOHash' == emptyHash
            && omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version, snapshotNumber', utxoHash', emptyHash, emptyHash, accumulatorHash)
              signature
      CloseUnusedDec{signature, accumulatorHash} ->
        traceIfFalse $(errorCode FailedCloseUnusedDec) $
          alphaUTxOHash' == emptyHash
            && omegaUTxOHash' /= emptyHash
            && verifySnapshotSignature
              parties
              (headId, version, snapshotNumber', utxoHash', emptyHash, omegaUTxOHash', accumulatorHash)
              signature
      CloseUsedDec{signature, alreadyDecommittedUTxOHash, accumulatorHash} ->
        traceIfFalse $(errorCode FailedCloseUsedDec) $
          alphaUTxOHash' == emptyHash
            && omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version - 1, snapshotNumber', utxoHash', emptyHash, alreadyDecommittedUTxOHash, accumulatorHash)
              signature
      CloseUnusedInc{signature, alreadyCommittedUTxOHash, accumulatorHash} ->
        traceIfFalse $(errorCode FailedCloseUnusedInc) $
          alphaUTxOHash' == emptyHash
            && omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version, snapshotNumber', utxoHash', alreadyCommittedUTxOHash, emptyHash, accumulatorHash)
              signature
      CloseUsedInc{signature, alreadyCommittedUTxOHash, accumulatorHash} ->
        traceIfFalse $(errorCode FailedCloseUsedInc) $
          alphaUTxOHash' == alreadyCommittedUTxOHash
            && omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version - 1, snapshotNumber', utxoHash', alreadyCommittedUTxOHash, emptyHash, accumulatorHash)
              signature

  checkDeadline =
    traceIfFalse $(errorCode IncorrectClosedContestationDeadline) $
      deadline == makeContestationDeadline cperiod ctx

  cp = fromMilliSeconds (milliseconds cperiod)

  tMax = case ivTo $ txInfoValidRange txInfo of
    UpperBound (Finite t) _ -> t
    _InfiniteBound -> traceError $(errorCode InfiniteUpperBound)

  tMin = case ivFrom $ txInfoValidRange txInfo of
    LowerBound (Finite t) _ -> t
    _InfiniteBound -> traceError $(errorCode InfiniteLowerBound)

  mustInitializeContesters =
    traceIfFalse $(errorCode ContestersNonEmpty) $
      L.null contesters'

  mustBindAccumulatorCommitment =
    case redeemer of
      CloseInitial -> True
      CloseAny{accumulatorHash} -> check' accumulatorHash
      CloseUnusedDec{accumulatorHash} -> check' accumulatorHash
      CloseUsedDec{accumulatorHash} -> check' accumulatorHash
      CloseUnusedInc{accumulatorHash} -> check' accumulatorHash
      CloseUsedInc{accumulatorHash} -> check' accumulatorHash
   where
    check' = mustMatchAccumulatorCommitmentHash accumulatorCommitment'

  ScriptContext{scriptContextTxInfo = txInfo} = ctx
{-# INLINEABLE checkClose #-}

-- | Verify a contest transaction.
checkContest ::
  ScriptContext ->
  -- | Closed state before the close
  ClosedDatum ->
  -- | Type of contest transition.
  ContestRedeemer ->
  Bool
checkContest ctx closedDatum redeemer =
  mustNotMintOrBurn txInfo
    && mustNotChangeVersion
    && mustBeNewer
    && mustBeValidSnapshot
    && mustBeSignedByParticipant ctx headId
    && checkSignedParticipantContestOnlyOnce
    && mustBeWithinContestationPeriod
    && mustUpdateContesters
    && mustPushDeadline
    && mustNotChangeParameters (parties', parties) (contestationPeriod', contestationPeriod) (headId', headId)
    && mustPreserveHeadValue ctx
    && mustBindAccumulatorCommitment
 where
  mustBeNewer =
    traceIfFalse $(errorCode TooOldSnapshot) $
      snapshotNumber' > snapshotNumber

  mustNotChangeVersion =
    traceIfFalse $(errorCode MustNotChangeVersion) $
      version' == version

  mustBeValidSnapshot =
    case redeemer of
      ContestCurrent{signature, accumulatorHash} ->
        traceIfFalse $(errorCode FailedContestCurrent) $
          alphaUTxOHash' == emptyHash
            && omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version, snapshotNumber', utxoHash', emptyHash, emptyHash, accumulatorHash)
              signature
      ContestUsedDec{signature, alreadyDecommittedUTxOHash, accumulatorHash} ->
        traceIfFalse $(errorCode FailedContestUsedDec) $
          alphaUTxOHash' == emptyHash
            && omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version - 1, snapshotNumber', utxoHash', emptyHash, alreadyDecommittedUTxOHash, accumulatorHash)
              signature
      ContestUnusedDec{signature, accumulatorHash} ->
        traceIfFalse $(errorCode FailedContestUnusedDec) $
          alphaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version, snapshotNumber', utxoHash', emptyHash, omegaUTxOHash', accumulatorHash)
              signature
      ContestUnusedInc{signature, alreadyCommittedUTxOHash, accumulatorHash} ->
        traceIfFalse $(errorCode FailedContestUnusedInc) $
          omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version - 1, snapshotNumber', utxoHash', alreadyCommittedUTxOHash, emptyHash, accumulatorHash)
              signature
      ContestUsedInc{signature, accumulatorHash} ->
        traceIfFalse $(errorCode FailedContestUsedInc) $
          omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version, snapshotNumber', utxoHash', alphaUTxOHash', emptyHash, accumulatorHash)
              signature

  mustBeWithinContestationPeriod =
    case ivTo (txInfoValidRange txInfo) of
      UpperBound (Finite time) _ ->
        traceIfFalse $(errorCode UpperBoundBeyondContestationDeadline) $
          time <= contestationDeadline
      _ -> traceError $(errorCode ContestNoUpperBoundDefined)

  mustPushDeadline =
    if L.length contesters' == L.length parties'
      then
        traceIfFalse $(errorCode MustNotPushDeadline) $
          contestationDeadline' == contestationDeadline
      else
        traceIfFalse $(errorCode MustPushDeadline) $
          contestationDeadline' == addContestationPeriod contestationDeadline contestationPeriod

  mustUpdateContesters =
    traceIfFalse $(errorCode ContesterNotIncluded) $
      contesters' == contester : contesters

  ClosedDatum
    { contestationDeadline
    , contestationPeriod
    , parties
    , snapshotNumber
    , contesters
    , headId
    , version
    } = closedDatum

  ClosedDatum
    { snapshotNumber = snapshotNumber'
    , utxoHash = utxoHash'
    , alphaUTxOHash = alphaUTxOHash'
    , omegaUTxOHash = omegaUTxOHash'
    , parties = parties'
    , contestationDeadline = contestationDeadline'
    , contestationPeriod = contestationPeriod'
    , headId = headId'
    , contesters = contesters'
    , version = version'
    , accumulatorCommitment = accumulatorCommitment'
    } = decodeHeadOutputClosedDatum ctx

  ScriptContext{scriptContextTxInfo = txInfo} = ctx

  contester =
    case txInfoSignatories txInfo of
      [signer] -> signer
      _ -> traceError $(errorCode WrongNumberOfSigners)

  checkSignedParticipantContestOnlyOnce =
    traceIfFalse $(errorCode SignerAlreadyContested) $
      contester `L.notElem` contesters

  mustBindAccumulatorCommitment =
    case redeemer of
      ContestCurrent{accumulatorHash} -> check' accumulatorHash
      ContestUsedDec{accumulatorHash} -> check' accumulatorHash
      ContestUnusedDec{accumulatorHash} -> check' accumulatorHash
      ContestUnusedInc{accumulatorHash} -> check' accumulatorHash
      ContestUsedInc{accumulatorHash} -> check' accumulatorHash
   where
    check' = mustMatchAccumulatorCommitmentHash accumulatorCommitment'
{-# INLINEABLE checkContest #-}

-- | Verify a fanout transaction.
headIsFinalizedWith ::
  ScriptContext ->
  -- | Closed state before the fanout
  ClosedDatum ->
  -- | Number of normal outputs to fanout
  Integer ->
  -- | Number of alpha outputs to fanout
  Integer ->
  -- | Number of delta outputs to fanout
  Integer ->
  -- | Snapshot 'TxOutRef's for every distributed output, in output order.
  [TxOutRef] ->
  -- | Membership proof for the fanout outputs
  BuiltinBLS12_381_G1_Element ->
  -- | Reference input containing crs
  TxOutRef ->
  Bool
headIsFinalizedWith ScriptContext{scriptContextTxInfo = txInfo} closedDatum numberOfFanoutOutputs numberOfCommitOutputs numberOfDecommitOutputs subsetTxInRefs proof crsRef =
  mustBurnAllHeadTokens minted headId parties
    && hashChecks
    && afterContestationDeadline txInfo contestationDeadline
    && checkCRSAndMembership
 where
  minted = txInfoMint txInfo

  totalOutputs = numberOfFanoutOutputs + numberOfCommitOutputs + numberOfDecommitOutputs

  hashChecks = hasSameUTxOHash && hasSameCommitUTxOHash && hasSameDecommitUTxOHash

  -- Hash checks use hashTxOuts which streams via foldMap (no intermediate list retained).
  -- Avoiding a shared allSerialized list prevents retaining all serialized ByteStrings
  -- in memory simultaneously, which would blow the memory budget for larger UTxO sets.
  hasSameUTxOHash =
    traceIfFalse $(errorCode FanoutUTxOHashMismatch) $
      hashTxOuts (L.take numberOfFanoutOutputs txInfoOutputs) == utxoHash

  hasSameCommitUTxOHash =
    traceIfFalse $(errorCode FanoutUTxOToCommitHashMismatch) $
      alphaUTxOHash
        == hashTxOuts (L.take numberOfCommitOutputs (L.drop numberOfFanoutOutputs txInfoOutputs))

  hasSameDecommitUTxOHash =
    traceIfFalse $(errorCode FanoutUTxOToDecommitHashMismatch) $
      omegaUTxOHash
        == hashTxOuts (L.take numberOfDecommitOutputs (L.drop (numberOfFanoutOutputs + numberOfCommitOutputs) txInfoOutputs))

  -- We do not explicitly check @length subsetTxInRefs == totalOutputs@: if the
  -- redeemer supplies a wrong-length list, 'L.zip' truncates and 'P_S' ends up
  -- with the wrong degree, causing the membership pairing below to reject. The
  -- explicit length check would cost extra Plutus CPU that small fanouts can't
  -- afford against the 10B exec budget.
  subsetScalars :: [Integer]
  subsetScalars = txOutsToSubsetScalars (L.zip subsetTxInRefs (L.take totalOutputs txInfoOutputs))

  ClosedDatum{utxoHash, alphaUTxOHash, omegaUTxOHash, parties, headId, contestationDeadline, accumulatorCommitment} = closedDatum
  TxInfo{txInfoOutputs} = txInfo

  checkCRSAndMembership =
    withCRSLookup txInfo crsRef $ \crsData ->
      checkMembershipPairing accumulatorCommitment proof crsData subsetScalars
{-# INLINEABLE headIsFinalizedWith #-}

-- | Verify a partial fanout transaction. Transitions either Closed → FanoutProgress
-- or FanoutProgress → FanoutProgress: distributes a subset of UTxOs and continues
-- with a smaller FanoutProgressDatum.
--
-- The continuing head output must be the first transaction output. Distributed
-- UTxOs follow at indices [1 .. numberOfPartialOutputs].
checkPartialFanout ::
  ScriptContext ->
  -- | Progress state (extracted from either Closed or FanoutProgress input)
  FanoutProgressDatum ->
  -- | Number of outputs to distribute in this partial fanout
  Integer ->
  -- | Snapshot 'TxOutRef's for each distributed output, in output order.
  [TxOutRef] ->
  -- | Reference input containing CRS
  TxOutRef ->
  Bool
checkPartialFanout ctx@ScriptContext{scriptContextTxInfo = txInfo} progressDatum numberOfPartialOutputs subsetTxInRefs crsRef =
  mustNotMintOrBurn txInfo
    && afterContestationDeadline txInfo contestationDeadline
    && mustPreserveFanoutProgressState
    && mustConserveValue
    && checkCRSAndMembership
 where
  FanoutProgressDatum
    { parties
    , headId
    , contestationDeadline
    , accumulatorCommitment
    } = progressDatum

  -- Decode continuing output datum as FanoutProgressDatum (first output, at head address)
  FanoutProgressDatum
    { parties = parties'
    , headId = headId'
    , contestationDeadline = contestationDeadline'
    , accumulatorCommitment = newAccumulatorCommitment
    } = decodeHeadOutputFanoutProgressDatum ctx

  TxInfo{txInfoOutputs} = txInfo

  -- The distributed UTxO outputs are at indices [1..numberOfPartialOutputs].
  -- Index 0 is the continuing head output.
  distributedOutputs = L.take numberOfPartialOutputs (L.drop 1 txInfoOutputs)

  -- Ensure the continuing FanoutProgressDatum carries the correct parameters.
  -- accumulatorCommitment is NOT checked here — it is verified by checkCRSAndMembership.
  mustPreserveFanoutProgressState =
    traceIfFalse $(errorCode PartialFanoutChangedParameters) $
      parties' == parties
        && headId' == headId
        && contestationDeadline' == contestationDeadline

  -- The head input value must equal the continuing head output value plus the
  -- sum of all distributed outputs. This prevents stealing Ada by adding extra
  -- outputs that are not counted by the membership proof.
  mustConserveValue =
    traceIfFalse $(errorCode HeadValueIsNotPreserved) $
      headInValue == headOutValue <> F.foldMap txOutValue distributedOutputs
   where
    headInValue = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx
    headOutValue = txOutValue $ L.head txInfoOutputs

  -- See note in 'headIsFinalizedWith' about why we do not explicitly verify
  -- @length subsetTxInRefs == numberOfPartialOutputs@: 'L.zip' truncating
  -- mismatched inputs flows through to a wrong @P_S@ degree, which the
  -- pairing rejects without the cost of an extra list traversal.
  subsetScalars :: [Integer]
  subsetScalars = txOutsToSubsetScalars (L.zip subsetTxInRefs distributedOutputs)

  -- CRS reference lookup and membership check.
  -- The newAccumulatorCommitment from the continuing output datum serves as
  -- the proof: checkMembership verifies that the subset elements were correctly
  -- removed from the old accumulator, and the remaining commitment is valid.
  checkCRSAndMembership =
    traceIfFalse $(errorCode PartialFanoutMembershipFailed) $
      withCRSLookup txInfo crsRef $ \crsData ->
        checkMembershipPairing accumulatorCommitment newAccumulatorCommitment crsData subsetScalars
{-# INLINEABLE checkPartialFanout #-}

-- | Verify the final partial fanout transaction. Transitions FanoutProgress → Final:
-- distributes all remaining UTxOs and burns all head tokens.
--
-- Unlike intermediate steps, there is no continuing head output. All distributed
-- UTxOs start at index 0. Tokens must be burned.
--
-- Note: headId, parties, and contestationDeadline are not re-verified explicitly.
-- headId and parties are validated implicitly by mustBurnAllHeadTokens (wrong values
-- would target the wrong token set). contestationDeadline was locked in by earlier
-- checkPartialFanout steps and is trustworthy from the on-chain datum.
checkFinalPartialFanout ::
  ScriptContext ->
  -- | FanoutProgress state before the final fanout
  FanoutProgressDatum ->
  -- | Number of outputs to distribute
  Integer ->
  -- | Snapshot 'TxOutRef's for each distributed output, in output order.
  [TxOutRef] ->
  -- | Membership proof (quotient commitment G1 element)
  BuiltinBLS12_381_G1_Element ->
  -- | Reference input containing CRS
  TxOutRef ->
  Bool
checkFinalPartialFanout ScriptContext{scriptContextTxInfo = txInfo} progressDatum numberOfPartialOutputs subsetTxInRefs proof crsRef =
  mustBurnAllHeadTokens minted headId parties
    && afterContestationDeadline txInfo contestationDeadline
    && checkCRSAndMembership
 where
  FanoutProgressDatum{headId, parties, contestationDeadline, accumulatorCommitment} = progressDatum

  minted = txInfoMint txInfo

  TxInfo{txInfoOutputs} = txInfo

  -- See note in 'headIsFinalizedWith' about why we do not explicitly verify
  -- subsetTxInRefs length.
  subsetScalars :: [Integer]
  subsetScalars =
    txOutsToSubsetScalars
      (L.zip subsetTxInRefs (L.take numberOfPartialOutputs txInfoOutputs))

  checkCRSAndMembership =
    traceIfFalse $(errorCode FinalPartialFanoutMembershipFailed) $
      withCRSLookup txInfo crsRef $ \crsData ->
        checkMembershipPairing accumulatorCommitment proof crsData subsetScalars
{-# INLINEABLE checkFinalPartialFanout #-}

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

makeContestationDeadline :: ContestationPeriod -> ScriptContext -> POSIXTime
makeContestationDeadline cperiod ScriptContext{scriptContextTxInfo} =
  case ivTo (txInfoValidRange scriptContextTxInfo) of
    UpperBound (Finite time) _ -> addContestationPeriod time cperiod
    _ -> traceError $(errorCode CloseNoUpperBoundDefined)
{-# INLINEABLE makeContestationDeadline #-}

-- | This is safe only because usually Head transaction only consume one input.
getHeadInput :: ScriptContext -> TxInInfo
getHeadInput ctx = case findOwnInput ctx of
  Nothing -> traceError $(errorCode ScriptNotSpendingAHeadInput)
  Just x -> x
{-# INLINEABLE getHeadInput #-}

getHeadAddress :: ScriptContext -> Address
getHeadAddress = txOutAddress . txInInfoResolved . getHeadInput
{-# INLINEABLE getHeadAddress #-}

mustNotChangeParameters ::
  ([Party], [Party]) ->
  (ContestationPeriod, ContestationPeriod) ->
  (CurrencySymbol, CurrencySymbol) ->
  Bool
mustNotChangeParameters (parties', parties) (contestationPeriod', contestationPeriod) (headId', headId) =
  traceIfFalse $(errorCode ChangedParameters) $
    parties' == parties
      && contestationPeriod' == contestationPeriod
      && headId' == headId
{-# INLINEABLE mustNotChangeParameters #-}

-- XXX: We might not need to distinguish between the three cases here.
mustBeSignedByParticipant ::
  ScriptContext ->
  CurrencySymbol ->
  Bool
mustBeSignedByParticipant ScriptContext{scriptContextTxInfo = txInfo} headCurrencySymbol =
  case getPubKeyHash <$> txInfoSignatories txInfo of
    [signer] ->
      traceIfFalse $(errorCode SignerIsNotAParticipant) $
        signer `L.elem` (unTokenName <$> participationTokens)
    [] ->
      traceError $(errorCode NoSigners)
    _ ->
      traceError $(errorCode TooManySigners)
 where
  participationTokens = loop (txInfoInputs txInfo)
  loop = \case
    [] -> []
    (TxInInfo{txInInfoResolved} : rest) ->
      findParticipationTokens headCurrencySymbol (txOutValue txInInfoResolved) L.++ loop rest
{-# INLINEABLE mustBeSignedByParticipant #-}

findParticipationTokens :: CurrencySymbol -> Value -> [TokenName]
findParticipationTokens headCurrency (Value val) =
  case AssocMap.toList <$> AssocMap.lookup headCurrency val of
    Just tokens ->
      mapMaybe (\(tokenName, n) -> if n == 1 then Just tokenName else Nothing) tokens
    _ ->
      []
{-# INLINEABLE findParticipationTokens #-}

headOutputDatum :: ScriptContext -> Datum
headOutputDatum ctx =
  case txInfoOutputs txInfo of
    (o : _)
      | txOutAddress o == headAddress -> getTxOutDatum o
    _ -> traceError $(errorCode NotPayingToHead)
 where
  headAddress = getHeadAddress ctx

  ScriptContext{scriptContextTxInfo = txInfo} = ctx
{-# INLINEABLE headOutputDatum #-}

getTxOutDatum :: TxOut -> Datum
getTxOutDatum o =
  case txOutDatum o of
    NoOutputDatum -> traceError $(errorCode NoOutputDatumError)
    OutputDatumHash _dh -> traceError $(errorCode UnexpectedNonInlineDatum)
    OutputDatum d -> d
{-# INLINEABLE getTxOutDatum #-}

-- | Verify the multi-signature of a snapshot using given constituents 'headId',
-- 'version', 'number', 'utxoHash' and 'utxoToDecommitHash'. See
-- 'SignableRepresentation Snapshot' for more details.
verifySnapshotSignature :: [Party] -> (CurrencySymbol, SnapshotVersion, SnapshotNumber, Hash, Hash, Hash, Hash) -> [Signature] -> Bool
verifySnapshotSignature parties msg sigs =
  traceIfFalse $(errorCode SignatureVerificationFailed) $
    L.length parties == L.length sigs
      && L.all (uncurry $ verifyPartySignature msg) (L.zip parties sigs)
{-# INLINEABLE verifySnapshotSignature #-}

-- | Verify individual party signature of a snapshot. See
-- 'SignableRepresentation Snapshot' for more details.
verifyPartySignature :: (CurrencySymbol, SnapshotVersion, SnapshotNumber, Hash, Hash, Hash, Hash) -> Party -> Signature -> Bool
verifyPartySignature (headId, snapshotVersion, snapshotNumber, utxoHash, utxoToCommitHash, utxoToDecommitHash, accumulatorHash) party =
  verifyEd25519Signature (vkey party) message
 where
  message =
    Builtins.serialiseData (toBuiltinData headId)
      <> Builtins.serialiseData (toBuiltinData snapshotVersion)
      <> Builtins.serialiseData (toBuiltinData snapshotNumber)
      <> Builtins.serialiseData (toBuiltinData utxoHash)
      <> Builtins.serialiseData (toBuiltinData utxoToCommitHash)
      <> Builtins.serialiseData (toBuiltinData utxoToDecommitHash)
      <> Builtins.serialiseData (toBuiltinData accumulatorHash)
{-# INLINEABLE verifyPartySignature #-}

compiledValidator :: CompiledCode ValidatorType
compiledValidator =
  $$(PlutusTx.compile [||wrap headValidator||])
 where
  wrap = wrapValidator @DatumType @RedeemerType

validatorScript :: PlutusScript
validatorScript = PlutusScriptSerialised $ serialiseCompiledCode compiledValidator

decodeHeadOutputClosedDatum :: ScriptContext -> ClosedDatum
decodeHeadOutputClosedDatum ctx =
  -- XXX: fromBuiltinData is super big (and also expensive?)
  case fromBuiltinData @DatumType $ getDatum (headOutputDatum ctx) of
    Just (Closed closedDatum) -> closedDatum
    _ -> traceError $(errorCode WrongStateInOutputDatum)
{-# INLINEABLE decodeHeadOutputClosedDatum #-}

decodeHeadOutputFanoutProgressDatum :: ScriptContext -> FanoutProgressDatum
decodeHeadOutputFanoutProgressDatum ctx =
  case fromBuiltinData @DatumType $ getDatum (headOutputDatum ctx) of
    Just (FanoutProgress progressDatum) -> progressDatum
    _ -> traceError $(errorCode WrongStateInOutputDatum)
{-# INLINEABLE decodeHeadOutputFanoutProgressDatum #-}

decodeHeadOutputOpenDatum :: ScriptContext -> OpenDatum
decodeHeadOutputOpenDatum ctx =
  -- XXX: fromBuiltinData is super big (and also expensive?)
  case fromBuiltinData @DatumType $ getDatum (headOutputDatum ctx) of
    Just (Open openDatum) -> openDatum
    _ -> traceError $(errorCode WrongStateInOutputDatum)
{-# INLINEABLE decodeHeadOutputOpenDatum #-}

-- | Check that the lower validity bound of the transaction is strictly after
-- the contestation deadline. Used by all three fanout paths.
afterContestationDeadline :: TxInfo -> POSIXTime -> Bool
afterContestationDeadline txInfo deadline =
  case ivFrom (txInfoValidRange txInfo) of
    LowerBound (Finite time) _ ->
      traceIfFalse $(errorCode LowerBoundBeforeContestationDeadline) $
        time > deadline
    _ -> traceError $(errorCode FanoutNoLowerBoundDefined)
{-# INLINEABLE afterContestationDeadline #-}

-- | Look up the CRS datum from a reference input and pass it to a continuation.
-- Traces hard errors if the CRS reference input or datum is missing.
withCRSLookup ::
  TxInfo ->
  TxOutRef ->
  (CRSDatum -> Bool) ->
  Bool
withCRSLookup txInfo crsRef cont =
  case L.find (\txin -> txInInfoOutRef txin == crsRef) (txInfoReferenceInputs txInfo) of
    Nothing -> traceError $(errorCode MissingCRSRefInput)
    Just txInInfo ->
      let crsData = case fromBuiltinData @CRSDatum $ getDatum (getTxOutDatum (txInInfoResolved txInInfo)) of
            Just d -> d
            _ -> traceError $(errorCode MissingCRSDatum)
       in case crsData of
            [] -> traceError $(errorCode MissingCRSDatum)
            _ -> cont crsData
{-# INLINEABLE withCRSLookup #-}

-- | Compute the accumulator scalar for each @(TxOutRef, TxOut)@ pair.
-- Used by all three fanout validators ('headIsFinalizedWith', 'checkPartialFanout',
-- 'checkFinalPartialFanout') to produce subset scalars for 'checkMembershipPairing'.
-- Each scalar is @blake2b_224(hashTxRefAndOuts [(txOutRef, txOut)])@, which equals
-- @blake2b_224(sha2_256(serialise(txOutRef, txOut)))@ because 'hashTxRefAndOuts'
-- uses sha2_256 internally (matching what 'Accumulator.addElement' computes off-chain).
-- Binding to the originating 'TxOutRef' keeps the scalar unique even when two
-- snapshot UTxOs share identical 'TxOut' content.
txOutsToSubsetScalars :: [(TxOutRef, TxOut)] -> [Integer]
txOutsToSubsetScalars pairs =
  let elementHash p = blake2b_224 (hashTxRefAndOuts [p])
   in fmap (Builtins.byteStringToInteger BigEndian . elementHash) pairs
{-# INLINEABLE txOutsToSubsetScalars #-}

emptyHash :: Hash
emptyHash = hashTxOuts []
{-# INLINEABLE emptyHash #-}
