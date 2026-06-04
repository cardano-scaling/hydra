{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoPolyKinds #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:optimize #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:target-version=1.1.0 #-}

module Hydra.Contract.Head where

import PlutusTx.Prelude

import GHC.ByteOrder (ByteOrder (BigEndian))
import Hydra.Cardano.Api (
  PlutusScript,
  pattern PlutusScriptSerialised,
 )
import Hydra.Contract.CRS (CRSDatum, checkMembershipPairing)
import Hydra.Contract.CRS qualified as CRS
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
import Hydra.Contract.Util (hasST, hashTxOuts, mustBurnAllHeadTokens, mustNotMintOrBurn, mustPreserveHeadValue)
import Hydra.Data.ContestationPeriod (ContestationPeriod, addContestationPeriod, milliseconds)
import Hydra.Data.Party (Party (vkey))
import Hydra.Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.V1.Time (fromMilliSeconds)
import PlutusLedgerApi.V1.Value (adaSymbol, adaToken, singleton)
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
  ScriptHash,
  TokenName (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef,
  UpperBound (..),
  Value (Value),
  mintValueBurned,
 )
import PlutusLedgerApi.V3.Contexts (findOwnInput)
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
  ScriptHash ->
  State ->
  Input ->
  ScriptContext ->
  Bool
headValidator crsHash oldState input ctx =
  case (oldState, input) of
    (Open openDatum, Increment redeemer) ->
      checkIncrement ctx openDatum redeemer
    (Open openDatum, Decrement redeemer) ->
      checkDecrement ctx openDatum redeemer
    (Open openDatum, Close redeemer) ->
      checkClose ctx openDatum redeemer
    (Closed closedDatum, Contest redeemer) ->
      checkContest ctx closedDatum redeemer
    (Closed closedDatum, Fanout{numberOfFanoutOutputs, proof, crsRef}) ->
      headIsFinalizedWith crsHash ctx closedDatum numberOfFanoutOutputs proof crsRef
    (Closed closedDatum, PartialFanout{numberOfPartialOutputs, crsRef}) ->
      checkPartialFanout crsHash ctx (progressFromClosed closedDatum) numberOfPartialOutputs crsRef
    (FanoutProgress progressDatum, PartialFanout{numberOfPartialOutputs, crsRef}) ->
      checkPartialFanout crsHash ctx progressDatum numberOfPartialOutputs crsRef
    (FanoutProgress progressDatum, FinalPartialFanout{numberOfPartialOutputs, proof, crsRef}) ->
      checkFinalPartialFanout crsHash ctx progressDatum numberOfPartialOutputs proof crsRef
    _ ->
      traceError $(errorCode InvalidHeadStateTransition)

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

  IncrementRedeemer{signature, snapshotNumber, increment} = redeemer

  claimedDepositIsSpent =
    traceIfFalse $(errorCode DepositNotSpent) $
      increment `L.elem` (txInInfoOutRef <$> txInfoInputs txInfo)

  checkSnapshotSignature =
    verifySnapshotSignature nextParties (nextHeadId, prevVersion, snapshotNumber, nextAccumulatorHash) signature

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
    { parties = nextParties
    , contestationPeriod = nextCperiod
    , headId = nextHeadId
    , version = nextVersion
    , accumulatorHash = nextAccumulatorHash
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
    verifySnapshotSignature nextParties (nextHeadId, prevVersion, snapshotNumber, nextAccumulatorHash) signature

  mustDecreaseValue =
    traceIfFalse $(errorCode HeadValueIsNotPreserved) $
      headInValue == headOutValue <> F.foldMap txOutValue decommitOutputs

  mustIncreaseVersion =
    traceIfFalse $(errorCode VersionNotIncremented) $
      nextVersion == prevVersion + 1

  DecrementRedeemer{signature, snapshotNumber, numberOfDecommitOutputs} = redeemer

  OpenDatum
    { parties = prevParties
    , contestationPeriod = prevCperiod
    , headId = prevHeadId
    , version = prevVersion
    } = openBefore

  OpenDatum
    { parties = nextParties
    , contestationPeriod = nextCperiod
    , headId = nextHeadId
    , version = nextVersion
    , accumulatorHash = nextAccumulatorHash
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
    , contestationPeriod = cperiod
    , headId
    , version
    } = openBefore

  hasBoundedValidity =
    traceIfFalse $(errorCode HasBoundedValidityCheckFailed) $
      tMax - tMin <= cp

  ClosedDatum
    { snapshotNumber = snapshotNumber'
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
        -- For the initial snapshot the accumulator must commit to the empty UTxO set,
        -- whose KZG commitment is the G1 generator (constant polynomial 1).
        traceIfFalse $(errorCode FailedCloseInitial) $
          version == 0
            && snapshotNumber' == 0
            -- The empty-accumulator commitment is the G1 generator
            -- (getFinalPoly [] = [1], so getG1Commitment [G1] [1] = G1). Pin it
            -- so a closer cannot seed a degenerate commitment that would later
            -- be trusted by progressFromClosed and checkMembershipPairing.
            && isG1Generator accumulatorCommitment'
      CloseAny{signature, accumulatorHash} ->
        traceIfFalse $(errorCode FailedCloseAny) $
          snapshotNumber' > 0
            && verifySnapshotSignature
              parties
              (headId, version, snapshotNumber', accumulatorHash)
              signature
      CloseUnused{signature, accumulatorHash} ->
        traceIfFalse $(errorCode FailedCloseUnused) $
          verifySnapshotSignature
            parties
            (headId, version, snapshotNumber', accumulatorHash)
            signature
      CloseUsed{signature, accumulatorHash} ->
        traceIfFalse $(errorCode FailedCloseUsed) $
          verifySnapshotSignature
            parties
            (headId, version - 1, snapshotNumber', accumulatorHash)
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
      CloseUnused{accumulatorHash} -> check' accumulatorHash
      CloseUsed{accumulatorHash} -> check' accumulatorHash
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
    && mustPreserveHeadAdaOverhead headAdaOverhead headAdaOverhead'
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
      ContestUnused{signature, accumulatorHash} ->
        traceIfFalse $(errorCode FailedContestUnused) $
          verifySnapshotSignature
            parties
            (headId, version, snapshotNumber', accumulatorHash)
            signature
      ContestUsed{signature, accumulatorHash} ->
        traceIfFalse $(errorCode FailedContestUsed) $
          verifySnapshotSignature
            parties
            (headId, version - 1, snapshotNumber', accumulatorHash)
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
    , headAdaOverhead
    } = closedDatum

  ClosedDatum
    { snapshotNumber = snapshotNumber'
    , parties = parties'
    , contestationDeadline = contestationDeadline'
    , contestationPeriod = contestationPeriod'
    , headId = headId'
    , contesters = contesters'
    , version = version'
    , accumulatorCommitment = accumulatorCommitment'
    , headAdaOverhead = headAdaOverhead'
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
      ContestUnused{accumulatorHash} -> check' accumulatorHash
      ContestUsed{accumulatorHash} -> check' accumulatorHash
   where
    check' = mustMatchAccumulatorCommitmentHash accumulatorCommitment'
{-# INLINEABLE checkContest #-}

-- | Verify a fanout transaction using a KZG membership proof.
-- All distributed outputs are verified as members of the accumulator in a single proof.
headIsFinalizedWith ::
  ScriptHash ->
  ScriptContext ->
  -- | Closed state before the fanout
  ClosedDatum ->
  -- | Number of distributed UTxO outputs (excludes change output)
  Integer ->
  -- | Membership proof (quotient commitment G1 element)
  BuiltinBLS12_381_G1_Element ->
  -- | Reference input containing CRS
  TxOutRef ->
  Bool
headIsFinalizedWith crsHash ctx closedDatum numberOfFanoutOutputs proof crsRef =
  mustBurnAllHeadTokens minted headId parties
    && afterContestationDeadline txInfo contestationDeadline
    && checkCRSAndMembership
    && mustConserveValue
 where
  ScriptContext{scriptContextTxInfo = txInfo} = ctx

  minted = txInfoMint txInfo

  TxInfo{txInfoOutputs} = txInfo

  ClosedDatum{accumulatorCommitment, parties, headId, contestationDeadline, headAdaOverhead} = closedDatum

  fanoutOutputs = L.take numberOfFanoutOutputs txInfoOutputs

  subsetScalars :: [Integer]
  subsetScalars = txOutsToSubsetScalars fanoutOutputs

  -- Subset membership proof: all fanout outputs are members of the accumulator.
  -- isG1Generator is intentionally omitted — pre-settled UTxOs (decommitted/deposited
  -- before Close) remain in the accumulator but are not fanned out. Completeness is
  -- enforced by mustConserveValue instead.
  checkCRSAndMembership =
    traceIfFalse $(errorCode FanoutUTxOHashMismatch) $
      withCRSLookup crsHash txInfo crsRef $ \crsData ->
        checkMembershipPairing accumulatorCommitment proof crsData subsetScalars

  -- Strict equality: fanout outputs + burned tokens + fixed overhead must equal the
  -- full head input value. headAdaOverhead is the lovelace locked in the head UTxO
  -- that is not part of any L2 UTxO (min-UTxO overhead set at CollectCom).
  mustConserveValue =
    traceIfFalse $(errorCode HeadValueIsNotPreserved) $
      headInValue
        == F.foldMap txOutValue fanoutOutputs
        <> mintValueBurned minted
        <> singleton adaSymbol adaToken headAdaOverhead
   where
    headInValue = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx
{-# INLINEABLE headIsFinalizedWith #-}

-- | Verify a partial fanout transaction. Transitions either Closed → FanoutProgress
-- or FanoutProgress → FanoutProgress: distributes a subset of UTxOs and continues
-- with a smaller FanoutProgressDatum.
--
-- The continuing head output must be the first transaction output. Distributed
-- UTxOs follow at indices [1 .. numberOfPartialOutputs].
checkPartialFanout ::
  ScriptHash ->
  ScriptContext ->
  -- | Progress state (extracted from either Closed or FanoutProgress input)
  FanoutProgressDatum ->
  -- | Number of outputs to distribute in this partial fanout
  Integer ->
  -- | Reference input containing CRS
  TxOutRef ->
  Bool
checkPartialFanout crsHash ctx@ScriptContext{scriptContextTxInfo = txInfo} progressDatum numberOfPartialOutputs crsRef =
  mustHaveOutputs
    && mustNotBeLastBatch
    && mustNotMintOrBurn txInfo
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
    , headAdaOverhead
    } = progressDatum

  -- Decode continuing output datum as FanoutProgressDatum (first output, at head address)
  FanoutProgressDatum
    { parties = parties'
    , headId = headId'
    , contestationDeadline = contestationDeadline'
    , accumulatorCommitment = newAccumulatorCommitment
    , headAdaOverhead = headAdaOverhead'
    } = decodeHeadOutputFanoutProgressDatum ctx

  -- Guard against numberOfPartialOutputs = 0: with an empty subset the KZG
  -- check degenerates to e(A,G2) = e(newAcc,G2), passing trivially when
  -- newAcc = A. The exact-equality value check (==) prevents fund theft, but
  -- the zero-output path is semantically meaningless and wastes budget.
  mustHaveOutputs =
    traceIfFalse $(errorCode PartialFanoutZeroOutputs) $
      numberOfPartialOutputs > 0

  -- Prevent distributing ALL remaining elements via PartialFanout instead of
  -- FinalPartialFanout. When newAccumulatorCommitment = G1_generator all
  -- elements have been removed, so the next step MUST be FinalPartialFanout
  -- (which burns tokens). Using PartialFanout for the last batch produces a
  -- stuck FanoutProgress UTxO whose tokens can never be burned.
  mustNotBeLastBatch =
    traceIfFalse $(errorCode PartialFanoutCannotBeLastBatch) $
      not (isG1Generator newAccumulatorCommitment)

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
        && headAdaOverhead' == headAdaOverhead

  -- The head input value must equal the continuing head output value plus the
  -- sum of all distributed outputs. This prevents stealing Ada by adding extra
  -- outputs that are not counted by the membership proof.
  mustConserveValue =
    traceIfFalse $(errorCode HeadValueIsNotPreserved) $
      headInValue == headOutValue <> F.foldMap txOutValue distributedOutputs
   where
    headInValue = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx
    headOutValue = txOutValue $ L.head txInfoOutputs

  subsetScalars :: [Integer]
  subsetScalars = txOutsToSubsetScalars distributedOutputs

  -- CRS reference lookup and membership check.
  -- The newAccumulatorCommitment from the continuing output datum serves as
  -- the proof: checkMembership verifies that the subset elements were correctly
  -- removed from the accumulator.
  checkCRSAndMembership =
    traceIfFalse $(errorCode PartialFanoutMembershipFailed) $
      withCRSLookup crsHash txInfo crsRef $ \crsData ->
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
  ScriptHash ->
  ScriptContext ->
  -- | FanoutProgress state before the final fanout
  FanoutProgressDatum ->
  -- | Number of outputs to distribute
  Integer ->
  -- | Membership proof (quotient commitment G1 element)
  BuiltinBLS12_381_G1_Element ->
  -- | Reference input containing CRS
  TxOutRef ->
  Bool
checkFinalPartialFanout crsHash ctx@ScriptContext{scriptContextTxInfo = txInfo} progressDatum numberOfPartialOutputs proof crsRef =
  mustHaveOutputs
    && mustBurnAllHeadTokens minted headId parties
    && afterContestationDeadline txInfo contestationDeadline
    && checkCRSAndMembership
    && mustConserveValue
 where
  FanoutProgressDatum{headId, parties, contestationDeadline, accumulatorCommitment, headAdaOverhead} = progressDatum

  minted = txInfoMint txInfo

  TxInfo{txInfoOutputs} = txInfo

  -- Guard against numberOfPartialOutputs = 0: with an empty subset the KZG check
  -- degenerates to e(A,G2) = e(proof,G2), which passes whenever proof = A. Since A
  -- is public (from the datum), any third party could satisfy the check with zero
  -- distributed outputs and route all head ADA to themselves under the geq value check.
  mustHaveOutputs =
    traceIfFalse $(errorCode FinalPartialFanoutZeroOutputs) $
      numberOfPartialOutputs > 0

  distributedOutputs = L.take numberOfPartialOutputs txInfoOutputs

  -- Strict equality: distributed outputs + burned tokens + fixed overhead must equal
  -- the full head input value. isG1Generator is omitted for the same reason as in
  -- headIsFinalizedWith — pre-settled UTxOs may remain in the accumulator.
  mustConserveValue =
    traceIfFalse $(errorCode HeadValueIsNotPreserved) $
      headInValue
        == F.foldMap txOutValue distributedOutputs
        <> mintValueBurned minted
        <> singleton adaSymbol adaToken headAdaOverhead
   where
    headInValue = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  subsetScalars :: [Integer]
  subsetScalars = txOutsToSubsetScalars distributedOutputs

  checkCRSAndMembership =
    traceIfFalse $(errorCode FinalPartialFanoutMembershipFailed) $
      withCRSLookup crsHash txInfo crsRef $ \crsData ->
        checkMembershipPairing accumulatorCommitment proof crsData subsetScalars
{-# INLINEABLE checkFinalPartialFanout #-}

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

isG1Generator :: BuiltinBLS12_381_G1_Element -> Bool
isG1Generator g = Builtins.bls12_381_G1_compress g == Builtins.bls12_381_G1_compressed_generator
{-# INLINEABLE isG1Generator #-}

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

mustPreserveHeadAdaOverhead :: Integer -> Integer -> Bool
mustPreserveHeadAdaOverhead overhead overhead' =
  traceIfFalse $(errorCode ChangedHeadAdaOverhead) $
    overhead' == overhead
{-# INLINEABLE mustPreserveHeadAdaOverhead #-}

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
-- 'version', 'number', and 'accumulatorHash'. See 'SignableRepresentation Snapshot'
-- for more details.
verifySnapshotSignature :: [Party] -> (CurrencySymbol, SnapshotVersion, SnapshotNumber, Hash) -> [Signature] -> Bool
verifySnapshotSignature parties msg sigs =
  traceIfFalse $(errorCode SignatureVerificationFailed) $
    L.length parties == L.length sigs
      && L.all (uncurry $ verifyPartySignature msg) (L.zip parties sigs)
{-# INLINEABLE verifySnapshotSignature #-}

-- | Verify individual party signature of a snapshot. See
-- 'SignableRepresentation Snapshot' for more details.
verifyPartySignature :: (CurrencySymbol, SnapshotVersion, SnapshotNumber, Hash) -> Party -> Signature -> Bool
verifyPartySignature (headId, snapshotVersion, snapshotNumber, accumulatorHash) party =
  verifyEd25519Signature (vkey party) message
 where
  message =
    Builtins.serialiseData (toBuiltinData headId)
      <> Builtins.serialiseData (toBuiltinData snapshotVersion)
      <> Builtins.serialiseData (toBuiltinData snapshotNumber)
      <> Builtins.serialiseData (toBuiltinData accumulatorHash)
{-# INLINEABLE verifyPartySignature #-}

unappliedValidator :: CompiledCode (ScriptHash -> ValidatorType)
unappliedValidator =
  $$(PlutusTx.compile [||wrap . headValidator||])
 where
  wrap = wrapValidator @DatumType @RedeemerType

compiledValidator :: CompiledCode ValidatorType
compiledValidator =
  unappliedValidator
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 (scriptValidatorHash CRS.validatorScript)

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

-- | Find a CRS reference input by 'TxOutRef' and decode its non-empty datum.
-- Errors on missing input, undecoded datum, or empty CRS list.
resolveCRS :: TxInfo -> TxOutRef -> (TxOut, CRSDatum)
resolveCRS txInfo crsRef =
  case L.find (\txin -> txInInfoOutRef txin == crsRef) (txInfoReferenceInputs txInfo) of
    Nothing -> traceError $(errorCode MissingCRSRefInput)
    Just txInInfo ->
      let resolved = txInInfoResolved txInInfo
       in case fromBuiltinData @CRSDatum $ getDatum (getTxOutDatum resolved) of
            Just d@(_ : _) -> (resolved, d)
            _ -> traceError $(errorCode MissingCRSDatum)
{-# INLINEABLE resolveCRS #-}

-- | Look up the CRS datum from a reference input and pass it to a continuation.
-- Verifies that the reference input carries the expected CRS validator script hash.
withCRSLookup ::
  ScriptHash ->
  TxInfo ->
  TxOutRef ->
  (CRSDatum -> Bool) ->
  Bool
withCRSLookup expectedHash txInfo crsRef cont =
  let (resolved, crsData) = resolveCRS txInfo crsRef
   in if txOutReferenceScript resolved /= Just expectedHash
        then traceError $(errorCode InvalidCRSRefScript)
        else cont crsData
{-# INLINEABLE withCRSLookup #-}

-- | Compute the accumulator scalar for each output in the list.
-- Used by all three fanout validators ('headIsFinalizedWith', 'checkPartialFanout',
-- 'checkFinalPartialFanout') to produce subset scalars for 'checkMembershipPairing'.
-- Each scalar is blake2b_224(hashTxOuts [txOut]), which equals
-- blake2b_224(sha2_256(serialised)) because 'hashTxOuts' uses sha2_256 internally
-- (matching what 'Accumulator.addElement' computes off-chain).
txOutsToSubsetScalars :: [TxOut] -> [Integer]
txOutsToSubsetScalars outputs =
  let elementHash txOut = blake2b_224 (hashTxOuts [txOut])
   in fmap (Builtins.byteStringToInteger BigEndian . elementHash) outputs
{-# INLINEABLE txOutsToSubsetScalars #-}
