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
  OnChainParameterUpdate (..),
  OpenDatum (..),
  Signature,
  SnapshotNumber,
  SnapshotVersion,
  State (..),
  UpdateParametersRedeemer (..),
  progressFromClosed,
 )
import Hydra.Contract.Util (hasST, hashPreSerializedCommits, hashTxOuts, mustBurnAllHeadTokens, mustNotMintOrBurn, mustPreserveHeadValue)
import Hydra.Data.ContestationPeriod (ContestationPeriod, addContestationPeriod, milliseconds)
import Hydra.Data.Party (Party (vkey))
import Hydra.Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.V1.Time (fromMilliSeconds)
import PlutusLedgerApi.V1.Value (geq)
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
  mintValueToMap,
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
    (Open openDatum, UpdateParameters redeemer) ->
      checkUpdateParameters ctx openDatum redeemer
    (Closed closedDatum, Contest redeemer) ->
      checkContest ctx closedDatum redeemer
    (Closed closedDatum, Fanout{numberOfFanoutOutputs, numberOfCommitOutputs, numberOfDecommitOutputs}) ->
      headIsFinalizedWith ctx closedDatum numberOfFanoutOutputs numberOfCommitOutputs numberOfDecommitOutputs
    (Closed closedDatum, PartialFanout{numberOfPartialOutputs, crsRef}) ->
      checkPartialFanout crsHash ctx (progressFromClosed closedDatum) numberOfPartialOutputs crsRef
    (FanoutProgress progressDatum, PartialFanout{numberOfPartialOutputs, crsRef}) ->
      checkPartialFanout crsHash ctx progressDatum numberOfPartialOutputs crsRef
    (FanoutProgress progressDatum, FinalPartialFanout{numberOfPartialOutputs, proof, crsRef}) ->
      checkFinalPartialFanout crsHash ctx progressDatum numberOfPartialOutputs proof crsRef
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

  IncrementRedeemer{signature, snapshotNumber, increment} = redeemer

  claimedDepositIsSpent =
    traceIfFalse $(errorCode DepositNotSpent) $
      increment `L.elem` (txInInfoOutRef <$> txInfoInputs txInfo)

  checkSnapshotSignature =
    verifySnapshotSignature nextParties (nextHeadId, prevVersion, snapshotNumber, nextUtxoHash, depositHash, emptyHash, nextAccumulatorHash) signature

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
    verifySnapshotSignature nextParties (nextHeadId, prevVersion, snapshotNumber, nextUtxoHash, emptyHash, decommitUtxoHash, nextAccumulatorHash) signature

  mustDecreaseValue =
    traceIfFalse $(errorCode HeadValueIsNotPreserved) $
      headInValue == headOutValue <> F.foldMap txOutValue decommitOutputs

  mustIncreaseVersion =
    traceIfFalse $(errorCode VersionNotIncremented) $
      nextVersion == prevVersion + 1

  decommitUtxoHash = hashTxOuts decommitOutputs

  DecrementRedeemer{signature, snapshotNumber, numberOfDecommitOutputs} = redeemer

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

-- | Verify an UpdateParameters transaction (issue #1813,
-- dynamic-head-participants). Stays in the open state but rewrites the
-- 'parties' field of the head datum according to the multi-signed
-- 'parameterUpdate', burns or mints the relevant participation token, and
-- bumps the snapshot version. All other open-state datum fields ('headId',
-- 'contestationPeriod', 'headSeed', 'utxoHash') must be preserved.
--
-- 'RemovePartyOC' burns the leaving party's PT and shrinks the head output
-- value by exactly that token. 'AddPartyOC' mints a fresh PT and grows the
-- head output value by exactly that token.
checkUpdateParameters ::
  ScriptContext ->
  -- | Open state before the update
  OpenDatum ->
  UpdateParametersRedeemer ->
  Bool
checkUpdateParameters ctx@ScriptContext{scriptContextTxInfo = txInfo} openBefore redeemer =
  mustOnlyChangeParties
    && mustApplyUpdateToParties
    && mustMintOrBurnParticipationToken
    && mustIncreaseVersion
    && mustPreserveHeadValueAdjustedForPT
    && mustBeSignedByParticipant ctx prevHeadId
    && checkSnapshotSignature
 where
  UpdateParametersRedeemer{signature, snapshotNumber, parameterUpdate} = redeemer

  OpenDatum
    { headSeed = prevHeadSeed
    , parties = prevParties
    , contestationPeriod = prevCperiod
    , headId = prevHeadId
    , version = prevVersion
    , utxoHash = prevUtxoHash
    , accumulatorHash = prevAccumulatorHash
    } = openBefore

  OpenDatum
    { headSeed = nextHeadSeed
    , parties = nextParties
    , contestationPeriod = nextCperiod
    , headId = nextHeadId
    , version = nextVersion
    , utxoHash = nextUtxoHash
    , accumulatorHash = nextAccumulatorHash
    } = decodeHeadOutputOpenDatum ctx

  mustOnlyChangeParties =
    traceIfFalse $(errorCode UpdateParametersChangedOtherFields) $
      prevHeadSeed == nextHeadSeed
        && prevCperiod == nextCperiod
        && prevHeadId == nextHeadId
        && prevUtxoHash == nextUtxoHash
        && prevAccumulatorHash == nextAccumulatorHash

  mustApplyUpdateToParties =
    traceIfFalse $(errorCode FailedUpdateParametersBadPartyChange) $
      case parameterUpdate of
        RemovePartyOC leavingParty _ ->
          nextParties == removeFirst leavingParty prevParties
        AddPartyOC joiningParty _ ->
          -- New party is appended to the end, preserving order of existing
          -- parties (matters for leader-rotation determinism off chain).
          nextParties == prevParties L.++ [joiningParty]

  -- Remove the first occurrence of x from the list, preserving order. Equivalent
  -- in spirit to Data.List.delete but inlined for plutus-tx.
  removeFirst x = go
   where
    go = \case
      [] -> []
      (y : ys)
        | x == y -> ys
        | otherwise -> y : go ys

  mustMintOrBurnParticipationToken =
    case parameterUpdate of
      RemovePartyOC _ tokenName ->
        traceIfFalse $(errorCode FailedUpdateParametersBadPTBurn) $
          ptMintsForHead prevHeadId == [(tokenName, -1)]
      AddPartyOC _ tokenName ->
        traceIfFalse $(errorCode FailedUpdateParametersBadPTMint) $
          ptMintsForHead prevHeadId == [(tokenName, 1)]

  -- All participation-token mints/burns for the head's currency, as
  -- (asset name, signed quantity) pairs.
  ptMintsForHead headCurrency =
    maybe
      []
      AssocMap.toList
      (AssocMap.lookup headCurrency (mintValueToMap (txInfoMint txInfo)))

  -- The output head value must equal the input head value adjusted by exactly
  -- the participation token that was minted (Add) or burned (Remove). For
  -- Remove, output + burnedPT == input. For Add, input + mintedPT == output.
  mustPreserveHeadValueAdjustedForPT =
    traceIfFalse $(errorCode HeadValueIsNotPreserved) $
      case parameterUpdate of
        RemovePartyOC _ tokenName ->
          headInValue == headOutValue <> singletonPTValue prevHeadId tokenName
        AddPartyOC _ tokenName ->
          headInValue <> singletonPTValue prevHeadId tokenName == headOutValue

  -- A 'Value' containing exactly one participation token of the given asset
  -- name under the head's currency.
  singletonPTValue currency tokenName =
    Value $ AssocMap.singleton currency $ AssocMap.singleton tokenName 1

  headInValue = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx
  headOutValue = txOutValue $ L.head $ txInfoOutputs txInfo

  mustIncreaseVersion =
    traceIfFalse $(errorCode VersionNotIncremented) $
      nextVersion == prevVersion + 1

  checkSnapshotSignature =
    verifyParameterUpdateSignature
      prevParties
      (prevHeadId, prevVersion, snapshotNumber, prevUtxoHash, emptyHash, emptyHash, prevAccumulatorHash)
      parameterUpdate
      signature

-- NOTE: We deliberately verify the signature against the *previous* parties
-- list for both 'RemovePartyOC' and 'AddPartyOC'. For 'RemoveParty', this lets
-- the leaving party sign their own departure (they are still in 'prevParties').
-- For 'AddParty', the joining party does not need to multi-sign the on-chain
-- transition itself — their consent is captured out-of-band when they start
-- participating in the head; the multi-sig from existing parties is what
-- authorizes the parameter change on chain. This keeps the off-chain
-- 'ifAllMembersHaveSigned' check (which uses 'OpenState.parameters.parties'
-- = previous parties) consistent with what the validator enforces.
{-# INLINEABLE checkUpdateParameters #-}

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
      CloseAny{signature} ->
        traceIfFalse $(errorCode FailedCloseAny) $
          snapshotNumber' > 0
            && alphaUTxOHash' == emptyHash
            && omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version, snapshotNumber', utxoHash', emptyHash, emptyHash, closedAccumulatorHash)
              signature
      CloseUnusedDec{signature} ->
        traceIfFalse $(errorCode FailedCloseUnusedDec) $
          alphaUTxOHash' == emptyHash
            && omegaUTxOHash' /= emptyHash
            && verifySnapshotSignature
              parties
              (headId, version, snapshotNumber', utxoHash', emptyHash, omegaUTxOHash', closedAccumulatorHash)
              signature
      CloseUsedDec{signature, alreadyDecommittedUTxOHash} ->
        traceIfFalse $(errorCode FailedCloseUsedDec) $
          alphaUTxOHash' == emptyHash
            && omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version - 1, snapshotNumber', utxoHash', emptyHash, alreadyDecommittedUTxOHash, closedAccumulatorHash)
              signature
      CloseUnusedInc{signature, alreadyCommittedUTxOHash} ->
        traceIfFalse $(errorCode FailedCloseUnusedInc) $
          alphaUTxOHash' == emptyHash
            && omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version, snapshotNumber', utxoHash', alreadyCommittedUTxOHash, emptyHash, closedAccumulatorHash)
              signature
      CloseUsedInc{signature, alreadyCommittedUTxOHash} ->
        traceIfFalse $(errorCode FailedCloseUsedInc) $
          alphaUTxOHash' == alreadyCommittedUTxOHash
            && omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version - 1, snapshotNumber', utxoHash', alreadyCommittedUTxOHash, emptyHash, closedAccumulatorHash)
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

  closedAccumulatorHash =
    Builtins.blake2b_256 (Builtins.bls12_381_G1_compress accumulatorCommitment')

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
-- The UTxO hash checks are sufficient to protect correctness here — the KZG membership
-- proof is redundant because the distributed outputs are fully pinned by the signed utxoHash.
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
  Bool
headIsFinalizedWith ScriptContext{scriptContextTxInfo = txInfo} closedDatum numberOfFanoutOutputs numberOfCommitOutputs numberOfDecommitOutputs =
  mustBurnAllHeadTokens minted headId parties
    && hashChecks
    && afterContestationDeadline txInfo contestationDeadline
 where
  minted = txInfoMint txInfo

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

  ClosedDatum{utxoHash, alphaUTxOHash, omegaUTxOHash, parties, headId, contestationDeadline} = closedDatum
  TxInfo{txInfoOutputs} = txInfo
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

  subsetScalars :: [Integer]
  subsetScalars = txOutsToSubsetScalars distributedOutputs

  -- CRS reference lookup and membership check.
  -- The newAccumulatorCommitment from the continuing output datum serves as
  -- the proof: checkMembership verifies that the subset elements were correctly
  -- removed from the old accumulator, and the remaining commitment is valid.
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
  FanoutProgressDatum{headId, parties, contestationDeadline, accumulatorCommitment} = progressDatum

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

  -- All head input value must be accounted by distributed outputs and burned tokens.
  -- XXX: geq rather than == because the head UTxO carries extra ADA (min-UTxO overhead
  -- from initialization) not represented in the L2 UTxO set.
  mustConserveValue =
    traceIfFalse $(errorCode HeadValueIsNotPreserved) $
      headInValue `geq` (F.foldMap txOutValue distributedOutputs <> mintValueBurned minted)
   where
    headInValue = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  subsetScalars :: [Integer]
  subsetScalars = txOutsToSubsetScalars distributedOutputs

  checkCRSAndMembership =
    traceIfFalse $(errorCode FinalPartialFanoutMembershipFailed) $
      withCRSLookup crsHash txInfo crsRef $ \crsData ->
        checkMembershipPairing accumulatorCommitment proof crsData subsetScalars
          -- When all accumulator elements are distributed the quotient polynomial is 1,
          -- so the proof must equal the G1 generator. This prevents a valid subset proof
          -- from being used to leave UTxOs unaccounted.
          && Builtins.bls12_381_G1_compress proof == Builtins.bls12_381_G1_compressed_generator
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
--
-- XXX: Maybe have a newtype for all the hashes instead of so many fields of
-- the same type.
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

-- | Verify the multi-signature of a snapshot that carries a pending
-- 'OnChainParameterUpdate'. Mirrors the off-chain
-- 'SignableRepresentation Snapshot': the unconditional accumulator hash term
-- (added when the on-chain accumulator was introduced) plus the CBOR encoding
-- of the parameter update appended at the end.
verifyParameterUpdateSignature ::
  [Party] ->
  (CurrencySymbol, SnapshotVersion, SnapshotNumber, Hash, Hash, Hash, Hash) ->
  OnChainParameterUpdate ->
  [Signature] ->
  Bool
verifyParameterUpdateSignature parties msg parameterUpdate sigs =
  traceIfFalse $(errorCode SignatureVerificationFailed) $
    L.length parties == L.length sigs
      && L.all (uncurry $ verifyParameterUpdatePartySignature msg parameterUpdate) (L.zip parties sigs)
{-# INLINEABLE verifyParameterUpdateSignature #-}

verifyParameterUpdatePartySignature ::
  (CurrencySymbol, SnapshotVersion, SnapshotNumber, Hash, Hash, Hash, Hash) ->
  OnChainParameterUpdate ->
  Party ->
  Signature ->
  Bool
verifyParameterUpdatePartySignature (headId, snapshotVersion, snapshotNumber, utxoHash, utxoToCommitHash, utxoToDecommitHash, accumulatorHash) parameterUpdate party =
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
      <> Builtins.serialiseData (toBuiltinData parameterUpdate)
{-# INLINEABLE verifyParameterUpdatePartySignature #-}

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

emptyHash :: Hash
emptyHash = hashTxOuts []
{-# INLINEABLE emptyHash #-}
