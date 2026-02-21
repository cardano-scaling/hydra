{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Hydra.Contract.Head where

import PlutusTx.Prelude

import Hydra.Cardano.Api (
  PlutusScript,
  pattern PlutusScriptSerialised,
 )
import Hydra.Contract.Commit (Commit (..))
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Contract.HeadError (HeadError (..), errorCode)
import Hydra.Contract.HeadState (
  CloseRedeemer (..),
  ClosedDatum (..),
  ContestRedeemer (..),
  DecrementRedeemer (..),
  Hash,
  IncrementRedeemer (..),
  Input (..),
  OpenDatum (..),
  Signature,
  SnapshotNumber,
  SnapshotVersion,
  State (..),
 )
import Hydra.Contract.Util (hasST, hashPreSerializedCommits, hashTxOuts, mustBurnAllHeadTokens, mustNotMintOrBurn)
import Hydra.Data.ContestationPeriod (ContestationPeriod, addContestationPeriod, milliseconds)
import Hydra.Data.Party (Party (vkey))
import Hydra.Plutus.Extras (ValidatorType, wrapValidator)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.V1.Time (fromMilliSeconds)
import PlutusLedgerApi.V1.Value (lovelaceValue)
import PlutusLedgerApi.V3 (
  Address,
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
    (Initial{contestationPeriod, parties, headId}, CollectCom) ->
      checkCollectCom ctx (contestationPeriod, parties, headId)
    (Initial{parties, headId}, Abort) ->
      checkAbort ctx headId parties
    (Open openDatum, Increment redeemer) ->
      checkIncrement ctx openDatum redeemer
    (Open openDatum, Decrement redeemer) ->
      checkDecrement ctx openDatum redeemer
    (Open openDatum, Close redeemer) ->
      checkClose ctx openDatum redeemer
    (Closed closedDatum, Contest redeemer) ->
      checkContest ctx closedDatum redeemer
    (Closed closedDatum, Fanout{numberOfFanoutOutputs, numberOfCommitOutputs, numberOfDecommitOutputs}) ->
      checkFanout ctx closedDatum numberOfFanoutOutputs numberOfCommitOutputs numberOfDecommitOutputs
    _ ->
      traceError $(errorCode InvalidHeadStateTransition)

-- | On-Chain verification for 'Abort' transition. It verifies that:
--
--   * All PTs have been burnt: The right number of Head tokens with the correct
--     head id are burnt, one PT for each party and a state token ST.
--
--   * All committed funds have been redistributed. This is done via v_commit
--     and it only needs to ensure that we have spent all committed outputs,
--     which follows from burning all the PTs.
checkAbort ::
  ScriptContext ->
  CurrencySymbol ->
  [Party] ->
  Bool
checkAbort ctx@ScriptContext{scriptContextTxInfo = txInfo} headCurrencySymbol parties =
  mustBurnAllHeadTokens minted headCurrencySymbol parties
    && mustBeSignedByParticipant ctx headCurrencySymbol
    && mustReimburseCommittedUTxO
 where
  minted = txInfoMint txInfo

  mustReimburseCommittedUTxO =
    traceIfFalse $(errorCode ReimbursedOutputsDontMatch) $
      hashOfCommittedUTxO == hashOfOutputs

  hashOfOutputs =
    -- NOTE: It is enough to just _take_ the same number of outputs that
    -- correspond to the number of commit inputs to make sure everything is
    -- reimbursed because we assume the outputs are correctly sorted with
    -- reimbursed commits coming first
    hashTxOuts $ L.take (L.length committed) (txInfoOutputs txInfo)

  hashOfCommittedUTxO =
    hashPreSerializedCommits committed

  committed = committedUTxO [] (txInfoInputs txInfo)

  committedUTxO commits = \case
    [] -> commits
    TxInInfo{txInInfoResolved = txOut} : rest
      | hasPT headCurrencySymbol txOut ->
          committedUTxO (commitDatum txOut <> commits) rest
      | otherwise ->
          committedUTxO commits rest

-- | On-Chain verification for 'CollectCom' transition. It verifies that:
--
--   * All participants have committed (even empty commits)
--
--   * All commits are properly collected and locked into η as a hash
--     of serialized tx outputs in the same sequence as commit inputs!
--
--   * The transaction is performed (i.e. signed) by one of the head participants
--
--   * State token (ST) is present in the output
checkCollectCom ::
  -- | Script execution context
  ScriptContext ->
  (ContestationPeriod, [Party], CurrencySymbol) ->
  Bool
checkCollectCom ctx@ScriptContext{scriptContextTxInfo = txInfo} (contestationPeriod, parties, headId) =
  mustCollectUtxoHash
    && mustInitVersion
    && mustNotChangeParameters (parties', parties) (contestationPeriod', contestationPeriod) (headId', headId)
    && mustCollectAllValue
    -- XXX: Is this really needed? If yes, why not check on the output?
    && traceIfFalse $(errorCode STNotSpent) (hasST headId val)
    && everyoneHasCommitted
    && mustBeSignedByParticipant ctx headId
    && mustNotMintOrBurn txInfo
 where
  mustCollectUtxoHash =
    traceIfFalse $(errorCode IncorrectUtxoHash) $
      utxoHash == hashPreSerializedCommits collectedCommits

  mustInitVersion =
    traceIfFalse $(errorCode IncorrectVersion) $
      version' == 0

  mustCollectAllValue =
    traceIfFalse $(errorCode NotAllValueCollected) $
      -- NOTE: Instead of checking the head output val' against all collected
      -- value, we do ensure the output value is all non collected value - fees.
      -- This makes the script not scale badly with number of participants as it
      -- would commonly only be a small number of inputs/outputs to pay fees.
      otherValueOut == notCollectedValueIn - lovelaceValue (txInfoFee txInfo)

  OpenDatum
    { utxoHash
    , parties = parties'
    , contestationPeriod = contestationPeriod'
    , headId = headId'
    , version = version'
    } = decodeHeadOutputOpenDatum ctx

  headAddress = getHeadAddress ctx

  everyoneHasCommitted =
    traceIfFalse $(errorCode MissingCommits) $
      nTotalCommits == L.length parties

  val = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  otherValueOut =
    case txInfoOutputs txInfo of
      -- NOTE: First output must be head output
      (_ : rest) -> F.foldMap txOutValue rest
      _ -> mempty

  -- NOTE: We do keep track of the value we do not want to collect as this is
  -- typically less, ideally only a single other input with only ADA in it.
  (collectedCommits, nTotalCommits, notCollectedValueIn) =
    F.foldr
      extractAndCountCommits
      ([], 0, mempty)
      (txInfoInputs txInfo)

  extractAndCountCommits TxInInfo{txInInfoResolved} (commits, nCommits, notCollected)
    | isHeadOutput txInInfoResolved =
        (commits, nCommits, notCollected)
    | hasPT headId txInInfoResolved =
        (commitDatum txInInfoResolved <> commits, succ nCommits, notCollected)
    | otherwise =
        (commits, nCommits, notCollected <> txOutValue txInInfoResolved)

  isHeadOutput txOut = txOutAddress txOut == headAddress
{-# INLINEABLE checkCollectCom #-}

-- | Try to find the commit datum in the input and
-- if it is there return the committed utxo
commitDatum :: TxOut -> [Commit]
commitDatum input = do
  let datum = getTxOutDatum input
  case fromBuiltinData @Commit.DatumType $ getDatum datum of
    Just (_party, commits, _headId) ->
      commits
    Nothing -> []
{-# INLINEABLE commitDatum #-}

-- | Try to find the deposit datum in the input and
-- if it is there return the committed utxo
depositDatum :: TxOut -> [Commit]
depositDatum input = do
  let datum = getTxOutDatum input
  case fromBuiltinData @Deposit.DepositDatum $ getDatum datum of
    Just (_headId, _deadline, commits) ->
      commits
    Nothing -> []
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
    && mustIncreaseValue
    && mustBeSignedByParticipant ctx prevHeadId
    && checkSnapshotSignature
    && claimedDepositIsSpent
 where
  inputs = txInfoInputs txInfo

  depositInput =
    case findTxInByTxOutRef increment txInfo of
      Nothing -> traceError $(errorCode DepositInputNotFound)
      Just i -> i

  commits = depositDatum $ txInInfoResolved depositInput

  depositHash = hashPreSerializedCommits commits

  depositValue = txOutValue $ txInInfoResolved depositInput

  headInValue =
    case L.find (hasST prevHeadId) $ txOutValue . txInInfoResolved <$> inputs of
      Nothing -> traceError $(errorCode HeadInputNotFound)
      Just i -> i

  headOutValue = txOutValue $ L.head $ txInfoOutputs txInfo

  IncrementRedeemer{signature, snapshotNumber, increment} = redeemer

  claimedDepositIsSpent =
    traceIfFalse $(errorCode DepositNotSpent) $
      increment `L.elem` (txInInfoOutRef <$> txInfoInputs txInfo)

  checkSnapshotSignature =
    verifySnapshotSignature nextParties (nextHeadId, prevVersion, snapshotNumber, nextUtxoHash, depositHash, emptyHash) signature

  mustIncreaseVersion =
    traceIfFalse $(errorCode VersionNotIncremented) $
      nextVersion == prevVersion + 1

  mustIncreaseValue =
    traceIfFalse $(errorCode HeadValueIsNotPreserved) $
      headInValue <> depositValue == headOutValue

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
    && mustDecreaseValue
    && mustBeSignedByParticipant ctx prevHeadId
 where
  checkSnapshotSignature =
    verifySnapshotSignature nextParties (nextHeadId, prevVersion, snapshotNumber, nextUtxoHash, emptyHash, decommitUtxoHash) signature

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
    && mustPreserveValue
    && mustNotChangeParameters (parties', parties) (cperiod', cperiod) (headId', headId)
 where
  OpenDatum
    { parties
    , utxoHash = initialUtxoHash
    , contestationPeriod = cperiod
    , headId
    , version
    } = openBefore

  mustPreserveValue =
    traceIfFalse $(errorCode HeadValueIsNotPreserved) $
      val == val'

  val' = txOutValue . L.head $ txInfoOutputs txInfo

  val = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

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
              (headId, version, snapshotNumber', utxoHash', emptyHash, emptyHash)
              signature
      CloseUnusedDec{signature} ->
        traceIfFalse $(errorCode FailedCloseUnusedDec) $
          alphaUTxOHash' == emptyHash
            && omegaUTxOHash' /= emptyHash
            && verifySnapshotSignature
              parties
              (headId, version, snapshotNumber', utxoHash', emptyHash, omegaUTxOHash')
              signature
      CloseUsedDec{signature, alreadyDecommittedUTxOHash} ->
        traceIfFalse $(errorCode FailedCloseUsedDec) $
          alphaUTxOHash' == emptyHash
            && omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version - 1, snapshotNumber', utxoHash', emptyHash, alreadyDecommittedUTxOHash)
              signature
      CloseUnusedInc{signature, alreadyCommittedUTxOHash} ->
        traceIfFalse $(errorCode FailedCloseUnusedInc) $
          alphaUTxOHash' == emptyHash
            && omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version, snapshotNumber', utxoHash', alreadyCommittedUTxOHash, emptyHash)
              signature
      CloseUsedInc{signature, alreadyCommittedUTxOHash} ->
        traceIfFalse $(errorCode FailedCloseUsedInc) $
          alphaUTxOHash' == alreadyCommittedUTxOHash
            && omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version - 1, snapshotNumber', utxoHash', alreadyCommittedUTxOHash, emptyHash)
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
    && mustPreserveValue
 where
  mustPreserveValue =
    traceIfFalse $(errorCode HeadValueIsNotPreserved) $
      val == val'

  val' = txOutValue . L.head $ txInfoOutputs txInfo

  val = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  mustBeNewer =
    traceIfFalse $(errorCode TooOldSnapshot) $
      snapshotNumber' > snapshotNumber

  mustNotChangeVersion =
    traceIfFalse $(errorCode MustNotChangeVersion) $
      version' == version

  mustBeValidSnapshot =
    case redeemer of
      ContestCurrent{signature} ->
        traceIfFalse $(errorCode FailedContestCurrent) $
          alphaUTxOHash' == emptyHash
            && omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version, snapshotNumber', utxoHash', emptyHash, emptyHash)
              signature
      ContestUsedDec{signature, alreadyDecommittedUTxOHash} ->
        traceIfFalse $(errorCode FailedContestUsedDec) $
          alphaUTxOHash' == emptyHash
            && omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version - 1, snapshotNumber', utxoHash', emptyHash, alreadyDecommittedUTxOHash)
              signature
      ContestUnusedDec{signature} ->
        traceIfFalse $(errorCode FailedContestUnusedDec) $
          alphaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version, snapshotNumber', utxoHash', emptyHash, omegaUTxOHash')
              signature
      ContestUnusedInc{signature, alreadyCommittedUTxOHash} ->
        traceIfFalse $(errorCode FailedContestUnusedInc) $
          omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version - 1, snapshotNumber', utxoHash', alreadyCommittedUTxOHash, emptyHash)
              signature
      ContestUsedInc{signature} ->
        traceIfFalse $(errorCode FailedContestUsedInc) $
          omegaUTxOHash' == emptyHash
            && verifySnapshotSignature
              parties
              (headId, version, snapshotNumber', utxoHash', alphaUTxOHash', emptyHash)
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
    } = decodeHeadOutputClosedDatum ctx

  ScriptContext{scriptContextTxInfo = txInfo} = ctx

  contester =
    case txInfoSignatories txInfo of
      [signer] -> signer
      _ -> traceError $(errorCode WrongNumberOfSigners)

  checkSignedParticipantContestOnlyOnce =
    traceIfFalse $(errorCode SignerAlreadyContested) $
      contester `L.notElem` contesters
{-# INLINEABLE checkContest #-}

-- | Verify a fanout transaction.
checkFanout ::
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
checkFanout ScriptContext{scriptContextTxInfo = txInfo} closedDatum numberOfFanoutOutputs numberOfCommitOutputs numberOfDecommitOutputs =
  mustBurnAllHeadTokens minted headId parties
    && hasSameUTxOHash
    && hasSameCommitUTxOHash
    && hasSameDecommitUTxOHash
    && afterContestationDeadline
 where
  minted = txInfoMint txInfo

  hasSameUTxOHash =
    traceIfFalse $(errorCode FanoutUTxOHashMismatch) $
      fannedOutUtxoHash == utxoHash

  hasSameCommitUTxOHash =
    traceIfFalse $(errorCode FanoutUTxOToCommitHashMismatch) $
      alphaUTxOHash == commitUtxoHash

  hasSameDecommitUTxOHash =
    traceIfFalse $(errorCode FanoutUTxOToDecommitHashMismatch) $
      omegaUTxOHash == decommitUtxoHash

  fannedOutUtxoHash = hashTxOuts $ L.take numberOfFanoutOutputs txInfoOutputs

  commitUtxoHash = hashTxOuts $ L.take numberOfCommitOutputs $ L.drop numberOfFanoutOutputs txInfoOutputs

  decommitUtxoHash = hashTxOuts $ L.take numberOfDecommitOutputs $ L.drop numberOfFanoutOutputs txInfoOutputs

  ClosedDatum{utxoHash, alphaUTxOHash, omegaUTxOHash, parties, headId, contestationDeadline} = closedDatum

  TxInfo{txInfoOutputs} = txInfo

  afterContestationDeadline =
    case ivFrom (txInfoValidRange txInfo) of
      LowerBound (Finite time) _ ->
        traceIfFalse $(errorCode LowerBoundBeforeContestationDeadline) $
          time > contestationDeadline
      _ -> traceError $(errorCode FanoutNoLowerBoundDefined)
{-# INLINEABLE checkFanout #-}

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

-- | Check if 'TxOut' contains the PT token.
hasPT :: CurrencySymbol -> TxOut -> Bool
hasPT headCurrencySymbol txOut =
  let pts = findParticipationTokens headCurrencySymbol (txOutValue txOut)
   in L.length pts == 1
{-# INLINEABLE hasPT #-}

-- | Verify the multi-signature of a snapshot using given constituents 'headId',
-- 'version', 'number', 'utxoHash' and 'utxoToDecommitHash'. See
-- 'SignableRepresentation Snapshot' for more details.
verifySnapshotSignature :: [Party] -> (CurrencySymbol, SnapshotVersion, SnapshotNumber, Hash, Hash, Hash) -> [Signature] -> Bool
verifySnapshotSignature parties msg sigs =
  traceIfFalse $(errorCode SignatureVerificationFailed) $
    L.length parties == L.length sigs
      && L.all (uncurry $ verifyPartySignature msg) (L.zip parties sigs)
{-# INLINEABLE verifySnapshotSignature #-}

-- | Verify individual party signature of a snapshot. See
-- 'SignableRepresentation Snapshot' for more details.
verifyPartySignature :: (CurrencySymbol, SnapshotVersion, SnapshotNumber, Hash, Hash, Hash) -> Party -> Signature -> Bool
verifyPartySignature (headId, snapshotVersion, snapshotNumber, utxoHash, utxoToCommitHash, utxoToDecommitHash) party =
  verifyEd25519Signature (vkey party) message
 where
  message =
    Builtins.serialiseData (toBuiltinData headId)
      <> Builtins.serialiseData (toBuiltinData snapshotVersion)
      <> Builtins.serialiseData (toBuiltinData snapshotNumber)
      <> Builtins.serialiseData (toBuiltinData utxoHash)
      <> Builtins.serialiseData (toBuiltinData utxoToCommitHash)
      <> Builtins.serialiseData (toBuiltinData utxoToDecommitHash)
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

decodeHeadOutputOpenDatum :: ScriptContext -> OpenDatum
decodeHeadOutputOpenDatum ctx =
  -- XXX: fromBuiltinData is super big (and also expensive?)
  case fromBuiltinData @DatumType $ getDatum (headOutputDatum ctx) of
    Just (Open openDatum) -> openDatum
    _ -> traceError $(errorCode WrongStateInOutputDatum)
{-# INLINEABLE decodeHeadOutputOpenDatum #-}

emptyHash :: Hash
emptyHash = hashTxOuts []
{-# INLINEABLE emptyHash #-}
