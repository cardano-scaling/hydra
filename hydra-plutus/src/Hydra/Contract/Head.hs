{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.Head where

import PlutusTx.Prelude

import Hydra.Contract.Commit (Commit (..))
import qualified Hydra.Contract.Commit as Commit
import Hydra.Contract.HeadError (HeadError (..), errorCode)
import Hydra.Contract.HeadState (Input (..), Signature, SnapshotNumber, State (..))
import Hydra.Contract.Util (hasST, mustNotMintOrBurn, (===))
import Hydra.Data.ContestationPeriod (ContestationPeriod, addContestationPeriod, milliseconds)
import Hydra.Data.Party (Party (vkey))
import Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import Plutus.V1.Ledger.Time (fromMilliSeconds)
import Plutus.V1.Ledger.Value (valueOf)
import Plutus.V2.Ledger.Api (
  Address,
  CurrencySymbol,
  Datum (..),
  Extended (Finite),
  FromData (fromBuiltinData),
  Interval (..),
  LowerBound (LowerBound),
  OutputDatum (..),
  POSIXTime,
  PubKeyHash (getPubKeyHash),
  Script,
  ScriptContext (..),
  ToData (toBuiltinData),
  TokenName (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
  UpperBound (..),
  Validator (getValidator),
  ValidatorHash,
  Value (Value, getValue),
  adaSymbol,
  adaToken,
  mkValidatorScript,
 )
import Plutus.V2.Ledger.Contexts (findDatum, findOwnInput)
import PlutusTx (CompiledCode)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map
import qualified PlutusTx.Builtins as Builtins

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
    (Open{parties, utxoHash = initialUtxoHash, contestationPeriod, headId}, Close{signature}) ->
      checkClose ctx parties initialUtxoHash signature contestationPeriod headId
    (Closed{parties, snapshotNumber = closedSnapshotNumber, contestationDeadline, contestationPeriod, headId, contesters}, Contest{signature}) ->
      checkContest ctx contestationDeadline contestationPeriod parties closedSnapshotNumber signature contesters headId
    (Closed{utxoHash, contestationDeadline}, Fanout{numberOfFanoutOutputs}) ->
      checkFanout utxoHash contestationDeadline numberOfFanoutOutputs ctx
    _ ->
      traceError $(errorCode InvalidHeadStateTransition)

-- | On-Chain verification for 'Abort' transition. It verifies that:
--
--   * All PTs have been burnt: The right number of Head tokens with the correct
--     head id are burnt, one PT for each party and a state token ST.
--
--   * All committed funds have been redistributed. This is done via v_commit
--     and it only needs to ensure that we have spent all comitted outputs,
--     which follows from burning all the PTs.
checkAbort ::
  ScriptContext ->
  CurrencySymbol ->
  [Party] ->
  Bool
checkAbort ctx@ScriptContext{scriptContextTxInfo = txInfo} headCurrencySymbol parties =
  mustBurnAllHeadTokens
    && mustBeSignedByParticipant ctx headCurrencySymbol
    && mustReimburseCommittedUTxO
 where
  mustBurnAllHeadTokens =
    traceIfFalse $(errorCode BurntTokenNumberMismatch) $
      burntTokens == length parties + 1

  minted = getValue $ txInfoMint txInfo

  burntTokens =
    case Map.lookup headCurrencySymbol minted of
      Nothing -> 0
      Just tokenMap -> negate $ sum tokenMap

  mustReimburseCommittedUTxO =
    traceIfFalse $(errorCode ReimbursedOutputsDontMatch) $
      hashOfCommittedUTxO == hashOfOutputs

  hashOfOutputs =
    -- NOTE: It is enough to just _take_ the same number of outputs that
    -- correspond to the number of commit inputs to make sure everything is
    -- reimbursed because we assume the outputs are correctly sorted with
    -- reimbursed commits coming first
    hashTxOuts $ take (length commited) (txInfoOutputs txInfo)

  hashOfCommittedUTxO =
    hashPreSerializedCommits commited

  commited = committedUTxO [] (txInfoInputs txInfo)

  committedUTxO commits = \case
    [] ->
      commits
    TxInInfo{txInInfoResolved = txOut} : rest
      | hasPT headCurrencySymbol txOut ->
        case commitDatum txInfo txOut of
          Just commit ->
            committedUTxO
              (commit : commits)
              rest
          Nothing ->
            committedUTxO
              commits
              rest
      | otherwise ->
        committedUTxO
          commits
          rest

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
  mustNotMintOrBurn txInfo
    && mustCollectUtxoHash
    && mustNotChangeParameters
    && everyoneHasCommitted
    && mustBeSignedByParticipant ctx headId
    -- FIXME: does not check all value collected
    && traceIfFalse $(errorCode STNotSpent) (hasST headId val)
 where
  mustCollectUtxoHash =
    traceIfFalse $(errorCode IncorrectUtxoHash) $
      utxoHash == hashPreSerializedCommits collectedCommits

  mustNotChangeParameters =
    traceIfFalse $(errorCode ChangedParameters) $
      parties' == parties
        && contestationPeriod' == contestationPeriod
        && headId' == headId

  (parties', utxoHash, contestationPeriod', headId') =
    -- XXX: fromBuiltinData is super big (and also expensive?)
    case fromBuiltinData @DatumType $ getDatum (headOutputDatum ctx) of
      Just
        Open
          { parties = p
          , utxoHash = h
          , contestationPeriod = cp
          , headId = hId
          } ->
          (p, h, cp, hId)
      _ -> traceError $(errorCode WrongStateInOutputDatum)

  headAddress = getHeadAddress ctx

  val =
    maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  everyoneHasCommitted =
    traceIfFalse $(errorCode MissingCommits) $
      nTotalCommits == length parties

  (collectedCommits, nTotalCommits) =
    foldr
      extractAndCountCommits
      ([], 0)
      (txInfoInputs txInfo)

  extractAndCountCommits TxInInfo{txInInfoResolved} (commits, nCommits)
    | isHeadOutput txInInfoResolved =
      (commits, nCommits)
    | hasPT headId txInInfoResolved =
      case commitDatum txInfo txInInfoResolved of
        Just commit@Commit{} ->
          (commit : commits, succ nCommits)
        Nothing ->
          (commits, succ nCommits)
    | otherwise =
      (commits, nCommits)

  isHeadOutput txOut = txOutAddress txOut == headAddress
{-# INLINEABLE checkCollectCom #-}

-- | Try to find the commit datum in the input and
-- if it is there return the commited utxo
commitDatum :: TxInfo -> TxOut -> Maybe Commit
commitDatum txInfo input = do
  let datum = findTxOutDatum txInfo input
  case fromBuiltinData @Commit.DatumType $ getDatum datum of
    Just (_party, commit, _headId) ->
      commit
    Nothing -> Nothing
{-# INLINEABLE commitDatum #-}

-- | The close validator must verify that:
--
--   * Check that the closing tx validity is bounded by contestation period
--
--   * Check that the deadline corresponds with tx validity and contestation period.
--
--   * The resulting utxo hash is correctly signed or the initial utxo hash,
--     depending on snapshot number
--
--   * The transaction is performed (i.e. signed) by one of the head participants
--
--   * State token (ST) is present in the output
--
--   * Contesters must be initialize as empty
--
--   * Value in v_head is preserved
checkClose ::
  ScriptContext ->
  [Party] ->
  BuiltinByteString ->
  [Signature] ->
  ContestationPeriod ->
  CurrencySymbol ->
  Bool
checkClose ctx parties initialUtxoHash sig cperiod headPolicyId =
  mustNotMintOrBurn txInfo
    && hasBoundedValidity
    && checkDeadline
    && checkSnapshot
    && mustBeSignedByParticipant ctx headPolicyId
    && mustInitializeContesters
    -- XXX: missing to trace for this error code
    && hasST headPolicyId val
    && mustPreserveValue
    && mustNotChangeParameters
 where
  mustPreserveValue =
    traceIfFalse $(errorCode HeadValueIsNotPreserved) $
      val === val'

  val' = txOutValue . head $ txInfoOutputs txInfo

  val = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  hasBoundedValidity =
    traceIfFalse $(errorCode HasBoundedValidityCheckFailed) $
      tMax - tMin <= cp

  (closedSnapshotNumber, closedUtxoHash, parties', closedContestationDeadline, headId', contesters') =
    -- XXX: fromBuiltinData is super big (and also expensive?)
    case fromBuiltinData @DatumType $ getDatum (headOutputDatum ctx) of
      Just
        Closed
          { snapshotNumber
          , utxoHash
          , parties = p
          , contestationDeadline
          , headId
          , contesters
          } -> (snapshotNumber, utxoHash, p, contestationDeadline, headId, contesters)
      _ -> traceError $(errorCode WrongStateInOutputDatum)

  checkSnapshot
    | closedSnapshotNumber > 0 =
      traceIfFalse $(errorCode InvalidSnapshotSignature) $
        verifySnapshotSignature parties closedSnapshotNumber closedUtxoHash sig
    | otherwise =
      traceIfFalse $(errorCode ClosedWithNonInitialHash) $
        closedUtxoHash == initialUtxoHash

  checkDeadline =
    traceIfFalse $(errorCode IncorrectClosedContestationDeadline) $
      closedContestationDeadline == makeContestationDeadline cperiod ctx

  cp = fromMilliSeconds (milliseconds cperiod)

  tMax = case ivTo $ txInfoValidRange txInfo of
    UpperBound (Finite t) _ -> t
    _InfiniteBound -> traceError $(errorCode InfiniteUpperBound)

  tMin = case ivFrom $ txInfoValidRange txInfo of
    LowerBound (Finite t) _ -> t
    _InfiniteBound -> traceError $(errorCode InfiniteLowerBound)

  mustNotChangeParameters =
    traceIfFalse $(errorCode ChangedParameters) $
      headId' == headPolicyId
        && parties' == parties

  mustInitializeContesters =
    traceIfFalse $(errorCode ContestersNonEmpty) $
      null contesters'

  ScriptContext{scriptContextTxInfo = txInfo} = ctx
{-# INLINEABLE checkClose #-}

-- | The contest validator must verify that:
--
--   * The transaction does not mint or burn tokens.
--
--   * The contest snapshot number is strictly greater than the closed snapshot number.
--
--   * The contest snapshot is correctly signed.
--
--   * The transaction is performed (i.e. signed) by one of the head participants
--
--   * Party can contest only once.
--
--   * The transaction is performed before the deadline.
--
--   * Add signer to list of contesters.
--
--   * State token (ST) is present in the output
--
--   * Push deadline if signer is not the last one to contest.
--
--   * No other parameters have changed.
--
--   * Value in v_head is preserved
checkContest ::
  ScriptContext ->
  POSIXTime ->
  ContestationPeriod ->
  [Party] ->
  -- | Snapshot number of the closed state.
  SnapshotNumber ->
  [Signature] ->
  -- | Keys of party member which already contested.
  [PubKeyHash] ->
  -- | Head id
  CurrencySymbol ->
  Bool
checkContest ctx contestationDeadline contestationPeriod parties closedSnapshotNumber sig contesters headId =
  mustNotMintOrBurn txInfo
    && mustBeNewer
    && mustBeMultiSigned
    && mustBeSignedByParticipant ctx headId
    && checkSignedParticipantContestOnlyOnce
    && mustBeWithinContestationPeriod
    && mustUpdateContesters
    -- XXX: This check is redundant and can be removed,
    -- because is enough to check that the value is preserved.
    -- Remember we are comming from a valid Closed state,
    -- having already checked that the ST is present.
    && hasST headId val
    && mustPushDeadline
    && mustNotChangeParameters
    && mustPreserveValue
 where
  mustPreserveValue =
    traceIfFalse $(errorCode HeadValueIsNotPreserved) $
      val === val'

  val' = txOutValue . head $ txInfoOutputs txInfo

  val = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  mustBeNewer =
    traceIfFalse $(errorCode TooOldSnapshot) $
      contestSnapshotNumber > closedSnapshotNumber

  mustBeMultiSigned =
    verifySnapshotSignature parties contestSnapshotNumber contestUtxoHash sig

  mustBeWithinContestationPeriod =
    case ivTo (txInfoValidRange txInfo) of
      UpperBound (Finite time) _ ->
        traceIfFalse $(errorCode UpperBoundBeyondContestationDeadline) $
          time <= contestationDeadline
      _ -> traceError $(errorCode ContestNoUpperBoundDefined)

  mustNotChangeParameters =
    traceIfFalse $(errorCode ChangedParameters) $
      parties' == parties
        && headId' == headId
        && contestationPeriod' == contestationPeriod

  mustPushDeadline =
    if length contesters' == length parties'
      then
        traceIfFalse $(errorCode MustNotPushDeadline) $
          contestationDeadline' == contestationDeadline
      else
        traceIfFalse $(errorCode MustPushDeadline) $
          contestationDeadline' == addContestationPeriod contestationDeadline contestationPeriod

  mustUpdateContesters =
    traceIfFalse $(errorCode ContesterNotIncluded) $
      contesters' == contester : contesters

  (contestSnapshotNumber, contestUtxoHash, parties', contestationDeadline', contestationPeriod', headId', contesters') =
    -- XXX: fromBuiltinData is super big (and also expensive?)
    case fromBuiltinData @DatumType $ getDatum (headOutputDatum ctx) of
      Just
        Closed
          { snapshotNumber
          , utxoHash
          , parties = p
          , contestationDeadline = dl
          , contestationPeriod = cp
          , headId = hid
          , contesters = cs
          } -> (snapshotNumber, utxoHash, p, dl, cp, hid, cs)
      _ -> traceError $(errorCode WrongStateInOutputDatum)

  ScriptContext{scriptContextTxInfo = txInfo} = ctx

  contester =
    case txInfoSignatories txInfo of
      [signer] -> signer
      _ -> traceError $(errorCode WrongNumberOfSigners)

  checkSignedParticipantContestOnlyOnce =
    traceIfFalse $(errorCode SignerAlreadyContested) $
      contester `notElem` contesters
{-# INLINEABLE checkContest #-}

checkFanout ::
  BuiltinByteString ->
  POSIXTime ->
  Integer ->
  ScriptContext ->
  Bool
checkFanout utxoHash contestationDeadline numberOfFanoutOutputs ScriptContext{scriptContextTxInfo = txInfo} =
  hasSameUTxOHash && afterContestationDeadline
 where
  hasSameUTxOHash =
    traceIfFalse $(errorCode FannedOutUtxoHashNotEqualToClosedUtxoHash) $
      fannedOutUtxoHash == utxoHash

  fannedOutUtxoHash = hashTxOuts $ take numberOfFanoutOutputs txInfoOutputs

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

(&) :: a -> (a -> b) -> b
(&) = flip ($)
{-# INLINEABLE (&) #-}

txOutAdaValue :: TxOut -> Integer
txOutAdaValue o = valueOf (txOutValue o) adaSymbol adaToken
{-# INLINEABLE txOutAdaValue #-}

txInfoAdaFee :: TxInfo -> Integer
txInfoAdaFee tx = valueOf (txInfoFee tx) adaSymbol adaToken
{-# INLINEABLE txInfoAdaFee #-}

makeContestationDeadline :: ContestationPeriod -> ScriptContext -> POSIXTime
makeContestationDeadline cperiod ScriptContext{scriptContextTxInfo} =
  case ivTo (txInfoValidRange scriptContextTxInfo) of
    UpperBound (Finite time) _ -> addContestationPeriod time cperiod
    _ -> traceError $(errorCode CloseNoUpperBoundDefined)
{-# INLINEABLE makeContestationDeadline #-}

getHeadAddress :: ScriptContext -> Address
getHeadAddress ctx =
  let headInput =
        fromMaybe
          (traceError $(errorCode ScriptNotSpendingAHeadInput))
          (findOwnInput ctx)
   in txOutAddress (txInInfoResolved headInput)
{-# INLINEABLE getHeadAddress #-}

-- XXX: We might not need to distinguish between the three cases here.
mustBeSignedByParticipant ::
  ScriptContext ->
  CurrencySymbol ->
  Bool
mustBeSignedByParticipant ScriptContext{scriptContextTxInfo = txInfo} headCurrencySymbol =
  case getPubKeyHash <$> txInfoSignatories txInfo of
    [signer] ->
      traceIfFalse $(errorCode SignerIsNotAParticipant) $
        signer `elem` (unTokenName <$> participationTokens)
    [] ->
      traceError $(errorCode NoSigners)
    _ ->
      traceError $(errorCode TooManySigners)
 where
  participationTokens = loop (txInfoInputs txInfo)
  loop = \case
    [] -> []
    (TxInInfo{txInInfoResolved} : rest) ->
      findParticipationTokens headCurrencySymbol (txOutValue txInInfoResolved) ++ loop rest
{-# INLINEABLE mustBeSignedByParticipant #-}

findParticipationTokens :: CurrencySymbol -> Value -> [TokenName]
findParticipationTokens headCurrency (Value val) =
  case Map.toList <$> Map.lookup headCurrency val of
    Just tokens ->
      mapMaybe (\(tokenName, n) -> if n == 1 then Just tokenName else Nothing) tokens
    _ ->
      []
{-# INLINEABLE findParticipationTokens #-}

headOutputDatum :: ScriptContext -> Datum
headOutputDatum ctx =
  case txInfoOutputs txInfo of
    (o : _)
      | txOutAddress o == headAddress -> findTxOutDatum txInfo o
    _ -> traceError $(errorCode NotPayingToHead)
 where
  headAddress = getHeadAddress ctx

  ScriptContext{scriptContextTxInfo = txInfo} = ctx
{-# INLINEABLE headOutputDatum #-}

findTxOutDatum :: TxInfo -> TxOut -> Datum
findTxOutDatum txInfo o =
  case txOutDatum o of
    NoOutputDatum -> traceError $(errorCode NoOutputDatumError)
    OutputDatumHash dh ->
      fromMaybe (traceError $(errorCode DatumNotFound)) $
        findDatum dh txInfo
    OutputDatum d -> d
{-# INLINEABLE findTxOutDatum #-}

-- | Hash a potentially unordered list of commits by sorting them, concatenating
-- their 'preSerializedOutput' bytes and creating a SHA2_256 digest over that.
hashPreSerializedCommits :: [Commit] -> BuiltinByteString
hashPreSerializedCommits commits =
  sha2_256 . foldMap preSerializedOutput $
    sortBy (\a b -> compareRef (input a) (input b)) commits
{-# INLINEABLE hashPreSerializedCommits #-}

-- | Hash a pre-ordered list of transaction outputs by serializing each
-- individual 'TxOut', concatenating all bytes together and creating a SHA2_256
-- digest over that.
hashTxOuts :: [TxOut] -> BuiltinByteString
hashTxOuts =
  sha2_256 . foldMap (Builtins.serialiseData . toBuiltinData)
{-# INLINEABLE hashTxOuts #-}

-- | Check if 'TxOut' contains the PT token.
hasPT :: CurrencySymbol -> TxOut -> Bool
hasPT headCurrencySymbol txOut =
  let pts = findParticipationTokens headCurrencySymbol (txOutValue txOut)
   in length pts == 1
{-# INLINEABLE hasPT #-}

verifySnapshotSignature :: [Party] -> SnapshotNumber -> BuiltinByteString -> [Signature] -> Bool
verifySnapshotSignature parties snapshotNumber utxoHash sigs =
  traceIfFalse $(errorCode SignatureVerificationFailed) $
    length parties == length sigs
      && all (uncurry $ verifyPartySignature snapshotNumber utxoHash) (zip parties sigs)
{-# INLINEABLE verifySnapshotSignature #-}

verifyPartySignature :: SnapshotNumber -> BuiltinByteString -> Party -> Signature -> Bool
verifyPartySignature snapshotNumber utxoHash party signed =
  traceIfFalse $(errorCode PartySignatureVerificationFailed) $
    verifyEd25519Signature (vkey party) message signed
 where
  message =
    -- TODO: document CDDL format, either here or in 'Hydra.Snapshot.getSignableRepresentation'
    Builtins.serialiseData (toBuiltinData snapshotNumber)
      <> Builtins.serialiseData (toBuiltinData utxoHash)
{-# INLINEABLE verifyPartySignature #-}

compareRef :: TxOutRef -> TxOutRef -> Ordering
TxOutRef{txOutRefId, txOutRefIdx} `compareRef` TxOutRef{txOutRefId = id', txOutRefIdx = idx'} =
  case compare txOutRefId id' of
    EQ -> compare txOutRefIdx idx'
    ord -> ord
{-# INLINEABLE compareRef #-}

compiledValidator :: CompiledCode ValidatorType
compiledValidator =
  $$(PlutusTx.compile [||wrap headValidator||])
 where
  wrap = wrapValidator @DatumType @RedeemerType

validatorScript :: Script
validatorScript = getValidator $ mkValidatorScript compiledValidator

validatorHash :: ValidatorHash
validatorHash = scriptValidatorHash validatorScript
