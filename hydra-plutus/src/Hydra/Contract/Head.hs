{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
-- Avoid trace calls to be optimized away when inlining functions.
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-simplifier-inline #-}
-- Plutus core version to compile to. In babbage era, that is Cardano protocol
-- version 7 and 8, only plutus-core version 1.0.0 is available.
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Hydra.Contract.Head where

import PlutusTx.Prelude

import Hydra.Cardano.Api (PlutusScriptVersion (PlutusScriptV2))
import Hydra.Contract.Commit (Commit (..))
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.HeadError (HeadError (..), errorCode)
import Hydra.Contract.HeadState (Hash, Input (..), Signature, SnapshotNumber, State (..))
import Hydra.Contract.Util (hasST, mustBurnAllHeadTokens, mustNotMintOrBurn, (===))
import Hydra.Data.ContestationPeriod (ContestationPeriod, addContestationPeriod, milliseconds)
import Hydra.Data.Party (Party (vkey))
import Hydra.Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1.Time (fromMilliSeconds)
import PlutusLedgerApi.V1.Value (valueOf)
import PlutusLedgerApi.V2 (
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
  ScriptContext (..),
  ScriptHash,
  ToData (toBuiltinData),
  TokenName (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
  UpperBound (..),
  Value (Value),
  adaSymbol,
  adaToken,
 )
import PlutusLedgerApi.V2.Contexts (findOwnInput)
import PlutusTx (CompiledCode)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins

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
    (Open{parties, contestationPeriod, snapshotNumber, headId, version}, Decrement{signature, numberOfDecommitOutputs}) ->
      checkDecrement ctx parties snapshotNumber contestationPeriod headId version signature numberOfDecommitOutputs
    (Open{parties, utxoHash = initialUtxoHash, contestationPeriod, headId, snapshotNumber, version}, Close{signature}) ->
      checkClose ctx parties initialUtxoHash signature contestationPeriod headId snapshotNumber version
    (Closed{parties, snapshotNumber = closedSnapshotNumber, contestationDeadline, contestationPeriod, headId, contesters, version}, Contest{signature}) ->
      checkContest ctx contestationDeadline contestationPeriod parties closedSnapshotNumber signature contesters headId version
    (Closed{parties, utxoHash, utxoToDecommitHash, contestationDeadline, headId}, Fanout{numberOfFanoutOutputs, numberOfDecommitOutputs}) ->
      checkFanout utxoHash utxoToDecommitHash contestationDeadline numberOfFanoutOutputs numberOfDecommitOutputs ctx headId parties
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
    hashTxOuts $ take (length committed) (txInfoOutputs txInfo)

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

  mustCollectAllValue =
    traceIfFalse $(errorCode NotAllValueCollected) $
      -- NOTE: Instead of checking the head output val' against all collected
      -- value, we do ensure the output value is all non collected value - fees.
      -- This makes the script not scale badly with number of participants as it
      -- would commonly only be a small number of inputs/outputs to pay fees.
      otherValueOut == notCollectedValueIn - txInfoFee txInfo

  (utxoHash, parties', _, contestationPeriod', headId') =
    extractOpenDatum ctx

  headAddress = getHeadAddress ctx

  everyoneHasCommitted =
    traceIfFalse $(errorCode MissingCommits) $
      nTotalCommits == length parties

  val = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  otherValueOut =
    case txInfoOutputs txInfo of
      -- NOTE: First output must be head output
      (_ : rest) -> foldMap txOutValue rest
      _ -> mempty

  -- NOTE: We do keep track of the value we do not want to collect as this is
  -- typically less, ideally only a single other input with only ADA in it.
  (collectedCommits, nTotalCommits, notCollectedValueIn) =
    foldr
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
  let !datum = getTxOutDatum input
  case fromBuiltinData @Commit.DatumType $ getDatum datum of
    Just (_party, commits, _headId) ->
      commits
    Nothing -> []
{-# INLINEABLE commitDatum #-}

checkDecrement ::
  ScriptContext ->
  [Party] ->
  SnapshotNumber ->
  ContestationPeriod ->
  CurrencySymbol ->
  Integer ->
  [Signature] ->
  Integer ->
  Bool
checkDecrement ctx@ScriptContext{scriptContextTxInfo = txInfo} prevParties prevSnapshotNumber prevCperiod prevHeadId version signature numberOfDecommitOutputs =
  mustNotChangeParameters (prevParties, nextParties) (prevCperiod, nextCperiod) (prevHeadId, nextHeadId)
    && checkSnapshot
    && checkSnapshotSignature
    && mustBeSignedByParticipant ctx prevHeadId
    && mustDecreaseValue
 where
  checkSnapshot =
    traceIfFalse $(errorCode SnapshotNumberMismatch) $
      nextSnapshotNumber > prevSnapshotNumber

  checkSnapshotSignature =
    verifySnapshotSignature nextParties nextHeadId nextSnapshotNumber nextUtxoHash decommitUtxoHash version signature

  mustDecreaseValue =
    traceIfFalse $(errorCode HeadValueIsNotPreserved) $
      headInValue == headOutValue <> foldMap txOutValue decommitOutputs

  -- NOTE: we always assume Head output is the first one so we pick all other
  -- outputs of a decommit tx to calculate the expected hash.
  decommitUtxoHash = hashTxOuts decommitOutputs
  (nextUtxoHash, nextParties, nextSnapshotNumber, nextCperiod, nextHeadId) =
    extractOpenDatum ctx

  -- NOTE: head output + whatever is decommitted needs to be equal to the head input.
  -- headOutValue = foldMap txOutValue outputs

  headOutValue = txOutValue $ head outputs
  headInValue = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  decommitOutputs = take numberOfDecommitOutputs (tail outputs)

  outputs = txInfoOutputs txInfo
{-# INLINEABLE checkDecrement #-}

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
  SnapshotNumber ->
  Integer ->
  Bool
checkClose ctx parties initialUtxoHash sig cperiod headPolicyId snapshotNumber version =
  mustNotMintOrBurn txInfo
    && hasBoundedValidity
    && checkDeadline
    && checkSnapshot
    && mustBeSignedByParticipant ctx headPolicyId
    && mustInitializeContesters
    && mustPreserveValue
    && mustNotChangeParameters (parties', parties) (cperiod', cperiod) (headId', headPolicyId)
    && checkSnapshotNumber
 where
  checkSnapshotNumber =
    traceIfFalse $(errorCode TooOldSnapshot) $
      closedSnapshotNumber >= snapshotNumber

  mustPreserveValue =
    traceIfFalse $(errorCode HeadValueIsNotPreserved) $
      val === val'

  val' = txOutValue . head $ txInfoOutputs txInfo

  val = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  hasBoundedValidity =
    traceIfFalse $(errorCode HasBoundedValidityCheckFailed) $
      tMax - tMin <= cp

  (closedSnapshotNumber, closedUtxoHash, decommitHash, parties', closedContestationDeadline, cperiod', headId', contesters') =
    extractClosedDatum ctx

  checkSnapshot
    | closedSnapshotNumber > 0 =
        verifySnapshotSignature parties headPolicyId closedSnapshotNumber closedUtxoHash decommitHash version sig
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
  Integer ->
  Bool
checkContest ctx contestationDeadline contestationPeriod parties closedSnapshotNumber sig contesters headId version =
  mustNotMintOrBurn txInfo
    && mustBeNewer
    && mustBeMultiSigned
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
      val === val'

  val' = txOutValue . head $ txInfoOutputs txInfo

  val = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  mustBeNewer =
    traceIfFalse $(errorCode TooOldSnapshot) $
      contestSnapshotNumber > closedSnapshotNumber

  mustBeMultiSigned =
    verifySnapshotSignature parties headId contestSnapshotNumber contestUtxoHash decommitHash version sig

  mustBeWithinContestationPeriod =
    case ivTo (txInfoValidRange txInfo) of
      UpperBound (Finite time) _ ->
        traceIfFalse $(errorCode UpperBoundBeyondContestationDeadline) $
          time <= contestationDeadline
      _ -> traceError $(errorCode ContestNoUpperBoundDefined)

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

  (contestSnapshotNumber, contestUtxoHash, decommitHash, parties', contestationDeadline', contestationPeriod', headId', contesters') =
    extractClosedDatum ctx

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
  BuiltinByteString ->
  POSIXTime ->
  Integer ->
  Integer ->
  ScriptContext ->
  CurrencySymbol ->
  [Party] ->
  Bool
checkFanout utxoHash utxoToDecommitHash contestationDeadline numberOfFanoutOutputs numberOfDecommitOutputs ScriptContext{scriptContextTxInfo = txInfo} currencySymbol parties =
  mustBurnAllHeadTokens minted currencySymbol parties
    && hasSameUTxOHash
    && afterContestationDeadline
 where
  minted = txInfoMint txInfo

  hasSameUTxOHash =
    traceIfFalse $(errorCode FannedOutUtxoHashNotEqualToClosedUtxoHash) $
      fannedOutUtxoHash == utxoHash && decommitUtxoHash == utxoToDecommitHash
  fannedOutUtxoHash = hashTxOuts $ take numberOfFanoutOutputs txInfoOutputs

  decommitUtxoHash = hashTxOuts $ take numberOfDecommitOutputs $ drop numberOfFanoutOutputs txInfoOutputs

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

-- | Hash a potentially unordered list of commits by sorting them, concatenating
-- their 'preSerializedOutput' bytes and creating a SHA2_256 digest over that.
--
-- NOTE: See note from `hashTxOuts`.
hashPreSerializedCommits :: [Commit] -> BuiltinByteString
hashPreSerializedCommits commits =
  sha2_256 . foldMap preSerializedOutput $
    sortBy (\a b -> compareRef (input a) (input b)) commits
{-# INLINEABLE hashPreSerializedCommits #-}

-- | Hash a pre-ordered list of transaction outputs by serializing each
-- individual 'TxOut', concatenating all bytes together and creating a SHA2_256
-- digest over that.
--
-- NOTE: In general, from asserting that `hash(x || y) = hash (x' || y')` it is
-- not safe to conclude that `(x,y) = (x', y')` as the same hash could be
-- obtained by moving one or more bytes from the end of `x` to the beginning of
-- `y`, but in the context of Hydra validators it seems impossible to exploit
-- this property without breaking other logic or verification (eg. producing a
-- valid and meaningful `TxOut`).
hashTxOuts :: [TxOut] -> BuiltinByteString
hashTxOuts =
  sha2_256 . foldMap (Builtins.serialiseData . toBuiltinData)
{-# INLINEABLE hashTxOuts #-}

-- | Check if 'TxOut' contains the PT token.
hasPT :: CurrencySymbol -> TxOut -> Bool
hasPT headCurrencySymbol txOut =
  let !pts = findParticipationTokens headCurrencySymbol (txOutValue txOut)
   in length pts == 1
{-# INLINEABLE hasPT #-}

verifySnapshotSignature :: [Party] -> CurrencySymbol -> SnapshotNumber -> BuiltinByteString -> BuiltinByteString -> Integer -> [Signature] -> Bool
verifySnapshotSignature parties headId snapshotNumber utxoHash utxoToDecommitHash version sigs =
  traceIfFalse $(errorCode SignatureVerificationFailed) $
    length parties
      == length sigs
      && all (uncurry $ verifyPartySignature headId snapshotNumber utxoHash utxoToDecommitHash version) (zip parties sigs)
{-# INLINEABLE verifySnapshotSignature #-}

verifyPartySignature :: CurrencySymbol -> SnapshotNumber -> BuiltinByteString -> BuiltinByteString -> Integer -> Party -> Signature -> Bool
verifyPartySignature headId snapshotNumber utxoHash utxoToDecommitHash version party =
  verifyEd25519Signature (vkey party) message
 where
  message =
    -- TODO: document CDDL format, either here or in 'Hydra.Snapshot.getSignableRepresentation'
    Builtins.serialiseData (toBuiltinData headId)
      <> Builtins.serialiseData (toBuiltinData snapshotNumber)
      <> Builtins.serialiseData (toBuiltinData utxoHash)
      <> Builtins.serialiseData (toBuiltinData utxoToDecommitHash)
      <> Builtins.serialiseData (toBuiltinData version)
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

validatorScript :: SerialisedScript
validatorScript = serialiseCompiledCode compiledValidator

validatorHash :: ScriptHash
validatorHash = scriptValidatorHash PlutusScriptV2 validatorScript

extractClosedDatum :: ScriptContext -> (SnapshotNumber, Hash, Hash, [Party], POSIXTime, ContestationPeriod, CurrencySymbol, [PubKeyHash])
extractClosedDatum ctx =
  -- XXX: fromBuiltinData is super big (and also expensive?)
  case fromBuiltinData @DatumType $ getDatum (headOutputDatum ctx) of
    Just
      Closed
        { snapshotNumber
        , utxoHash
        , utxoToDecommitHash
        , parties = p
        , contestationDeadline = dl
        , contestationPeriod = cp
        , headId = hid
        , contesters = cs
        } -> (snapshotNumber, utxoHash, utxoToDecommitHash, p, dl, cp, hid, cs)
    _ -> traceError $(errorCode WrongStateInOutputDatum)
{-# INLINEABLE extractClosedDatum #-}

extractOpenDatum :: ScriptContext -> (Hash, [Party], SnapshotNumber, ContestationPeriod, CurrencySymbol)
extractOpenDatum ctx =
  case fromBuiltinData @DatumType $ getDatum (headOutputDatum ctx) of
    Just
      Open
        { utxoHash
        , parties = p
        , headId
        , contestationPeriod
        , snapshotNumber = sn
        } -> (utxoHash, p, sn, contestationPeriod, headId)
    _ -> traceError $(errorCode WrongStateInOutputDatum)
{-# INLINEABLE extractOpenDatum #-}
