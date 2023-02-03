{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.Head (
  DatumType,
  RedeemerType,
  headValidator,
  hashPreSerializedCommits,
  hashTxOuts,
  verifyPartySignature,
  verifySnapshotSignature,
  compiledValidator,
  validatorScript,
  validatorHash,
) where

import PlutusTx.Prelude

import Hydra.Contract.Commit (Commit (..))
import qualified Hydra.Contract.Commit as Commit
import Hydra.Contract.HeadState (Input (..), Signature, SnapshotNumber, State (..))
import Hydra.Contract.Util (hasST, mustNotMintOrBurn)
import Hydra.Data.ContestationPeriod (ContestationPeriod, addContestationPeriod, milliseconds)
import Hydra.Data.Party (Party (vkey))
import Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import Plutus.V1.Ledger.Time (fromMilliSeconds)
import Plutus.V1.Ledger.Value (assetClass, assetClassValue, valueOf)
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
import Plutus.V2.Ledger.Contexts (findDatum, findOwnInput, getContinuingOutputs)
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
    (Closed{parties, snapshotNumber = closedSnapshotNumber, contestationDeadline, headId}, Contest{signature}) ->
      checkContest ctx contestationDeadline parties closedSnapshotNumber signature headId
    (Closed{utxoHash, contestationDeadline}, Fanout{numberOfFanoutOutputs}) ->
      checkFanout utxoHash contestationDeadline numberOfFanoutOutputs ctx
    _ ->
      traceError "invalid head state transition"

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
    traceIfFalse "burnt token number mismatch" $
      burntTokens == length parties + 1

  minted = getValue $ txInfoMint txInfo

  burntTokens =
    case Map.lookup headCurrencySymbol minted of
      Nothing -> 0
      Just tokenMap -> negate $ sum tokenMap

  mustReimburseCommittedUTxO =
    traceIfFalse "reimbursed outputs dont match" $
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
--   * All commits are properly collected and locked into the contract as a hash
--     of serialized tx outputs in the same sequence as commit inputs!
--
--   * The transaction is performed (i.e. signed) by one of the head participants
--
--   * State token (ST) is present in the output
--
-- It must also initialize the on-chain state η* with a snapshot number and a
-- hash of committed outputs.
--
-- (*) In principle, η contains not a hash but a full UTXO set as well as a set
-- of dangling transactions. However, in the coordinated version of the
-- protocol, there can't be any dangling transactions and thus, it is no longer
-- required to check applicability of those transactions to the UTXO set. It
-- suffices to store a hash of the resulting outputs of that UTXO instead.
checkCollectCom ::
  -- | Script execution context
  ScriptContext ->
  (ContestationPeriod, [Party], CurrencySymbol) ->
  Bool
checkCollectCom ctx@ScriptContext{scriptContextTxInfo = txInfo} (contestationPeriod, parties, headId) =
  mustNotMintOrBurn txInfo
    && mustContinueHeadWith ctx headAddress expectedChangeValue expectedOutputDatum
    && everyoneHasCommitted
    && mustBeSignedByParticipant ctx headId
    && hasST headId outValue
 where
  headAddress = mkHeadAddress ctx

  outValue =
    maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  everyoneHasCommitted =
    traceIfFalse "missing commits" $
      nTotalCommits == length parties

  (expectedChangeValue, collectedCommits, nTotalCommits) =
    foldr
      traverseInputs
      (negate (txInfoAdaFee txInfo), [], 0)
      (txInfoInputs txInfo)

  expectedOutputDatum :: Datum
  expectedOutputDatum =
    let utxoHash = hashPreSerializedCommits collectedCommits
     in Datum $ toBuiltinData Open{parties, utxoHash, contestationPeriod, headId = headId}

  -- Collect fuel and commits from resolved inputs. Any output containing a PT
  -- is treated as a commit, "our" output is the head output and all remaining
  -- will be accumulated as 'fuel'.
  traverseInputs TxInInfo{txInInfoResolved} (fuel, commits, nCommits)
    | isHeadOutput txInInfoResolved =
      (fuel, commits, nCommits)
    | hasPT headId txInInfoResolved =
      case commitDatum txInfo txInInfoResolved of
        Just commit@Commit{} ->
          (fuel, commit : commits, succ nCommits)
        Nothing ->
          (fuel, commits, succ nCommits)
    | otherwise =
      (fuel + txOutAdaValue txInInfoResolved, commits, nCommits)

  isHeadOutput txOut = txOutAddress txOut == headAddress
{-# INLINEABLE checkCollectCom #-}

-- | Try to find the commit datum in the input and
-- if it is there return the commited utxo
commitDatum :: TxInfo -> TxOut -> Maybe Commit
commitDatum txInfo input = do
  let datum = findTxOutDatum txInfo input
  case fromBuiltinData @Commit.DatumType $ getDatum datum of
    Just (_party, _validatorHash, commit, _headId) ->
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
    && hasST headPolicyId outValue
    && mustNotChangeParameters
 where
  hasBoundedValidity =
    traceIfFalse "hasBoundedValidity check failed" $
      tMax - tMin <= cp

  outValue =
    maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  (closedSnapshotNumber, closedUtxoHash, parties', closedContestationDeadline, headId') =
    -- XXX: fromBuiltinData is super big (and also expensive?)
    case fromBuiltinData @DatumType $ getDatum (continuingDatum ctx) of
      Just
        Closed
          { snapshotNumber
          , utxoHash
          , parties = p
          , contestationDeadline
          , headId
          } -> (snapshotNumber, utxoHash, p, contestationDeadline, headId)
      _ -> traceError "wrong state in output datum"

  checkSnapshot
    | closedSnapshotNumber > 0 =
      traceIfFalse "invalid snapshot signature" $
        verifySnapshotSignature parties closedSnapshotNumber closedUtxoHash sig
    | otherwise =
      traceIfFalse "closed with non-initial hash" $
        closedUtxoHash == initialUtxoHash

  checkDeadline =
    traceIfFalse "incorrect closed contestation deadline" $
      closedContestationDeadline == makeContestationDeadline cperiod ctx

  cp = fromMilliSeconds (milliseconds cperiod)

  tMax = case ivTo $ txInfoValidRange txInfo of
    UpperBound (Finite t) _ -> t
    _InfiniteBound -> traceError "infinite upper bound"

  tMin = case ivFrom $ txInfoValidRange txInfo of
    LowerBound (Finite t) _ -> t
    _InfiniteBound -> traceError "infinite lower bound"

  mustNotChangeParameters =
    traceIfFalse "changed parameters" $
      headId' == headPolicyId
        && parties' == parties

  ScriptContext{scriptContextTxInfo = txInfo} = ctx
{-# INLINEABLE checkClose #-}

-- | The contest validator must verify that:
--
--   * The contest snapshot number is strictly greater than the closed snapshot number.
--
--   * The contest snapshot is correctly signed.
--
--   * No other parameters have changed.
--
--   * The transaction is performed before the deadline.
--
--   * The transaction is performed (i.e. signed) by one of the head participants
--
--   * State token (ST) is present in the output
checkContest ::
  ScriptContext ->
  POSIXTime ->
  [Party] ->
  -- | Snapshot number of the closed state.
  SnapshotNumber ->
  [Signature] ->
  -- | Head id
  CurrencySymbol ->
  Bool
checkContest ctx contestationDeadline parties closedSnapshotNumber sig headId =
  mustNotMintOrBurn txInfo
    && mustBeNewer
    && mustBeMultiSigned
    && mustNotChangeParameters
    && mustBeSignedByParticipant ctx headId
    && mustBeWithinContestationPeriod
    && hasST headId outValue
 where
  outValue =
    maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  mustBeNewer =
    traceIfFalse "too old snapshot" $
      contestSnapshotNumber > closedSnapshotNumber

  mustBeMultiSigned =
    verifySnapshotSignature parties contestSnapshotNumber contestUtxoHash sig

  mustBeWithinContestationPeriod =
    case ivTo (txInfoValidRange txInfo) of
      UpperBound (Finite time) _ ->
        traceIfFalse "upper bound beyond contestation deadline" $ time <= contestationDeadline
      _ -> traceError "contest: no upper bound defined"

  mustNotChangeParameters =
    traceIfFalse "changed parameters" $
      parties' == parties
        && contestationDeadline' == contestationDeadline
        && headId' == headId

  (contestSnapshotNumber, contestUtxoHash, parties', contestationDeadline', headId') =
    -- XXX: fromBuiltinData is super big (and also expensive?)
    case fromBuiltinData @DatumType $ getDatum (continuingDatum ctx) of
      Just
        Closed
          { snapshotNumber
          , utxoHash
          , parties = p
          , contestationDeadline = dl
          , headId = hid
          } -> (snapshotNumber, utxoHash, p, dl, hid)
      _ -> traceError "wrong state in output datum"

  ScriptContext{scriptContextTxInfo = txInfo} = ctx
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
  hasSameUTxOHash = traceIfFalse "fannedOutUtxoHash /= closedUtxoHash" $ fannedOutUtxoHash == utxoHash
  fannedOutUtxoHash = hashTxOuts $ take numberOfFanoutOutputs txInfoOutputs
  TxInfo{txInfoOutputs} = txInfo

  afterContestationDeadline =
    case ivFrom (txInfoValidRange txInfo) of
      LowerBound (Finite time) _ ->
        traceIfFalse "lower bound before contestation deadline" $ time > contestationDeadline
      _ -> traceError "fanout: no lower bound defined"
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
    _ -> traceError "close: no upper bound defined"
{-# INLINEABLE makeContestationDeadline #-}

mkHeadAddress :: ScriptContext -> Address
mkHeadAddress ctx =
  let headInput =
        fromMaybe
          (traceError "script not spending a head input?")
          (findOwnInput ctx)
   in txOutAddress (txInInfoResolved headInput)
{-# INLINEABLE mkHeadAddress #-}

mustBeSignedByParticipant ::
  ScriptContext ->
  CurrencySymbol ->
  Bool
mustBeSignedByParticipant ScriptContext{scriptContextTxInfo = txInfo} headCurrencySymbol =
  case getPubKeyHash <$> txInfoSignatories txInfo of
    [signer] ->
      traceIfFalse "signer is not a participant" $
        signer `elem` (unTokenName <$> participationTokens)
    [] ->
      traceError "no signers"
    _ ->
      traceError "too many signers"
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

mustContinueHeadWith :: ScriptContext -> Address -> Integer -> Datum -> Bool
mustContinueHeadWith ScriptContext{scriptContextTxInfo = txInfo} headAddress changeValue datum =
  case txInfoOutputs txInfo of
    -- NOTE: in the real scenario here we should always get two outputs - head
    -- and change one. But, since we are not dealing with the change outputs in
    -- tests we either need to keep this pattern match for a single head output
    -- or fix the end-to-end spec to actually add a change output and have more
    -- realistic txs.
    [headOutput] ->
      checkHeadOutput headOutput
    [headOutput, changeOutput] ->
      checkHeadOutput headOutput
        && traceIfFalse "change value does not match" (lovelaceValue changeValue == txOutValue changeOutput)
    _moreThanTwoOutputs -> traceError "does not have exactly two outputs"
 where
  lovelaceValue = assetClassValue (assetClass adaSymbol adaToken)
  checkHeadOutput headOutput =
    traceIfFalse "first output should be head address" (txOutAddress headOutput == headAddress)
      && traceIfFalse "wrong output head datum" (findTxOutDatum txInfo headOutput == datum)
{-# INLINEABLE mustContinueHeadWith #-}

continuingDatum :: ScriptContext -> Datum
continuingDatum ctx@ScriptContext{scriptContextTxInfo} =
  case getContinuingOutputs ctx of
    [o] -> findTxOutDatum scriptContextTxInfo o
    _ -> traceError "expected only one continuing output"
{-# INLINEABLE continuingDatum #-}

findTxOutDatum :: TxInfo -> TxOut -> Datum
findTxOutDatum txInfo o =
  case txOutDatum o of
    NoOutputDatum -> traceError "no output datum"
    OutputDatumHash dh -> fromMaybe (traceError "datum not found") $ findDatum dh txInfo
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
  traceIfFalse "signature verification failed" $
    length parties == length sigs
      && all (uncurry $ verifyPartySignature snapshotNumber utxoHash) (zip parties sigs)
{-# INLINEABLE verifySnapshotSignature #-}

verifyPartySignature :: SnapshotNumber -> BuiltinByteString -> Party -> Signature -> Bool
verifyPartySignature snapshotNumber utxoHash party signed =
  traceIfFalse "party signature verification failed" $
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

-- TODO: Add a NetworkId so that we can properly serialise address hashes
-- see 'encodeAddress' for details
compiledValidator :: CompiledCode ValidatorType
compiledValidator =
  $$(PlutusTx.compile [||wrap headValidator||])
 where
  wrap = wrapValidator @DatumType @RedeemerType

validatorScript :: Script
validatorScript = getValidator $ mkValidatorScript compiledValidator

validatorHash :: ValidatorHash
validatorHash = scriptValidatorHash validatorScript
