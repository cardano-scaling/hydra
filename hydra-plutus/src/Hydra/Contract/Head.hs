{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.Head where

import PlutusTx.Prelude

import Hydra.Contract.Commit (Commit (..))
import qualified Hydra.Contract.Commit as Commit
import Hydra.Contract.HeadState (Input (..), Signature, SnapshotNumber, State (..))
import Hydra.Data.ContestationPeriod (ContestationPeriod, addContestationPeriod, milliseconds)
import Hydra.Data.Party (Party (vkey))
import Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
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
import Plutus.V2.Ledger.Contexts (findDatum, findDatumHash, findOwnInput, getContinuingOutputs)
import PlutusTx (CompiledCode)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map
import qualified PlutusTx.Builtins as Builtins

-- REVIEW: Functions not re-exported "as V2", but using the same data types.
import Plutus.V1.Ledger.Time (fromMilliSeconds)
import Plutus.V1.Ledger.Value (assetClass, assetClassValue, valueOf)

type DatumType = State
type RedeemerType = Input

hydraHeadV1 :: BuiltinByteString
hydraHeadV1 = "HydraHeadV1"

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
    (initialState@Initial{}, CollectCom) ->
      checkCollectCom ctx (mkHeadAddress ctx) initialState
    (Initial{parties, initialHeadId}, Abort) ->
      checkAbort ctx initialHeadId parties
    (Open{parties, utxoHash = initialUtxoHash, contestationPeriod, openHeadId}, Close{snapshotNumber, utxoHash = closedUtxoHash, signature}) ->
      checkClose ctx parties initialUtxoHash snapshotNumber closedUtxoHash signature contestationPeriod openHeadId
    (Closed{parties, snapshotNumber = closedSnapshotNumber, contestationDeadline, closedHeadId}, Contest{snapshotNumber = contestSnapshotNumber, utxoHash = contestUtxoHash, signature}) ->
      checkContest ctx contestationDeadline parties closedSnapshotNumber contestSnapshotNumber contestUtxoHash signature closedHeadId
    (Closed{utxoHash, contestationDeadline}, Fanout{numberOfFanoutOutputs}) ->
      checkFanout utxoHash contestationDeadline numberOfFanoutOutputs ctx
    _ ->
      traceError "invalid head state transition"

-- | On-Chain verification for 'Abort' transition. It verifies that:
--
--   * All PTs have been burnt: The right number of Head tokens, both PT for
--     parties and thread token, with the correct head id, are burnt,
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
    && hasSTToken headCurrencySymbol outValue
 where
  outValue =
    maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  mustBurnAllHeadTokens =
    burntTokens == length parties + 1

  minted = getValue $ txInfoMint txInfo

  burntTokens =
    case Map.lookup headCurrencySymbol minted of
      Nothing -> 0
      Just tokenMap -> negate $ sum tokenMap

-- | On-Chain verification for 'CollectCom' transition. It verifies that:
--
--   * All participants have committed (even empty commits)
--
--   * All commits are properly collected and locked into the contract as a hash
--     of serialized tx outputs in the same sequence as commit inputs!
--
--   * The transaction is performed (i.e. signed) by one of the head participants
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
  -- | Head address
  Address ->
  -- | Initial state
  State ->
  Bool
checkCollectCom ctx@ScriptContext{scriptContextTxInfo = txInfo} headAddress Initial{contestationPeriod, parties, initialHeadId} =
  mustContinueHeadWith ctx headAddress expectedChangeValue expectedOutputDatum
    && everyoneHasCommitted
    && mustBeSignedByParticipant ctx initialHeadId
    && hasSTToken initialHeadId outValue
 where
  outValue =
    maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx
  everyoneHasCommitted =
    nTotalCommits == length parties

  (expectedChangeValue, collectedCommits, nTotalCommits) =
    traverseInputs
      (negate (txInfoAdaFee txInfo), [], 0)
      (txInfoInputs txInfo)

  expectedOutputDatum :: Datum
  expectedOutputDatum =
    let utxoHash = hashPreSerializedCommits collectedCommits
     in Datum $ toBuiltinData Open{parties, utxoHash, contestationPeriod, openHeadId = initialHeadId}

  -- Collect fuel and commits from resolved inputs. Any output containing a PT
  -- is treated as a commit, "our" output is the head output and all remaining
  -- will be accumulated as 'fuel'.
  traverseInputs (fuel, commits, nCommits) = \case
    [] ->
      (fuel, commits, nCommits)
    TxInInfo{txInInfoResolved} : rest
      | isHeadOutput txInInfoResolved ->
        traverseInputs
          (fuel, commits, nCommits)
          rest
      | hasPT txInInfoResolved ->
        case commitDatum txInInfoResolved of
          Just commit@Commit{} ->
            traverseInputs
              (fuel, commit : commits, succ nCommits)
              rest
          Nothing ->
            traverseInputs
              (fuel, commits, succ nCommits)
              rest
      | otherwise ->
        traverseInputs
          (fuel + txOutAdaValue txInInfoResolved, commits, nCommits)
          rest

  isHeadOutput txOut = txOutAddress txOut == headAddress

  hasPT txOut =
    let pts = findParticipationTokens initialHeadId (txOutValue txOut)
     in length pts == 1

  commitDatum :: TxOut -> Maybe Commit
  commitDatum o = do
    let d = findTxOutDatum txInfo o
    case fromBuiltinData @Commit.DatumType $ getDatum d of
      Just (_p, _, mCommit) ->
        mCommit
      Nothing ->
        traceError "commitDatum failed fromBuiltinData"
checkCollectCom _context _headContext _ = traceError "Expected Initial state in checkCollectCom"
{-# INLINEABLE checkCollectCom #-}

-- | The close validator must verify that:
--   * Check that the closing tx validity is bounded by contestation period
--     Expressed in our spec as: `T_max <= T_min + CP`
--
--   * The closing snapshot number and signature is correctly signed
--
--   * The resulting closed state is consistent with the open state or the
--     closing snapshot, depending on snapshot number
--
--   * The transaction is performed (i.e. signed) by one of the head participants
checkClose ::
  ScriptContext ->
  [Party] ->
  BuiltinByteString ->
  SnapshotNumber ->
  BuiltinByteString ->
  [Signature] ->
  ContestationPeriod ->
  CurrencySymbol ->
  Bool
checkClose ctx parties initialUtxoHash snapshotNumber closedUtxoHash sig cperiod headPolicyId =
  hasBoundedValidity
    && checkSnapshot
    && mustBeSignedByParticipant ctx headPolicyId
    && hasSTToken headPolicyId outValue
 where
  hasBoundedValidity = tMax - tMin <= cp

  outValue =
    maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  checkSnapshot
    | snapshotNumber == 0 =
      let expectedOutputDatum =
            Closed
              { parties
              , snapshotNumber = 0
              , utxoHash = initialUtxoHash
              , contestationDeadline = makeContestationDeadline cperiod ctx
              , closedHeadId = headPolicyId
              }
       in checkHeadOutputDatum ctx expectedOutputDatum
    | snapshotNumber > 0 =
      let expectedOutputDatum =
            Closed
              { parties
              , snapshotNumber
              , utxoHash = closedUtxoHash
              , contestationDeadline = makeContestationDeadline cperiod ctx
              , closedHeadId = headPolicyId
              }
       in verifySnapshotSignature parties snapshotNumber closedUtxoHash sig
            && checkHeadOutputDatum ctx expectedOutputDatum
    | otherwise = traceError "negative snapshot number"

  cp = fromMilliSeconds (milliseconds cperiod)

  tMax = case ivTo $ txInfoValidRange txInfo of
    UpperBound (Finite t) _ -> t
    _InfiniteBound -> traceError "infinite upper bound"

  tMin = case ivFrom $ txInfoValidRange txInfo of
    LowerBound (Finite t) _ -> t
    _InfiniteBound -> traceError "infinite lower bound"

  ScriptContext{scriptContextTxInfo = txInfo} = ctx
{-# INLINEABLE checkClose #-}

-- | The contest validator must verify that:
--
--   * The contest snapshot number is strictly greater than the closed snapshot number.
--
--   * The contest snapshot is correctly signed.
--
--   * The resulting closed state is consistent with the contested snapshot.
--
--   * The transaction is performed (i.e. signed) by one of the head participants
--   * ST token is present in the output
checkContest ::
  ScriptContext ->
  POSIXTime ->
  [Party] ->
  -- | Snapshot number of the closed state.
  -- XXX: Having two snapshot numbers here is FRAGILE
  SnapshotNumber ->
  -- | Snapshot number of the contestin snapshot.
  SnapshotNumber ->
  BuiltinByteString ->
  [Signature] ->
  -- | Head id
  CurrencySymbol ->
  Bool
checkContest ctx@ScriptContext{scriptContextTxInfo} contestationDeadline parties closedSnapshotNumber contestSnapshotNumber contestUtxoHash sig headPolicyId =
  mustBeNewer
    && mustBeMultiSigned
    && checkHeadOutputDatum
      ctx
      (Closed{parties, snapshotNumber = contestSnapshotNumber, utxoHash = contestUtxoHash, contestationDeadline, closedHeadId = headPolicyId})
    && mustBeSignedByParticipant ctx headPolicyId
    && mustBeWithinContestationPeriod
    && hasSTToken headPolicyId outValue
 where
  outValue =
    maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx
  mustBeNewer =
    contestSnapshotNumber > closedSnapshotNumber

  mustBeMultiSigned =
    verifySnapshotSignature parties contestSnapshotNumber contestUtxoHash sig

  mustBeWithinContestationPeriod =
    case ivTo (txInfoValidRange scriptContextTxInfo) of
      UpperBound (Finite time) _ -> time <= contestationDeadline
      _ -> traceError "no upper bound validity interval defined for contest"
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
  hasSameUTxOHash = fannedOutUtxoHash == utxoHash
  fannedOutUtxoHash = hashTxOuts $ take numberOfFanoutOutputs txInfoOutputs
  TxInfo{txInfoOutputs} = txInfo

  afterContestationDeadline =
    case ivFrom (txInfoValidRange txInfo) of
      LowerBound (Finite time) _ -> time > contestationDeadline
      _ -> traceError "no lower bound validity interval defined for fanout"
{-# INLINEABLE checkFanout #-}

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

(&) :: a -> (a -> b) -> b
(&) = flip ($)
{-# INLINEABLE (&) #-}

-- | Check that the output datum of this script corresponds to an expected
-- value. Takes care of resolving datum hashes and inline datums.
checkHeadOutputDatum :: ToData a => ScriptContext -> a -> Bool
checkHeadOutputDatum ctx d =
  case ownDatum of
    NoOutputDatum ->
      traceError "missing datum"
    OutputDatumHash actualHash ->
      Just actualHash == expectedHash
    OutputDatum actual ->
      getDatum actual == expectedData
 where
  expectedData = toBuiltinData d

  expectedHash = findDatumHash (Datum $ toBuiltinData d) txInfo

  ownDatum =
    case getContinuingOutputs ctx of
      [o] -> txOutDatum o
      _ -> traceError "expected only one head output"

  ScriptContext{scriptContextTxInfo = txInfo} = ctx
{-# INLINEABLE checkHeadOutputDatum #-}

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
    _ -> traceError "no upper bound validaty interval defined for close"
{-# INLINEABLE makeContestationDeadline #-}

-- | Checks that the output contains the ST token with the head 'CurrencySymbol'
-- and 'TokenName' of 'hydraHeadV1'
hasSTToken :: CurrencySymbol -> Value -> Bool
hasSTToken headPolicyId v =
  isJust $
    find
      (\(cs, tokenMap) -> cs == headPolicyId && hasHydraToken tokenMap)
      (Map.toList $ getValue v)
 where
  hasHydraToken tm =
    isJust $ find (\(tn, q) -> q == 1 && TokenName hydraHeadV1 == tn) (Map.toList tm)
{-# INLINEABLE hasSTToken #-}

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
      signer `elem` (unTokenName <$> participationTokens)
    [] ->
      traceError "mustBeSignedByParticipant: no signers"
    _ ->
      traceError "mustBeSignedByParticipant: too many signers"
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
  checkOutputDatum [] (txInfoOutputs txInfo)
 where
  checkOutputDatum xs = \case
    [] ->
      traceError "no continuing head output"
    (o : rest)
      | txOutAddress o == headAddress ->
        findTxOutDatum txInfo o == datum
          && checkOutputValue (xs <> rest)
    (o : rest) ->
      checkOutputDatum (o : xs) rest

  checkOutputValue = \case
    [] ->
      True
    [o]
      | txOutAddress o /= headAddress ->
        txOutValue o == lovelaceValue changeValue
    _ ->
      traceError "invalid collect-com outputs: more than 2 outputs."

  lovelaceValue = assetClassValue (assetClass adaSymbol adaToken)
{-# INLINEABLE mustContinueHeadWith #-}

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

verifySnapshotSignature :: [Party] -> SnapshotNumber -> BuiltinByteString -> [Signature] -> Bool
verifySnapshotSignature parties snapshotNumber utxoHash sigs =
  length parties == length sigs
    && all (uncurry $ verifyPartySignature snapshotNumber utxoHash) (zip parties sigs)
{-# INLINEABLE verifySnapshotSignature #-}

verifyPartySignature :: SnapshotNumber -> BuiltinByteString -> Party -> Signature -> Bool
verifyPartySignature snapshotNumber utxoHash party =
  verifyEd25519Signature (vkey party) message
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
