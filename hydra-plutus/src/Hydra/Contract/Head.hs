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
  HeadError (..),
) where

import PlutusTx.Prelude

import Hydra.Contract.Commit (Commit (..))
import qualified Hydra.Contract.Commit as Commit
import Hydra.Contract.HeadState (Input (..), Signature, SnapshotNumber, State (..))
import Hydra.Contract.Util (hasST, mustNotMintOrBurn, (===))
import Hydra.Data.ContestationPeriod (ContestationPeriod, addContestationPeriod, milliseconds)
import Hydra.Data.Party (Party (vkey))
import Hydra.Prelude (Show)
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
import Hydra.Contract.Error (ToErrorCode (..))

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
      traceError "H01"

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
    traceIfFalse "H02" $
      burntTokens == length parties + 1

  minted = getValue $ txInfoMint txInfo

  burntTokens =
    case Map.lookup headCurrencySymbol minted of
      Nothing -> 0
      Just tokenMap -> negate $ sum tokenMap

  mustReimburseCommittedUTxO =
    traceIfFalse "H03" $
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
    && mustCollectUtxoHash
    && mustNotChangeParameters
    && everyoneHasCommitted
    && mustBeSignedByParticipant ctx headId
    && traceIfFalse "H04" (hasST headId val)
 where
  mustCollectUtxoHash =
    traceIfFalse "H05" $
      utxoHash == hashPreSerializedCommits collectedCommits

  mustNotChangeParameters =
    traceIfFalse "H06" $
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
      _ -> traceError "H07"
  headAddress = mkHeadAddress ctx

  val =
    maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  everyoneHasCommitted =
    traceIfFalse "H08" $
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
    && hasST headPolicyId val
    && mustPreserveValue
    && mustNotChangeParameters
 where
  mustPreserveValue =
    traceIfFalse "H09" $
      val === val'

  val' = txOutValue . head $ txInfoOutputs txInfo

  val = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  hasBoundedValidity =
    traceIfFalse "H10" $
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
      _ -> traceError "H07"

  checkSnapshot
    | closedSnapshotNumber > 0 =
      traceIfFalse "H11" $
        verifySnapshotSignature parties closedSnapshotNumber closedUtxoHash sig
    | otherwise =
      traceIfFalse "H12" $
        closedUtxoHash == initialUtxoHash

  checkDeadline =
    traceIfFalse "H13" $
      closedContestationDeadline == makeContestationDeadline cperiod ctx

  cp = fromMilliSeconds (milliseconds cperiod)

  tMax = case ivTo $ txInfoValidRange txInfo of
    UpperBound (Finite t) _ -> t
    _InfiniteBound -> traceError "H14"

  tMin = case ivFrom $ txInfoValidRange txInfo of
    LowerBound (Finite t) _ -> t
    _InfiniteBound -> traceError "H15"

  mustNotChangeParameters =
    traceIfFalse "H06" $
      headId' == headPolicyId
        && parties' == parties

  mustInitializeContesters =
    traceIfFalse "H16" $
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
    && hasST headId val
    && mustPushDeadline
    && mustNotChangeParameters
    && mustPreserveValue
 where
  mustPreserveValue =
    traceIfFalse "H09" $
      val === val'

  val' = txOutValue . head $ txInfoOutputs txInfo

  val = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput ctx

  mustBeNewer =
    traceIfFalse "H17" $
      contestSnapshotNumber > closedSnapshotNumber

  mustBeMultiSigned =
    verifySnapshotSignature parties contestSnapshotNumber contestUtxoHash sig

  mustBeWithinContestationPeriod =
    case ivTo (txInfoValidRange txInfo) of
      UpperBound (Finite time) _ ->
        traceIfFalse "H18" $ time <= contestationDeadline
      _ -> traceError "H19"

  mustNotChangeParameters =
    traceIfFalse "H06" $
      parties' == parties
        && headId' == headId
        && contestationPeriod' == contestationPeriod

  mustPushDeadline =
    if length contesters' == length parties'
      then
        traceIfFalse "H20" $
          contestationDeadline' == contestationDeadline
      else
        traceIfFalse "H21" $
          contestationDeadline' == addContestationPeriod contestationDeadline contestationPeriod

  mustUpdateContesters =
    traceIfFalse "H22" $
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
      _ -> traceError "H07"

  ScriptContext{scriptContextTxInfo = txInfo} = ctx

  contester =
    case txInfoSignatories txInfo of
      [signer] -> signer
      _ -> traceError "H23"

  checkSignedParticipantContestOnlyOnce =
    traceIfFalse "H24" $
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
  hasSameUTxOHash = traceIfFalse "H25" $ fannedOutUtxoHash == utxoHash
  fannedOutUtxoHash = hashTxOuts $ take numberOfFanoutOutputs txInfoOutputs
  TxInfo{txInfoOutputs} = txInfo

  afterContestationDeadline =
    case ivFrom (txInfoValidRange txInfo) of
      LowerBound (Finite time) _ ->
        traceIfFalse "H26" $ time > contestationDeadline
      _ -> traceError "H27"
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
    _ -> traceError "H28"
{-# INLINEABLE makeContestationDeadline #-}

mkHeadAddress :: ScriptContext -> Address
mkHeadAddress ctx =
  let headInput =
        fromMaybe
          (traceError "H29")
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
      traceIfFalse "H30" $
        signer `elem` (unTokenName <$> participationTokens)
    [] ->
      traceError "H31"
    _ ->
      traceError "H32"
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
  findTxOutDatum txInfo (head $ txInfoOutputs txInfo)
 where
  ScriptContext{scriptContextTxInfo = txInfo} = ctx
{-# INLINEABLE headOutputDatum #-}

findTxOutDatum :: TxInfo -> TxOut -> Datum
findTxOutDatum txInfo o =
  case txOutDatum o of
    NoOutputDatum -> traceError "H33"
    OutputDatumHash dh -> fromMaybe (traceError "H34") $ findDatum dh txInfo
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
  traceIfFalse "H35" $
    length parties == length sigs
      && all (uncurry $ verifyPartySignature snapshotNumber utxoHash) (zip parties sigs)
{-# INLINEABLE verifySnapshotSignature #-}

verifyPartySignature :: SnapshotNumber -> BuiltinByteString -> Party -> Signature -> Bool
verifyPartySignature snapshotNumber utxoHash party signed =
  traceIfFalse "H36" $
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

-- * Errors

data HeadError
  = InvalidHeadStateTransition
  | BurntTokenNumberMismatch
  | ReimbursedOutputsDontMatch
  | STNotSpent
  | IncorrectUtxoHash
  | ChangedParameters
  | WrongStateInOutputDatum
  | MissingCommits
  | HeadValueIsNotPreserved
  | HasBoundedValidityCheckFailed
  | InvalidSnapshotSignature
  | ClosedWithNonInitialHash
  | IncorrectClosedContestationDeadline
  | InfiniteUpperBound
  | InfiniteLowerBound
  | ContestersNonEmpty
  | TooOldSnapshot
  | UpperBoundBeyondContestationDeadline
  | ContestNoUpperBoundDefined
  | MustNotPushDeadline
  | MustPushDeadline
  | ContesterNotIncluded
  | WrongNumberOfSigners
  | SignerAlreadyContested
  | FannedOutUtxoHashNotEqualToClosedUtxoHash
  | LowerBoundBeforeContestationDeadline
  | FanoutNoLowerBoundDefined
  | CloseNoUpperBoundDefined
  | ScriptNotSpendingAHeadInput
  | SignerIsNotAParticipant
  | NoSigners
  | TooManySigners
  | NoOutputDatumError
  | DatumNotFound
  | SignatureVerificationFailed
  | PartySignatureVerificationFailed
  deriving (Show)

instance ToErrorCode HeadError where
  toErrorCode = \case
    InvalidHeadStateTransition -> "H01"
    BurntTokenNumberMismatch -> "H02"
    ReimbursedOutputsDontMatch -> "H03"
    STNotSpent -> "H04"
    IncorrectUtxoHash -> "H05"
    ChangedParameters -> "H06"
    WrongStateInOutputDatum -> "H07"
    MissingCommits -> "H08"
    HeadValueIsNotPreserved -> "H09"
    HasBoundedValidityCheckFailed -> "H10"
    InvalidSnapshotSignature -> "H11"
    ClosedWithNonInitialHash -> "H12"
    IncorrectClosedContestationDeadline -> "H13"
    InfiniteUpperBound -> "H14"
    InfiniteLowerBound -> "H15"
    ContestersNonEmpty -> "H16"
    TooOldSnapshot -> "H17"
    UpperBoundBeyondContestationDeadline -> "H18"
    ContestNoUpperBoundDefined -> "H19"
    MustNotPushDeadline -> "H20"
    MustPushDeadline -> "H21"
    ContesterNotIncluded -> "H22"
    WrongNumberOfSigners -> "H23"
    SignerAlreadyContested -> "H24"
    FannedOutUtxoHashNotEqualToClosedUtxoHash -> "H25"
    LowerBoundBeforeContestationDeadline -> "H26"
    FanoutNoLowerBoundDefined -> "H27"
    CloseNoUpperBoundDefined -> "H28"
    ScriptNotSpendingAHeadInput -> "H29"
    SignerIsNotAParticipant -> "H30"
    NoSigners -> "H31"
    TooManySigners -> "H32"
    NoOutputDatumError -> "H33"
    DatumNotFound -> "H34"
    SignatureVerificationFailed -> "H35"
    PartySignatureVerificationFailed -> "H36"
