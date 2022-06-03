{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.Head where

import PlutusTx.Prelude

import Hydra.Contract.Commit (SerializedTxOut (..))
import qualified Hydra.Contract.Commit as Commit
import Hydra.Contract.Encoding (serialiseTxOuts)
import Hydra.Contract.HeadState (Input (..), SnapshotNumber, State (..))
import Hydra.Data.ContestationPeriod (ContestationPeriod, addContestationPeriod)
import Hydra.Data.Party (Party (vkey))
import Plutus.Codec.CBOR.Encoding (
  encodeBeginList,
  encodeBreak,
  encodeByteString,
  encodeInteger,
  encodingToBuiltinByteString,
  unsafeEncodeRaw,
 )
import Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Api (
  Address,
  CurrencySymbol,
  Datum (..),
  FromData (fromBuiltinData),
  Interval (..),
  LowerBound (LowerBound),
  POSIXTime,
  PubKeyHash (getPubKeyHash),
  Script,
  ScriptContext (..),
  ToData (toBuiltinData),
  TokenName (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  UpperBound (..),
  Validator (getValidator),
  ValidatorHash,
  Value (Value, getValue),
  adaSymbol,
  adaToken,
  mkValidatorScript,
 )
import Plutus.V1.Ledger.Contexts (findDatum, findDatumHash, findOwnInput, getContinuingOutputs)
import Plutus.V1.Ledger.Crypto (Signature (getSignature))
import Plutus.V1.Ledger.Interval (Extended (Finite))
import Plutus.V1.Ledger.Value (valueOf)
import PlutusTx (CompiledCode)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map

type DatumType = State
type RedeemerType = Input

hydraHeadV1 :: BuiltinByteString
hydraHeadV1 = "HydraHeadV1"

{-# INLINEABLE headValidator #-}
headValidator ::
  State ->
  Input ->
  ScriptContext ->
  Bool
headValidator oldState input context =
  case (oldState, input) of
    (Initial{contestationPeriod, parties}, CollectCom) ->
      checkCollectCom context headContext (contestationPeriod, parties)
    (Initial{parties}, Abort) ->
      checkAbort context headContext parties
    (Open{parties, utxoHash = initialUtxoHash, contestationPeriod}, Close{snapshotNumber, utxoHash = closedUtxoHash, signature}) ->
      checkClose context headContext parties initialUtxoHash snapshotNumber closedUtxoHash signature contestationPeriod
    (Closed{parties, snapshotNumber = closedSnapshotNumber, contestationDeadline}, Contest{snapshotNumber = contestSnapshotNumber, utxoHash = contestUtxoHash, signature}) ->
      checkContest context headContext contestationDeadline parties closedSnapshotNumber contestSnapshotNumber contestUtxoHash signature
    (Closed{utxoHash, contestationDeadline}, Fanout{numberOfFanoutOutputs}) ->
      checkFanout utxoHash contestationDeadline numberOfFanoutOutputs context
    _ ->
      traceError "invalid head state transition"
 where
  headContext = mkHeadContext context

data CheckCollectComError
  = NoContinuingOutput
  | MoreThanOneContinuingOutput
  | OutputValueNotPreserved
  | OutputHashNotMatching

data HeadContext = HeadContext
  { headAddress :: Address
  , headInputValue :: Value
  , headCurrencySymbol :: CurrencySymbol
  }

mkHeadContext :: ScriptContext -> HeadContext
mkHeadContext context =
  HeadContext
    { headAddress
    , headInputValue
    , headCurrencySymbol
    }
 where
  headInput :: TxInInfo
  headInput =
    fromMaybe
      (traceError "script not spending a head input?")
      (findOwnInput context)

  headInputValue :: Value
  headInputValue =
    txOutValue (txInInfoResolved headInput)

  headAddress :: Address
  headAddress =
    txOutAddress (txInInfoResolved headInput)

  headCurrencySymbol :: CurrencySymbol
  headCurrencySymbol =
    headInputValue
      & findCandidateSymbols
      & \case
        [s] -> s
        _ -> traceError "malformed thread token, expected exactly one asset."

  findCandidateSymbols :: Value -> [CurrencySymbol]
  findCandidateSymbols (Value v) = loop (Map.toList v)
   where
    loop = \case
      [] -> []
      (symbol, assets) : rest ->
        case filter ((TokenName hydraHeadV1, 1) ==) (Map.toList assets) of
          [] -> loop rest
          _ -> symbol : loop rest
{-# INLINEABLE mkHeadContext #-}

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
  HeadContext ->
  [Party] ->
  Bool
checkAbort context@ScriptContext{scriptContextTxInfo = txInfo} headContext parties =
  mustBurnAllHeadTokens
    && mustBeSignedByParticipant context headContext
 where
  HeadContext{headCurrencySymbol} = headContext

  mustBurnAllHeadTokens =
    traceIfFalse "number of inputs do not match number of parties" $
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
  -- | Static information about the head (i.e. address, value, currency...)
  HeadContext ->
  -- | Initial state
  (ContestationPeriod, [Party]) ->
  Bool
checkCollectCom context@ScriptContext{scriptContextTxInfo = txInfo} headContext (contestationPeriod, parties) =
  mustContinueHeadWith context headAddress expectedChangeValue expectedOutputDatum
    && everyoneHasCommitted
    && mustBeSignedByParticipant context headContext
 where
  everyoneHasCommitted =
    traceIfFalse "not everyone committed" $
      nTotalCommits == length parties

  HeadContext
    { headAddress
    , headCurrencySymbol
    } = headContext

  (expectedChangeValue, collectedCommits, nTotalCommits) =
    traverseInputs
      (negate (txInfoAdaFee txInfo), encodeBeginList, 0)
      (txInfoInputs txInfo)

  expectedOutputDatum :: Datum
  expectedOutputDatum =
    let utxoHash =
          (collectedCommits <> encodeBreak)
            & encodingToBuiltinByteString
            & sha2_256
     in Datum $ toBuiltinData Open{parties, utxoHash, contestationPeriod}

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
          Just (SerializedTxOut commit) ->
            traverseInputs
              (fuel, commits <> unsafeEncodeRaw commit, succ nCommits)
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
    let pts = findParticipationTokens headCurrencySymbol (txOutValue txOut)
     in length pts == 1

  commitDatum :: TxOut -> Maybe SerializedTxOut
  commitDatum o = do
    dh <- txOutDatumHash o
    d <- getDatum <$> findDatum dh txInfo
    case fromBuiltinData @Commit.DatumType d of
      Just (_p, _, Just serializedTxOut) ->
        Just serializedTxOut
      Just (_p, _, Nothing) ->
        Nothing
      Nothing ->
        traceError "fromBuiltinData failed"
{-# INLINEABLE checkCollectCom #-}

-- | The close validator must verify that:
--
--   * The closing snapshot number and signature is correctly signed
--
--   * The resulting closed state is consistent with the open state or the
--     closing snapshot, depending on snapshot number
--
--   * The transaction is performed (i.e. signed) by one of the head participants
checkClose ::
  ScriptContext ->
  HeadContext ->
  [Party] ->
  BuiltinByteString ->
  SnapshotNumber ->
  BuiltinByteString ->
  [Signature] ->
  ContestationPeriod ->
  Bool
checkClose ctx headContext parties initialUtxoHash snapshotNumber closedUtxoHash sig cperiod =
  checkSnapshot && mustBeSignedByParticipant ctx headContext
 where
  checkSnapshot
    | snapshotNumber == 0 =
      let expectedOutputDatum =
            Closed
              { parties
              , snapshotNumber = 0
              , utxoHash = initialUtxoHash
              , contestationDeadline = makeContestationDeadline cperiod ctx
              }
       in checkHeadOutputDatum ctx expectedOutputDatum
    | snapshotNumber > 0 =
      let expectedOutputDatum =
            Closed
              { parties
              , snapshotNumber
              , utxoHash = closedUtxoHash
              , contestationDeadline = makeContestationDeadline cperiod ctx
              }
       in verifySnapshotSignature parties snapshotNumber closedUtxoHash sig
            && checkHeadOutputDatum ctx expectedOutputDatum
    | otherwise = traceError "negative snapshot number"
{-# INLINEABLE checkClose #-}

-- | Checks that the datum
makeContestationDeadline :: ContestationPeriod -> ScriptContext -> POSIXTime
makeContestationDeadline cperiod ScriptContext{scriptContextTxInfo} =
  case ivTo (txInfoValidRange scriptContextTxInfo) of
    UpperBound (Finite time) _ -> addContestationPeriod time cperiod
    _ -> traceError "no upper bound validaty interval defined for close"
{-# INLINEABLE makeContestationDeadline #-}

-- | The contest validator must verify that:
--
--   * The contest snapshot number is strictly greater than the closed snapshot number.
--
--   * The contest snapshot is correctly signed.
--
--   * The resulting closed state is consistent with the contested snapshot.
--
--   * The transaction is performed (i.e. signed) by one of the head participants
checkContest ::
  ScriptContext ->
  HeadContext ->
  POSIXTime ->
  [Party] ->
  -- | Snapshot number of the closed state.
  -- XXX: Having two snapshot numbers here is FRAGILE
  SnapshotNumber ->
  -- | Snapshot number of the contestin snapshot.
  SnapshotNumber ->
  BuiltinByteString ->
  [Signature] ->
  Bool
checkContest ctx@ScriptContext{scriptContextTxInfo} headContext contestationDeadline parties closedSnapshotNumber contestSnapshotNumber contestUtxoHash sig =
  mustBeNewer
    && mustBeMultiSigned
    && checkHeadOutputDatum ctx (Closed{parties, snapshotNumber = contestSnapshotNumber, utxoHash = contestUtxoHash, contestationDeadline})
    && mustBeSignedByParticipant ctx headContext
    && mustBeWithinContestationPeriod
 where
  mustBeNewer =
    traceIfFalse "too old snapshot" $
      contestSnapshotNumber > closedSnapshotNumber

  mustBeMultiSigned =
    verifySnapshotSignature parties contestSnapshotNumber contestUtxoHash sig

  mustBeWithinContestationPeriod =
    case ivTo (txInfoValidRange scriptContextTxInfo) of
      UpperBound (Finite time) _ -> traceIfFalse "upper bound validity beyond contestation deadline" $ time < contestationDeadline
      _ -> traceError "no upper bound validity interval defined for contest"
{-# INLINEABLE checkContest #-}

checkHeadOutputDatum :: ToData a => ScriptContext -> a -> Bool
checkHeadOutputDatum ctx d =
  case (ownDatumHash, expectedDatumHash) of
    (Just actual, Just expected) ->
      traceIfFalse "output datum hash mismatch" $ actual == expected
    (Nothing, _) ->
      traceError "no head output datum"
    (_, Nothing) ->
      traceError "expected datum hash not found"
 where
  expectedDatumHash = findDatumHash (Datum $ toBuiltinData d) txInfo

  ownDatumHash =
    case getContinuingOutputs ctx of
      [o] -> txOutDatumHash o
      _ -> traceError "expected only one head output"

  ScriptContext{scriptContextTxInfo = txInfo} = ctx
{-# INLINEABLE checkHeadOutputDatum #-}

txOutAdaValue :: TxOut -> Integer
txOutAdaValue o = valueOf (txOutValue o) adaSymbol adaToken
{-# INLINEABLE txOutAdaValue #-}

txInfoAdaFee :: TxInfo -> Integer
txInfoAdaFee tx = valueOf (txInfoFee tx) adaSymbol adaToken
{-# INLINEABLE txInfoAdaFee #-}

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
      LowerBound (Finite time) _ -> traceIfFalse "lower bound validity before contestation deadline" $ time > contestationDeadline
      _ -> traceError "no lower bound validity interval defined for fanout"
{-# INLINEABLE checkFanout #-}

(&) :: a -> (a -> b) -> b
(&) = flip ($)
{-# INLINEABLE (&) #-}

mustBeSignedByParticipant ::
  ScriptContext ->
  HeadContext ->
  Bool
mustBeSignedByParticipant ScriptContext{scriptContextTxInfo = txInfo} HeadContext{headCurrencySymbol} =
  case getPubKeyHash <$> txInfoSignatories txInfo of
    [signer] ->
      traceIfFalse "mustBeSignedByParticipant: did not find expected signer" $
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
        traceIfFalse "wrong output head datum" (txOutDatum txInfo o == datum)
          && traceIfFalse "wrong output value" (checkOutputValue (xs <> rest))
    (o : rest) ->
      checkOutputDatum (o : xs) rest

  checkOutputValue = \case
    [] ->
      True
    [o]
      | txOutAddress o /= headAddress ->
        txOutValue o == lovelaceValueOf changeValue
    _ ->
      traceError "invalid collect-com outputs: more than 2 outputs."
{-# INLINEABLE mustContinueHeadWith #-}

txOutDatum :: TxInfo -> TxOut -> Datum
txOutDatum txInfo o =
  case txOutDatumHash o >>= (`findDatum` txInfo) of
    Nothing -> traceError "no datum"
    Just dt -> dt
{-# INLINEABLE txOutDatum #-}

hashPreSerializedCommits :: [SerializedTxOut] -> BuiltinByteString
hashPreSerializedCommits o =
  sha2_256 . encodingToBuiltinByteString $
    encodeBeginList
      <> foldMap (\(SerializedTxOut bytes) -> unsafeEncodeRaw bytes) o
      <> encodeBreak
{-# INLINEABLE hashPreSerializedCommits #-}

hashTxOuts :: [TxOut] -> BuiltinByteString
hashTxOuts =
  sha2_256 . serialiseTxOuts
{-# INLINEABLE hashTxOuts #-}

verifySnapshotSignature :: [Party] -> SnapshotNumber -> BuiltinByteString -> [Signature] -> Bool
verifySnapshotSignature parties snapshotNumber utxoHash sigs =
  traceIfFalse "signature verification failed" $
    length parties == length sigs
      && all (uncurry $ verifyPartySignature snapshotNumber utxoHash) (zip parties sigs)
{-# INLINEABLE verifySnapshotSignature #-}

verifyPartySignature :: SnapshotNumber -> BuiltinByteString -> Party -> Signature -> Bool
verifyPartySignature snapshotNumber utxoHash party signed =
  traceIfFalse "party signature verification failed" $
    verifySignature (vkey party) message (getSignature signed)
 where
  message =
    encodingToBuiltinByteString $
      encodeInteger snapshotNumber <> encodeByteString utxoHash
{-# INLINEABLE verifyPartySignature #-}

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
