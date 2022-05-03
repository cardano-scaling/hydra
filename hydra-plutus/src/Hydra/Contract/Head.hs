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
import qualified Hydra.Contract.Initial as Initial
import Hydra.Data.ContestationPeriod (ContestationPeriod)
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
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Api (
  Address,
  CurrencySymbol,
  Datum (..),
  DatumHash,
  FromData (fromBuiltinData),
  PubKeyHash (getPubKeyHash),
  Script,
  ScriptContext (..),
  ToData (toBuiltinData),
  TokenName (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  Validator (getValidator),
  ValidatorHash,
  Value (Value),
  adaSymbol,
  adaToken,
  mkValidatorScript,
 )
import Plutus.V1.Ledger.Contexts (findDatum, findDatumHash, findOwnInput, getContinuingOutputs)
import Plutus.V1.Ledger.Crypto (Signature (getSignature))
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
  -- | Commit script address. NOTE: Used to identify inputs from commits and
  -- likely could be replaced by looking for PTs.
  Address ->
  -- | Inital script address. NOTE: Used to identify inputs from initials and
  -- likely could be replaced by looking for PTs.
  Address ->
  State ->
  Input ->
  ScriptContext ->
  Bool
headValidator commitAddress initialAddress oldState input context =
  case (oldState, input) of
    (Initial{contestationPeriod, parties}, CollectCom) ->
      checkCollectCom context headContext (contestationPeriod, parties)
    (Initial{parties}, Abort) ->
      checkAbort context headContext parties
    (Open{parties, utxoHash = initialUtxoHash}, Close{snapshotNumber, utxoHash = closedUtxoHash, signature}) ->
      checkClose context headContext parties initialUtxoHash snapshotNumber closedUtxoHash signature
    (Closed{utxoHash}, Fanout{numberOfFanoutOutputs}) ->
      checkFanout utxoHash numberOfFanoutOutputs context
    _ -> traceError "invalid head state transition"
 where
  headContext = mkHeadContext context initialAddress commitAddress

data CheckCollectComError
  = NoContinuingOutput
  | MoreThanOneContinuingOutput
  | OutputValueNotPreserved
  | OutputHashNotMatching

data HeadContext = HeadContext
  { headAddress :: Address
  , headInputValue :: Value
  , headCurrencySymbol :: CurrencySymbol
  , commitAddress :: Address
  , initialAddress :: Address
  }

mkHeadContext :: ScriptContext -> Address -> Address -> HeadContext
mkHeadContext context initialAddress commitAddress =
  HeadContext
    { headAddress
    , headInputValue
    , headCurrencySymbol
    , initialAddress
    , commitAddress
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

-- | On-Chain Validation for 'Abort' transition.
-- It must verify that:
--  * All PTs have been burnt
--  * It has collected inputs for all parties, either from `Initial` or `Commit` script.
checkAbort ::
  ScriptContext ->
  HeadContext ->
  [Party] ->
  Bool
checkAbort context@ScriptContext{scriptContextTxInfo = txInfo} headContext parties =
  consumeInputsForAllParties
    && mustBeSignedByParticipant context headContext
 where
  HeadContext{initialAddress, commitAddress} = headContext

  consumeInputsForAllParties =
    traceIfFalse "number of inputs do not match number of parties" $
      length parties == length initialAndCommitInputs
  initialAndCommitInputs =
    filter
      ( \TxInInfo{txInInfoResolved} ->
          let addr = txOutAddress txInInfoResolved
           in addr == commitAddress || addr == initialAddress
      )
      (txInfoInputs txInfo)

-- | On-Chain Validation for the 'CollectCom' transition.
--
-- The 'CollectCom' transition must verify that:
--
-- - All participants have committed (even empty commits)
-- - All commits are properly collected and locked into the contract
-- - The transaction is performed (i.e. signed) by one of the head participants
--
-- It must also Initialize the on-chain state η* with a snapshot number and a
-- Merkle-Tree root hash of committed outputs.
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
checkCollectCom context@ScriptContext{scriptContextTxInfo = txInfo} headContext (_, parties) =
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
    , commitAddress
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
     in Datum $ toBuiltinData Open{parties, utxoHash}

  traverseInputs (fuel, commits, nCommits) = \case
    [] ->
      (fuel, commits, nCommits)
    TxInInfo{txInInfoResolved} : rest
      | txOutAddress txInInfoResolved == headAddress ->
        traverseInputs
          (fuel, commits, nCommits)
          rest
      | txOutAddress txInInfoResolved == commitAddress ->
        case commitFrom txInInfoResolved of
          (commitValue, Just (SerializedTxOut commit)) ->
            case matchParticipationToken headCurrencySymbol commitValue of
              [_] ->
                traverseInputs
                  (fuel, commits <> unsafeEncodeRaw commit, succ nCommits)
                  rest
              _ ->
                traceError "Invalid commit: does not contain valid PT."
          (commitValue, Nothing) ->
            case matchParticipationToken headCurrencySymbol commitValue of
              [_] ->
                traverseInputs
                  (fuel, commits, succ nCommits)
                  rest
              _ ->
                traceError "Invalid commit: does not contain valid PT."
      | otherwise ->
        traverseInputs
          (fuel + txOutAdaValue txInInfoResolved, commits, nCommits)
          rest

  commitFrom :: TxOut -> (Value, Maybe SerializedTxOut)
  commitFrom o =
    case txOutDatumHash o >>= lookupCommit of
      Nothing -> (txOutValue o, Nothing)
      Just commit -> (txOutValue o, Just commit)

  lookupCommit :: DatumHash -> Maybe SerializedTxOut
  lookupCommit h = do
    d <- getDatum <$> findDatum h txInfo
    case fromBuiltinData @Commit.DatumType d of
      Just (_p, _, Just o) ->
        Just o
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
  Bool
checkClose ctx headContext parties initialUtxoHash snapshotNumber closedUtxoHash sig =
  checkSnapshot && mustBeSignedByParticipant ctx headContext
 where
  checkSnapshot
    | snapshotNumber == 0 = checkHeadOutputDatum ctx (Closed 0 initialUtxoHash)
    | snapshotNumber > 0 =
      verifySnapshotSignature parties snapshotNumber closedUtxoHash sig
        && checkHeadOutputDatum ctx (Closed snapshotNumber closedUtxoHash)
    | otherwise = traceError "negative snapshot number"
{-# INLINEABLE checkClose #-}

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
  Integer ->
  ScriptContext ->
  Bool
checkFanout utxoHash numberOfFanoutOutputs ScriptContext{scriptContextTxInfo = txInfo} =
  traceIfFalse "fannedOutUtxoHash /= closedUtxoHash" $ fannedOutUtxoHash == utxoHash
 where
  fannedOutUtxoHash = hashTxOuts $ take numberOfFanoutOutputs txInfoOutputs
  TxInfo{txInfoOutputs} = txInfo
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
      matchParticipationToken headCurrencySymbol (txOutValue txInInfoResolved) ++ loop rest
{-# INLINEABLE mustBeSignedByParticipant #-}

matchParticipationToken :: CurrencySymbol -> Value -> [TokenName]
matchParticipationToken headCurrency (Value val) =
  case Map.toList <$> Map.lookup headCurrency val of
    Just tokens ->
      mapMaybe (\(tokenName, n) -> if n == 1 then Just tokenName else Nothing) tokens
    _ ->
      []
{-# INLINEABLE matchParticipationToken #-}

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
-- TODO: Use validatorHash directly in headValidator arguments
compiledValidator :: CompiledCode ValidatorType
compiledValidator =
  $$(PlutusTx.compile [||\ca ia -> wrap (headValidator ca ia)||])
    `PlutusTx.applyCode` PlutusTx.liftCode (scriptHashAddress Commit.validatorHash)
    `PlutusTx.applyCode` PlutusTx.liftCode (scriptHashAddress Initial.validatorHash)
 where
  wrap = wrapValidator @DatumType @RedeemerType

validatorScript :: Script
validatorScript = getValidator $ mkValidatorScript compiledValidator

validatorHash :: ValidatorHash
validatorHash = scriptValidatorHash validatorScript
