{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.Head where

import Ledger hiding (txOutDatum, validatorHash)
import PlutusTx.Prelude

import Hydra.Contract.Commit (Commit, SerializedTxOut (..))
import qualified Hydra.Contract.Commit as Commit
import Hydra.Contract.Encoding (serialiseTxOuts)
import Hydra.Contract.HeadState (Input (..), SnapshotNumber, State (..))
import qualified Hydra.Contract.Initial as Initial
import Hydra.Data.ContestationPeriod (ContestationPeriod)
import Hydra.Data.Party (Party (UnsafeParty))
import Ledger.Typed.Scripts (TypedValidator, ValidatorType, ValidatorTypes (RedeemerType))
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Typed.Scripts.Validators (DatumType)
import Plutus.Codec.CBOR.Encoding (
  encodeBeginList,
  encodeBreak,
  encodeInteger,
  encodingToBuiltinByteString,
  unsafeEncodeRaw,
 )
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken, lovelaceValueOf)
import Plutus.V1.Ledger.Value (TokenName (..), Value (..), valueOf)
import PlutusTx (fromBuiltinData, toBuiltinData)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Code (CompiledCode)

data Head

instance Scripts.ValidatorTypes Head where
  type DatumType Head = State
  type RedeemerType Head = Input

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
    (Open{parties}, Close{snapshotNumber, signature}) ->
      checkClose context headContext parties snapshotNumber signature
    (Closed{utxoHash}, Fanout{numberOfFanoutOutputs}) ->
      checkFanout utxoHash numberOfFanoutOutputs context
    _ -> False
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
    case fromBuiltinData @(DatumType Commit) d of
      Just (_p, _, Just o) ->
        Just o
      Just (_p, _, Nothing) ->
        Nothing
      Nothing ->
        traceError "fromBuiltinData failed"
{-# INLINEABLE checkCollectCom #-}

checkClose ::
  ScriptContext ->
  HeadContext ->
  [Party] ->
  SnapshotNumber ->
  [Signature] ->
  Bool
checkClose context headContext parties snapshotNumber sig =
  checkSnapshot && mustBeSignedByParticipant context headContext
 where
  checkSnapshot
    | snapshotNumber == 0 = True
    | snapshotNumber > 0 = verifySnapshotSignature parties snapshotNumber sig
    | otherwise = traceError "negative snapshot number"
{-# INLINEABLE checkClose #-}

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
  traceIfFalse "mustBeSignedByParticipant: did not found expected signer" $
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
          && checkOutputValue (xs <> rest)
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

verifySnapshotSignature :: [Party] -> SnapshotNumber -> [Signature] -> Bool
verifySnapshotSignature parties snapshotNumber sigs =
  traceIfFalse "signature verification failed" $
    length parties == length sigs
      && all (uncurry $ verifyPartySignature snapshotNumber) (zip parties sigs)
{-# INLINEABLE verifySnapshotSignature #-}

verifyPartySignature :: SnapshotNumber -> Party -> Signature -> Bool
verifyPartySignature snapshotNumber vkey signed =
  traceIfFalse "party signature verification failed" $
    mockVerifySignature vkey snapshotNumber (getSignature signed)
{-# INLINEABLE verifyPartySignature #-}

-- TODO: This really should be the builtin Plutus function 'verifySignature' but as we
-- are using Mock crypto in the Head, so must we use Mock crypto on-chain to verify
-- signatures.
mockVerifySignature :: Party -> SnapshotNumber -> BuiltinByteString -> Bool
mockVerifySignature (UnsafeParty vkey) snapshotNumber signed =
  traceIfFalse "mock signed message is not equal to signed" $
    mockSign vkey (encodingToBuiltinByteString $ encodeInteger snapshotNumber) == signed
{-# INLINEABLE mockVerifySignature #-}

mockSign :: Integer -> BuiltinByteString -> BuiltinByteString
mockSign vkey msg = appendByteString (sliceByteString 0 8 hashedMsg) (encodingToBuiltinByteString $ encodeInteger vkey)
 where
  hashedMsg = sha2_256 msg
{-# INLINEABLE mockSign #-}

-- | The script instance of the auction state machine. It contains the state
-- machine compiled to a Plutus core validator script.
-- TODO: Add a NetworkId so that we can properly serialise address hashes
-- see 'encodeAddress' for details
typedValidator :: TypedValidator Head
typedValidator =
  Scripts.mkTypedValidator @Head
    compiledValidator
    $$(PlutusTx.compile [||wrap||])
 where
  wrap = Scripts.wrapValidator @(DatumType Head) @(RedeemerType Head)

compiledValidator :: CompiledCode (ValidatorType Head)
compiledValidator =
  $$(PlutusTx.compile [||headValidator||])
    `PlutusTx.applyCode` PlutusTx.liftCode Commit.address
    `PlutusTx.applyCode` PlutusTx.liftCode Initial.address

validatorHash :: ValidatorHash
validatorHash = Scripts.validatorHash typedValidator

address :: Address
address = scriptHashAddress validatorHash

-- | Get the actual plutus script. Mainly used to serialize and use in
-- transactions.
validatorScript :: Script
validatorScript = unValidatorScript $ Scripts.validatorScript typedValidator
