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
import Plutus.V1.Ledger.Ada (adaSymbol)
import Plutus.V1.Ledger.Value (Value (Value), symbols)
import PlutusTx (fromBuiltinData, toBuiltinData)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Code (CompiledCode)

data Head

instance Scripts.ValidatorTypes Head where
  type DatumType Head = State
  type RedeemerType Head = Input

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
      checkCollectCom commitAddress (contestationPeriod, parties) context
    (Initial{parties}, Abort) ->
      checkAbort commitAddress initialAddress parties context
    (Open{parties}, Close{snapshotNumber, signature})
      | snapshotNumber == 0 -> True
      | snapshotNumber > 0 -> verifySnapshotSignature parties snapshotNumber signature
      | otherwise -> False
    (Closed{utxoHash}, Fanout{numberOfFanoutOutputs}) ->
      checkFanout utxoHash numberOfFanoutOutputs context
    _ -> False

data CheckCollectComError
  = NoContinuingOutput
  | MoreThanOneContinuingOutput
  | OutputValueNotPreserved
  | OutputHashNotMatching

-- | On-Chain Validation for 'Abort' transition.
-- It must verify that:
--  * All PTs have been burnt
--  * It has collected inputs for all parties, either from `Initial` or `Commit` script.
checkAbort ::
  Address ->
  Address ->
  [Party] ->
  ScriptContext ->
  Bool
checkAbort commitAddress initialAddress parties ScriptContext{scriptContextTxInfo = txInfo} =
  consumeInputsForAllParties
 where
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
  -- | The commit script address
  Address ->
  -- | Initial state
  (ContestationPeriod, [Party]) ->
  -- | Script execution context
  ScriptContext ->
  Bool
checkCollectCom commitAddress (_, parties) context@ScriptContext{scriptContextTxInfo = txInfo} =
  mustContinueHeadWith context expectedOutputValue expectedOutputDatum
    && everyoneHasCommitted
 where
  everyoneHasCommitted =
    nTotalCommits == length parties

  headInputValue :: Value
  headInputValue =
    maybe mempty (txOutValue . txInInfoResolved) (findOwnInput context)

  headCurrencySymbol =
    headInputValue
      & symbols
      & filter (/= adaSymbol)
      & \case
        [s] -> s
        _ -> traceError "malformed thread token, expected single asset"

  -- TODO: Can find own input (i.e. 'headInputValue') during this traversal already.
  (expectedOutputValue, collectedCommits, nTotalCommits) =
    traverseInputs (headInputValue, encodeBeginList, 0) (txInfoInputs txInfo)

  expectedOutputDatum :: Datum
  expectedOutputDatum =
    let utxoHash =
          (collectedCommits <> encodeBreak)
            & encodingToBuiltinByteString
            & sha2_256
     in Datum $ toBuiltinData Open{parties, utxoHash}

  traverseInputs (val, commits, nCommits) = \case
    [] -> (val, commits, nCommits)
    TxInInfo{txInInfoResolved} : rest ->
      if txOutAddress txInInfoResolved == commitAddress
        then case commitFrom txInInfoResolved of
          (commitValue, Just (SerializedTxOut commit))
            | hasParticipationToken commitValue ->
              traverseInputs
                (val + commitValue, commits <> unsafeEncodeRaw commit, succ nCommits)
                rest
          (commitValue, Nothing)
            | hasParticipationToken commitValue ->
              traverseInputs
                (val + commitValue, commits, succ nCommits)
                rest
          _ ->
            traceError "Invalid commit: does not contain valid PT."
        else
          traverseInputs
            (val, commits, nCommits)
            rest

  hasParticipationToken :: Value -> Bool
  hasParticipationToken (Value val) =
    headCurrencySymbol `Map.member` val

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

mustContinueHeadWith :: ScriptContext -> Value -> Datum -> Bool
mustContinueHeadWith context@ScriptContext{scriptContextTxInfo = txInfo} val datum =
  case findContinuingOutputs context of
    [ix] ->
      let headOutput = txInfoOutputs txInfo !! ix
          checkOutputValue =
            traceIfFalse "wrong output head value" $ txOutValue headOutput == val
          checkOutputDatum =
            traceIfFalse "wrong output head datum" $ txOutDatum txInfo headOutput == datum
       in checkOutputValue && checkOutputDatum
    [] ->
      traceIfFalse "no continuing head output" False
    _ ->
      traceIfFalse "more than one continuing head output" False
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
