{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.Head where

import Ledger hiding (validatorHash)
import PlutusTx.Prelude

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Hydra.Contract.Commit (Commit)
import qualified Hydra.Contract.Commit as Commit
import Hydra.Contract.Encoding (serialiseTxOuts)
import Hydra.Data.ContestationPeriod (ContestationPeriod)
import Hydra.Data.Party (Party (UnsafeParty))
import Hydra.Data.Utxo (Utxo (Utxo))
import Ledger.Typed.Scripts (TypedValidator, ValidatorTypes (RedeemerType))
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Typed.Scripts.Validators (DatumType)
import Plutus.Codec.CBOR.Encoding (
  encodeBeginList,
  encodeBreak,
  encodeInteger,
  encodeRaw,
  encodingToBuiltinByteString,
 )
import PlutusTx (fromBuiltinData, toBuiltinData)
import qualified PlutusTx
import Text.Show (Show)

type SnapshotNumber = Integer

type Hash = BuiltinByteString

data State
  = Initial {contestationPeriod :: ContestationPeriod, parties :: [Party]}
  | Open {parties :: [Party], utxoHash :: Hash}
  | Closed {snapshotNumber :: SnapshotNumber, utxoHash :: Hash}
  | Final
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''State

data Input
  = -- FIXME: This `Hash` needs to be calculated by the on-chain script and not
    -- provided as redeemer. This requires:
    --
    -- (a) finding the new state-machine's state, make sure it's Open and extract the hash
    -- (b) construct that merkle root from the collected UTXO
    -- (c) controll that (a) and (b) matches.
    CollectCom {utxoHash :: Hash}
  | Close
      { snapshotNumber :: SnapshotNumber
      , utxoHash :: Hash
      , signature :: [Signature]
      }
  | Abort
  | Fanout {numberOfFanoutOutputs :: Integer}
  deriving (Generic, Show)

PlutusTx.unstableMakeIsData ''Input

data Head

instance Scripts.ValidatorTypes Head where
  type DatumType Head = State
  type RedeemerType Head = Input

-- TODO: Add state checkings as done previously by SM
{-# INLINEABLE headValidator #-}
headValidator ::
  -- | Unique identifier for this particular Head
  -- TODO: currently unused
  MintingPolicyHash ->
  -- | Commit script address. NOTE: Used to identify inputs from commits and
  -- likely could be replaced by looking for PTs.
  Address ->
  State ->
  Input ->
  ScriptContext ->
  Bool
headValidator _ commitAddress oldState input context =
  case (oldState, input) of
    (Initial{parties}, CollectCom{}) ->
      let collectedValue =
            foldr
              ( \TxInInfo{txInInfoResolved} val ->
                  if txOutAddress txInInfoResolved == commitAddress
                    then val + txOutValue txInInfoResolved
                    else val
              )
              mempty
              txInfoInputs
          headInputValue = maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput context

          collectedUtxo :: [Utxo] =
            reverse $
              foldr
                ( \TxInInfo{txInInfoResolved} utxos ->
                    if txOutAddress txInInfoResolved == commitAddress
                      then maybe (traceError "could not decode commit") (: utxos) $ do
                        dh <- txOutDatumHash txInInfoResolved
                        d <- getDatum <$> findDatum dh txInfo
                        case fromBuiltinData d of
                          Nothing -> traceError "fromBuiltinData failed"
                          Just ((_p, u) :: DatumType Commit) -> pure u
                      else utxos
                )
                mempty
                txInfoInputs
          utxoHash = hashPreSerializedCommits collectedUtxo
          expectedDatum = Open{parties, utxoHash}
       in case findContinuingOutputs context of
            [ix] ->
              let headOutput = txInfoOutputs !! ix
                  headOutputValue = txOutValue headOutput

                  checkOutputValue =
                    traceIfFalse "committed value is not preserved in head" $
                      headOutputValue == collectedValue <> headInputValue

                  checkOutputDatumError =
                    "unexpected output datum in collectCom, expected utxo hash: "
                      `appendByteString` utxoHash
                      `appendByteString` ", actual utxo hash: "
                      `appendByteString` actualUtxoHash

                  actualUtxoHash = fromMaybe "couldn't find actual hash?" $ do
                    headOutputDatumHash <- txOutDatumHash headOutput
                    actualDatum <- findDatum headOutputDatumHash txInfo
                    Open{utxoHash = actual} <- fromBuiltinData (getDatum actualDatum)
                    pure actual

                  checkOutputDatum =
                    traceIfFalse (decodeUtf8 checkOutputDatumError) $
                      fromMaybe False $ do
                        headOutputDatumHash <- txOutDatumHash headOutput
                        actualDatum <- findDatum headOutputDatumHash txInfo
                        pure (actualDatum == Datum (toBuiltinData expectedDatum))
               in checkOutputValue && checkOutputDatum
            [] -> traceIfFalse "No continuing head output" False
            _ -> traceIfFalse "More than one continuing head output" False
    (Initial{}, Abort) -> True
    (Open{parties}, Close{snapshotNumber, signature})
      | snapshotNumber == 0 -> True
      | snapshotNumber > 0 -> verifySnapshotSignature parties snapshotNumber signature
      | otherwise -> False
    (Closed{utxoHash}, Fanout{numberOfFanoutOutputs}) ->
      traceIfFalse "fannedOutUtxoHash /= closedUtxoHash" $ fannedOutUtxoHash numberOfFanoutOutputs == utxoHash
    _ -> False
 where
  fannedOutUtxoHash numberOfFanoutOutputs = hashTxOuts $ take numberOfFanoutOutputs txInfoOutputs

  TxInfo{txInfoInputs, txInfoOutputs} = txInfo

  ScriptContext{scriptContextTxInfo = txInfo} = context

hashPreSerializedCommits :: [Utxo] -> BuiltinByteString
hashPreSerializedCommits u =
  sha2_256 . encodingToBuiltinByteString $
    encodeBeginList <> foldMap (\(Utxo bytes) -> encodeRaw bytes) u <> encodeBreak
{-# INLINEABLE hashPreSerializedCommits #-}

hashTxOuts :: [TxOut] -> BuiltinByteString
hashTxOuts =
  sha2_256 . serialiseTxOuts
{-# INLINEABLE hashTxOuts #-}

{-# INLINEABLE verifySnapshotSignature #-}
verifySnapshotSignature :: [Party] -> SnapshotNumber -> [Signature] -> Bool
verifySnapshotSignature parties snapshotNumber sigs =
  traceIfFalse "signature verification failed" $
    length parties == length sigs
      && all (uncurry $ verifyPartySignature snapshotNumber) (zip parties sigs)

{-# INLINEABLE verifyPartySignature #-}
verifyPartySignature :: SnapshotNumber -> Party -> Signature -> Bool
verifyPartySignature snapshotNumber vkey signed =
  traceIfFalse "party signature verification failed" $
    mockVerifySignature vkey snapshotNumber (getSignature signed)

{-# INLINEABLE mockVerifySignature #-}
-- TODO: This really should be the builtin Plutus function 'verifySignature' but as we
-- are using Mock crypto in the Head, so must we use Mock crypto on-chain to verify
-- signatures.
mockVerifySignature :: Party -> SnapshotNumber -> BuiltinByteString -> Bool
mockVerifySignature (UnsafeParty vkey) snapshotNumber signed =
  traceIfFalse "mock signed message is not equal to signed" $
    mockSign vkey (encodingToBuiltinByteString $ encodeInteger snapshotNumber) == signed

{-# INLINEABLE mockSign #-}
mockSign :: Integer -> BuiltinByteString -> BuiltinByteString
mockSign vkey msg = appendByteString (sliceByteString 0 8 hashedMsg) (encodingToBuiltinByteString $ encodeInteger vkey)
 where
  hashedMsg = sha2_256 msg

-- | The script instance of the auction state machine. It contains the state
-- machine compiled to a Plutus core validator script. The 'MintingPolicyHash' serves
-- two roles here:
--
--   1. Parameterizing the script, such that we get a unique address and allow
--   for multiple instances of it
--
--   2. Identify the 'state thread token', which should be passed in
--   transactions transitioning the state machine and provide "contract
--   continuity"
--
-- TODO: Add a NetworkId so that we can properly serialise address hashes
-- see 'encodeAddress' for details
typedValidator :: MintingPolicyHash -> TypedValidator Head
typedValidator policyId =
  Scripts.mkTypedValidator @Head
    compiledValidator
    $$(PlutusTx.compile [||wrap||])
 where
  compiledValidator =
    $$(PlutusTx.compile [||headValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode policyId
      `PlutusTx.applyCode` PlutusTx.liftCode Commit.address

  wrap = Scripts.wrapValidator @(DatumType Head) @(RedeemerType Head)

validatorHash :: MintingPolicyHash -> ValidatorHash
validatorHash = Scripts.validatorHash . typedValidator

address :: MintingPolicyHash -> Address
address = scriptHashAddress . validatorHash

-- | Get the actual plutus script. Mainly used to serialize and use in
-- transactions.
validatorScript :: MintingPolicyHash -> Script
validatorScript = unValidatorScript . Scripts.validatorScript . typedValidator
