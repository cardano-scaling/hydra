{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | The validator used to collect & open or abort a Head.
module Hydra.Contract.Commit where

import PlutusTx.Prelude

import Hydra.Contract.Encoding (encodeTxOut)
import Hydra.Contract.HeadState (State (..))
import Hydra.Data.Party (Party)
import Plutus.Codec.CBOR.Encoding (encodingToBuiltinByteString)
import Plutus.V1.Ledger.Api (
  Address (Address),
  Credential (ScriptCredential),
  Datum (..),
  FromData (fromBuiltinData),
  Redeemer (Redeemer),
  Script,
  ScriptContext (ScriptContext, scriptContextTxInfo),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoInputs, txInfoOutputs),
  TxOut (TxOut, txOutAddress),
  Validator (getValidator),
  ValidatorHash,
  mkValidatorScript,
 )
import Plutus.V1.Ledger.Contexts (findDatum)
import PlutusTx (toBuiltinData)
import qualified PlutusTx
import Test.Plutus.Validator (wrapValidator)

data CommitRedeemer = CollectCom | Abort

PlutusTx.unstableMakeIsData ''CommitRedeemer

newtype SerializedTxOut = SerializedTxOut BuiltinByteString
PlutusTx.unstableMakeIsData ''SerializedTxOut

instance Eq SerializedTxOut where
  SerializedTxOut bs == SerializedTxOut bs' = bs == bs'

-- TODO: Party is not used on-chain but is needed off-chain while it's still
-- based on mock crypto. When we move to real crypto we could simply use
-- the PT's token name to identify the committing party
type DatumType = (Party, ValidatorHash, Maybe SerializedTxOut)
type RedeemerType = CommitRedeemer

validator :: DatumType -> RedeemerType -> ScriptContext -> Bool
validator (_party, headScriptHash, commit) consumer ScriptContext{scriptContextTxInfo = txInfo} =
  case txInInfoResolved <$> findHeadScript of
    Nothing -> traceError "Cannot find Head script"
    Just (TxOut _ _ (Just dh)) ->
      case getDatum <$> findDatum dh txInfo of
        Nothing -> traceError "Invalid datum hash with no datum"
        (Just da) ->
          case fromBuiltinData @State da of
            -- NOTE: we could check the committed txOut is present in the Head output hash, for
            -- example by providing some proof in the redeemer and checking that but this is redundant
            -- with what the Head script is already doing so it's enough to check that the Head script
            -- is actually running in the correct "branch" (eg. handling a `CollectCom` or `Abort`
            -- redeemer)
            -- However we can't get the redeemer for another input so we'll need to check the datum
            -- is `Initial`
            Just Initial{} ->
              case consumer of
                Abort ->
                  case commit of
                    Nothing -> True
                    Just (SerializedTxOut serialisedTxOut) ->
                      -- There should be an output in the transaction corresponding to this serialisedTxOut
                      traceIfFalse "cannot find commit output" $
                        serialisedTxOut `elem` (encodingToBuiltinByteString . encodeTxOut <$> txInfoOutputs txInfo)
                -- NOTE: In the Collectcom case the inclusion of the committed output 'commit' is
                -- delegated to the 'CollectCom' script who has more information to do it.
                CollectCom -> True
            _ -> traceError "Head script in wrong state"
    Just (TxOut _ _ Nothing) -> traceError "Head script has no datum hash"
 where
  findHeadScript = find (paytoHeadScript . txInInfoResolved) $ txInfoInputs txInfo

  paytoHeadScript = \case
    TxOut{txOutAddress = Address (ScriptCredential s) _} -> s == headScriptHash
    _ -> False

compiledValidator :: Validator
compiledValidator =
  mkValidatorScript
    $$(PlutusTx.compile [||wrap validator||])
 where
  wrap = wrapValidator @DatumType @RedeemerType

-- | Get the actual plutus script. Mainly used to serialize and use in
-- transactions.
validatorScript :: Script
validatorScript = getValidator compiledValidator

datum :: DatumType -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: RedeemerType -> Redeemer
redeemer a = Redeemer (toBuiltinData a)
