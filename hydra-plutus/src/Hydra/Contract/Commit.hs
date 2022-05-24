{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | The validator used to collect & open or abort a Head.
module Hydra.Contract.Commit where

import PlutusTx.Prelude

import Codec.Serialise (deserialiseOrFail, serialise)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Hydra.Cardano.Api (CtxUTxO, fromPlutusTxOut, toPlutusTxOut)
import qualified Hydra.Cardano.Api as OffChain
import Hydra.Cardano.Api.Network (Network (Testnet))
import Hydra.Contract.HeadState (State (..))
import Hydra.Data.Party (Party)
import Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import Plutus.V2.Ledger.Api (
  Address (Address),
  Credential (ScriptCredential),
  Datum (..),
  FromData (fromBuiltinData),
  OutputDatum (..),
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
import Plutus.V2.Ledger.Contexts (findDatum)
import PlutusTx (CompiledCode, fromData, toBuiltinData, toData)
import qualified PlutusTx
import qualified PlutusTx.Builtins as Builtins
import qualified Prelude as Haskell

data CommitRedeemer = CollectCom | Abort

PlutusTx.unstableMakeIsData ''CommitRedeemer

-- | A data type representing how comitted outputs are recorded on-chain. The
-- format needs to be binary compatible between on- and off-chain code as we use
-- hashes over these in validators.
newtype SerializedTxOut = SerializedTxOut BuiltinByteString
  deriving newtype (Eq, Haskell.Eq, Haskell.Show, Haskell.Ord)

PlutusTx.unstableMakeIsData ''SerializedTxOut

-- | Record an off-chain 'TxOut' as a 'SerializedTxOut' on-chain.
-- NOTE: Depends on the 'Serialise' instance for Plutus' 'Data'.
serializeTxOut :: OffChain.TxOut CtxUTxO -> Maybe SerializedTxOut
serializeTxOut o =
  SerializedTxOut . toBuiltin . toStrict . serialise . toData <$> toPlutusTxOut o

-- | Decode an on-chain 'SerializedTxOut' back into an off-chain 'TxOut'.
-- NOTE: Depends on the 'Serialise' instance for Plutus' 'Data'.
deserializeTxOut :: SerializedTxOut -> Maybe (OffChain.TxOut CtxUTxO)
deserializeTxOut (SerializedTxOut bs) =
  case deserialiseOrFail . fromStrict $ fromBuiltin bs of
    Left{} -> Nothing
    Right dat -> fromPlutusTxOut network <$> fromData dat
 where
  -- FIXME: not hard-code this
  network = Testnet

-- TODO: Party is not used on-chain but is needed off-chain while it's still
-- based on mock crypto. When we move to real crypto we could simply use
-- the PT's token name to identify the committing party
type DatumType = (Party, ValidatorHash, Maybe SerializedTxOut)
type RedeemerType = CommitRedeemer

validator :: DatumType -> RedeemerType -> ScriptContext -> Bool
validator (_party, headScriptHash, commit) consumer ScriptContext{scriptContextTxInfo = txInfo} =
  case txInInfoResolved <$> findHeadScript of
    Nothing -> traceError "Cannot find Head script"
    Just (TxOut _ _ d _) ->
      case d of
        NoOutputDatum -> traceError "missing datum"
        OutputDatum _ -> traceError "unexpected inline datum"
        OutputDatumHash dh ->
          case findDatum dh txInfo of
            Nothing -> traceError "could not find datum"
            Just da ->
              case fromBuiltinData @State $ getDatum da of
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
                            serialisedTxOut `elem` (Builtins.serialiseData . toBuiltinData <$> txInfoOutputs txInfo)
                    -- NOTE: In the Collectcom case the inclusion of the committed output 'commit' is
                    -- delegated to the 'CollectCom' script who has more information to do it.
                    CollectCom -> True
                _ -> traceError "Head script in wrong state"
 where
  findHeadScript = find (paytoHeadScript . txInInfoResolved) $ txInfoInputs txInfo

  paytoHeadScript = \case
    TxOut{txOutAddress = Address (ScriptCredential s) _} -> s == headScriptHash
    _ -> False

compiledValidator :: CompiledCode ValidatorType
compiledValidator =
  $$(PlutusTx.compile [||wrap validator||])
 where
  wrap = wrapValidator @DatumType @RedeemerType

validatorScript :: Script
validatorScript = getValidator $ mkValidatorScript compiledValidator

validatorHash :: ValidatorHash
validatorHash = scriptValidatorHash validatorScript

datum :: DatumType -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: RedeemerType -> Redeemer
redeemer a = Redeemer (toBuiltinData a)
