{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | The validator used to collect & open or abort a Head.
module Hydra.Contract.Commit where

import PlutusTx.Prelude

import Codec.Serialise (deserialiseOrFail, serialise)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Hydra.Cardano.Api (CtxUTxO, fromPlutusTxOut, fromPlutusTxOutRef, toPlutusTxOut, toPlutusTxOutRef)
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
  TxOutRef,
  Validator (getValidator),
  ValidatorHash,
  mkValidatorScript,
 )
import PlutusTx (CompiledCode, fromData, toBuiltinData, toData)
import qualified PlutusTx
import qualified PlutusTx.Builtins as Builtins
import qualified Prelude as Haskell

data CommitRedeemer
  = ViaCollectCom
  | ViaAbort

PlutusTx.unstableMakeIsData ''CommitRedeemer

-- | A data type representing comitted outputs on-chain. Besides recording the
-- original 'TxOutRef', it also stores a binary representation compatible
-- between on- and off-chain code to be hashed in the validators.
data Commit = Commit
  { input :: TxOutRef
  , preSerializedOutput :: BuiltinByteString
  }
  deriving (Haskell.Eq, Haskell.Show, Haskell.Ord)

instance Eq Commit where
  (Commit i o) == (Commit i' o') =
    i == i' && o == o'

PlutusTx.unstableMakeIsData ''Commit

-- | Record an off-chain 'TxOut' as a 'Commit' on-chain.
-- NOTE: Depends on the 'Serialise' instance for Plutus' 'Data'.
serializeCommit :: (OffChain.TxIn, OffChain.TxOut CtxUTxO) -> Maybe Commit
serializeCommit (i, o) = do
  preSerializedOutput <- toBuiltin . toStrict . serialise . toData <$> toPlutusTxOut o
  pure
    Commit
      { input = toPlutusTxOutRef i
      , preSerializedOutput
      }

-- | Decode an on-chain 'SerializedTxOut' back into an off-chain 'TxOut'.
-- NOTE: Depends on the 'Serialise' instance for Plutus' 'Data'.
deserializeCommit :: Commit -> Maybe (OffChain.TxIn, OffChain.TxOut CtxUTxO)
deserializeCommit Commit{input, preSerializedOutput} =
  case deserialiseOrFail . fromStrict $ fromBuiltin preSerializedOutput of
    Left{} -> Nothing
    Right dat -> do
      txOut <- fromPlutusTxOut network <$> fromData dat
      pure (fromPlutusTxOutRef input, txOut)
 where
  -- FIXME: not hard-code this
  network = Testnet

-- TODO: Party is not used on-chain but is needed off-chain while it's still
-- based on mock crypto. When we move to real crypto we could simply use
-- the PT's token name to identify the committing party
type DatumType = (Party, ValidatorHash, Maybe Commit)
type RedeemerType = CommitRedeemer

-- | The v_commit validator verifies that:
--
--   * spent in a transaction also consuming a v_head output
--
--   * on abort, redistribute comitted utxo
validator :: DatumType -> RedeemerType -> ScriptContext -> Bool
validator (_party, headScriptHash, commit) consumer ScriptContext{scriptContextTxInfo = txInfo} =
  case txInInfoResolved <$> findHeadScript of
    Nothing -> traceError "Cannot find Head script"
    Just (TxOut _ _ d _) ->
      case d of
        NoOutputDatum -> traceError "missing datum"
        OutputDatumHash{} -> traceError "expected inline datum, not hash only"
        OutputDatum da ->
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
                ViaAbort ->
                  case commit of
                    Nothing -> True
                    Just Commit{preSerializedOutput} ->
                      -- There should be an output in the transaction corresponding to this serialisedTxOut
                      traceIfFalse "cannot find commit output" $
                        preSerializedOutput `elem` (Builtins.serialiseData . toBuiltinData <$> txInfoOutputs txInfo)
                -- NOTE: In the Collectcom case the inclusion of the committed output 'commit' is
                -- delegated to the 'CollectCom' script who has more information to do it.
                ViaCollectCom -> True
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
