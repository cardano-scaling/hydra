{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize -Wwarn #-}

-- | The validator used to collect & open or abort a Head.
module Hydra.Contract.Commit where

import PlutusTx.Prelude

import Codec.Serialise (deserialiseOrFail, serialise)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Hydra.Cardano.Api (CtxUTxO, fromPlutusTxOut, fromPlutusTxOutRef, toPlutusTxOut, toPlutusTxOutRef)
import qualified Hydra.Cardano.Api as OffChain
import Hydra.Cardano.Api.Network (Network (Testnet))
import Hydra.Contract.Util (hasST, mustBurnST)
import Hydra.Data.Party (Party)
import Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  Datum (..),
  Redeemer (Redeemer),
  Script,
  ScriptContext (ScriptContext, scriptContextTxInfo),
  TxInfo (txInfoMint, txInfoOutputs),
  TxOutRef,
  Validator (getValidator),
  ValidatorHash,
  mkValidatorScript,
  txOutValue,
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
type DatumType = (Party, ValidatorHash, Maybe Commit, CurrencySymbol)
type RedeemerType = CommitRedeemer

-- | The v_commit validator verifies that:
--
--   * spent in a transaction also consuming a v_head output
validator :: DatumType -> RedeemerType -> ScriptContext -> Bool
validator (_party, _headScriptHash, commit, headId) r ctx@ScriptContext{scriptContextTxInfo = txInfo} =
  case r of
    ViaAbort ->
      traceIfFalse "ST not burned" (mustBurnST (txInfoMint $ scriptContextTxInfo ctx) headId)
    --  && case commit of
    --    Nothing -> True
    --    Just Commit{preSerializedOutput} ->
    --      traceIfFalse
    --        "cannot find committed output"
    --        -- There should be an output in the transaction corresponding to this preSerializedOutput
    --        (preSerializedOutput `elem` (Builtins.serialiseData . toBuiltinData <$> txInfoOutputs txInfo))
    -- NOTE: In the Collectcom case the inclusion of the committed output 'commit' is
    -- delegated to the 'CollectCom' script who has more information to do it.
    ViaCollectCom ->
      traceIfFalse "ST is missing in the output" (hasST headId outputs)
 where
  outputs = foldMap txOutValue $ txInfoOutputs $ scriptContextTxInfo ctx

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
