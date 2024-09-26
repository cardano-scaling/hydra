{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
-- Plutus core version to compile to. In babbage era, that is Cardano protocol
-- version 7 and 8, only plutus-core version 1.0.0 is available.
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | The validator used to collect & open or abort a Head.
module Hydra.Contract.Commit where

import PlutusTx.Prelude

import Codec.Serialise (deserialiseOrFail, serialise)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Hydra.Cardano.Api (CtxUTxO, PlutusScriptVersion (PlutusScriptV2), fromPlutusTxOut, fromPlutusTxOutRef, toPlutusTxOut, toPlutusTxOutRef)
import Hydra.Cardano.Api qualified as OffChain
import Hydra.Cardano.Api.Network (Network)
import Hydra.Contract.CommitError (CommitError (..), errorCode)
import Hydra.Contract.Util (hasST, mustBurnST)
import Hydra.Data.Party (Party)
import Hydra.Plutus.Extras (ValidatorType, scriptValidatorHash)
import Hydra.ScriptContext (ScriptContext (..), TxInfo (..))
import Plutus.Script.Utils.Typed (mkUntypedValidator)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  Datum (..),
  Redeemer (Redeemer),
  ScriptHash,
  SerialisedScript,
  TxOutRef,
  serialiseCompiledCode,
  txOutValue,
 )
import PlutusTx (CompiledCode, fromData, toBuiltinData, toData)
import PlutusTx qualified
import Prelude qualified as Haskell

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
  deriving stock (Haskell.Eq, Haskell.Show, Haskell.Ord)

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
deserializeCommit :: Network -> Commit -> Maybe (OffChain.TxIn, OffChain.TxOut CtxUTxO)
deserializeCommit network Commit{input, preSerializedOutput} =
  case deserialiseOrFail . fromStrict $ fromBuiltin preSerializedOutput of
    Left{} -> Nothing
    Right dat -> do
      txOut <- fromPlutusTxOut network =<< fromData dat
      pure (fromPlutusTxOutRef input, txOut)

-- TODO: Party is not used on-chain but is needed off-chain while it's still
-- based on mock crypto. When we move to real crypto we could simply use
-- the PT's token name to identify the committing party
type DatumType = (Party, [Commit], CurrencySymbol)
type RedeemerType = CommitRedeemer

-- | The v_commit validator verifies that:
--
--   * spent in a transaction also consuming a v_head output
--
--   * ST is burned if the redeemer is 'ViaAbort'
--
--   * ST is present in the output if the redeemer is 'ViaCollectCom'
validator :: DatumType -> RedeemerType -> ScriptContext -> Bool
validator (_party, _commit, headId) r ctx =
  case r of
    -- NOTE: The reimbursement of the committed output 'commit' is
    -- delegated to the 'head' script who has more information to do it.
    ViaAbort ->
      traceIfFalse
        $(errorCode STNotBurnedError)
        (mustBurnST (txInfoMint $ scriptContextTxInfo ctx) headId)
    ViaCollectCom ->
      traceIfFalse
        $(errorCode STIsMissingInTheOutput)
        (hasST headId headOutputValue)
 where
  headOutputValue =
    txOutValue . head $ txInfoOutputs (scriptContextTxInfo ctx)

compiledValidator :: CompiledCode ValidatorType
compiledValidator =
  $$(PlutusTx.compile [||wrap validator||])
 where
  wrap = mkUntypedValidator @ScriptContext @DatumType @RedeemerType

validatorScript :: SerialisedScript
validatorScript = serialiseCompiledCode compiledValidator

validatorHash :: ScriptHash
validatorHash = scriptValidatorHash PlutusScriptV2 validatorScript

datum :: DatumType -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: RedeemerType -> Redeemer
redeemer a = Redeemer (toBuiltinData a)
