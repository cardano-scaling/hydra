{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | The validator used to collect & open or abort a Head.
module Hydra.Contract.Commit where

import Ledger hiding (validatorHash)
import PlutusTx.Prelude

import Hydra.Contract.HeadState (State (..))
import Hydra.Data.Party (Party)
import Ledger.Typed.Scripts (TypedValidator, ValidatorType, ValidatorTypes (..))
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.V1.Ledger.Api (Credential (ScriptCredential))
import PlutusTx (CompiledCode)
import qualified PlutusTx
import PlutusTx.IsData.Class (FromData (fromBuiltinData), ToData (..))

data Commit

newtype SerializedTxOutRef = SerializedTxOutRef BuiltinByteString
PlutusTx.unstableMakeIsData ''SerializedTxOutRef

newtype SerializedTxOut = SerializedTxOut BuiltinByteString
PlutusTx.unstableMakeIsData ''SerializedTxOut

-- TODO: Having the 'TxOutRef' on-chain is not necessary but it is convenient
-- for the off-chain code to reconstrut the commit UTXO.
--
-- Ideally, since the TxOutRef is already present in the redeemer for the
-- initial validator, the off-chain code could get it from there.
instance Scripts.ValidatorTypes Commit where
  type DatumType Commit = (Party, ValidatorHash, Maybe (SerializedTxOutRef, SerializedTxOut))
  type RedeemerType Commit = ()

validator :: DatumType Commit -> RedeemerType Commit -> ScriptContext -> Bool
validator (_party, headScriptHash, commit) () ScriptContext{scriptContextTxInfo = txInfo} =
  case commit of
    -- we don't commit anything, so there's nothing to validate
    Nothing -> True
    -- NOTE: we could check the committed txOut is present in the Head output hash, for
    -- example by providing some proof in the redeemer and checking that but this is redundant
    -- with what the Head script is already doing so it's enough to check that the Head script
    -- is actually running in the correct "branch" (eg. handling a `CollectCom` or `Abort`
    -- redeemer)
    -- However we can't get the redeemer for another input so we'll need to check the datum
    -- is `Initial`
    Just _ ->
      case txInInfoResolved <$> findHeadScript of
        Nothing -> traceError "Cannot find Head script"
        Just (TxOut _ _ (Just dh)) ->
          case getDatum <$> findDatum dh txInfo of
            Nothing -> traceError "Invalid datum hash with no datum"
            (Just da) ->
              case fromBuiltinData @State da of
                Just Initial{} -> True
                _ -> traceError "Head script in wrong state"
        Just (TxOut _ _ Nothing) -> traceError "Head script has no datum hash"
 where
  findHeadScript = find (paytoHeadScript . txInInfoResolved) $ txInfoInputs txInfo

  paytoHeadScript = \case
    TxOut{txOutAddress = Address (ScriptCredential s) _} -> s == headScriptHash
    _ -> False

{- ORMOLU_DISABLE -}
typedValidator :: TypedValidator Commit
typedValidator = Scripts.mkTypedValidator @Commit
  compiledValidator
  $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Scripts.wrapValidator @(DatumType Commit) @(RedeemerType Commit)
{- ORMOLU_ENABLE -}

compiledValidator :: CompiledCode (ValidatorType Commit)
compiledValidator = $$(PlutusTx.compile [||validator||])

-- | Get the actual plutus script. Mainly used to serialize and use in
-- transactions.
validatorScript :: Script
validatorScript = unValidatorScript $ Scripts.validatorScript typedValidator

validatorHash :: ValidatorHash
validatorHash = Scripts.validatorHash typedValidator

address :: Address
address = scriptHashAddress validatorHash

datum :: DatumType Commit -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: RedeemerType Commit
redeemer = ()
