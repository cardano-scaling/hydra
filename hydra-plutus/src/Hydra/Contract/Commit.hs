{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

-- | Datum and redeemer types, as well as helper functions for the commit
-- validator implemented in aiken.
module Hydra.Contract.Commit where

import "plutus-tx" PlutusTx.Prelude

import Hydra.Cardano.Api (CtxUTxO, Network, fromPlutusTxOut, fromPlutusTxOutRef, toPlutusTxOut, toPlutusTxOutRef)
import Hydra.Cardano.Api qualified as OffChain
import Hydra.Data.Party (Party)
import "bytestring" Data.ByteString.Lazy (fromStrict, toStrict)
import "plutus-ledger-api" PlutusLedgerApi.V3 (
  CurrencySymbol,
  Datum (..),
  Redeemer (Redeemer),
  TxOutRef,
 )
import "plutus-tx" PlutusTx (fromData, toData)
import "plutus-tx" PlutusTx qualified
import "serialise" Codec.Serialise (deserialiseOrFail, serialise)
import "base" Prelude qualified as Haskell

-- | A data type representing committed outputs on-chain. Besides recording the
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

data CommitRedeemer
  = ViaCollectCom
  | ViaAbort

PlutusTx.unstableMakeIsData ''CommitRedeemer

-- TODO: Note that we now have datum and the redeemer duplicated in haskell and
-- on aiken side. Idea is to remove these types from haskell land but we want to
-- wait on the plutus team to implement
-- [CIP-57](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0057)
-- which will allow us to parse the 'plutus.json' datum and redeemer definitions
-- directly to Plutus `Data` .
type RedeemerType = CommitRedeemer

datum :: DatumType -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: RedeemerType -> Redeemer
redeemer a = Redeemer (toBuiltinData a)
