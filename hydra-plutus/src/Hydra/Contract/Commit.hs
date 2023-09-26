{-# LANGUAGE TemplateHaskell #-}

-- | The validator used to collect & open or abort a Head.
module Hydra.Contract.Commit where

import PlutusTx.Prelude

import Codec.Serialise (deserialiseOrFail, serialise)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Hydra.Cardano.Api (CtxUTxO, fromPlutusTxOut, fromPlutusTxOutRef, toPlutusTxOut, toPlutusTxOutRef)
import Hydra.Cardano.Api qualified as OffChain
import Hydra.Cardano.Api.Network (Network)
import Hydra.Data.Party (Party)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  Datum (..),
  Redeemer (Redeemer),
  TxOutRef,
 )
import PlutusTx (fromData, toBuiltinData, toData)
import PlutusTx qualified
import Prelude qualified as Haskell

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

data CommitRedeemer
  = ViaCollectCom
  | ViaAbort

PlutusTx.unstableMakeIsData ''CommitRedeemer

type RedeemerType = CommitRedeemer

datum :: DatumType -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: RedeemerType -> Redeemer
redeemer a = Redeemer (toBuiltinData a)
