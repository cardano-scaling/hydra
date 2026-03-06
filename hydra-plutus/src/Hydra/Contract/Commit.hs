{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

-- | Representation of a UTxO when committed / deposited into the Hydra Head protocol.
-- TODO: Rename/move this to Deposit
module Hydra.Contract.Commit where

import PlutusTx.Prelude

import Codec.Serialise (deserialiseOrFail, serialise)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Hydra.Cardano.Api (CtxUTxO, Network, fromPlutusTxOut, fromPlutusTxOutRef, toPlutusTxOut, toPlutusTxOutRef)
import Hydra.Cardano.Api qualified as OffChain
import PlutusLedgerApi.V3 (TxOutRef)
import PlutusTx (fromData, toData)
import PlutusTx qualified
import Prelude qualified as Haskell

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
