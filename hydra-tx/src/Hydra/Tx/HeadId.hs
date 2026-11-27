{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.HeadId (
  module Hydra.Tx.HeadId,
  module Hydra.Tx.HeadId.Types,
  AsType (AsHeadId, AsHeadSeed),
) where

import Data.Aeson qualified as Aeson
import Hydra.Cardano.Api (HasTypeProxy (..), PolicyId, SerialiseAsRawBytes (..), TxIn, fromPlutusCurrencySymbol)
import Hydra.Prelude
import Hydra.Tx.HeadId.Types
import PlutusLedgerApi.V3 (CurrencySymbol (..), toBuiltin)

currencySymbolToHeadId :: MonadFail m => CurrencySymbol -> m HeadId
currencySymbolToHeadId = fmap mkHeadId . fromPlutusCurrencySymbol

headIdToPolicyId :: MonadFail m => HeadId -> m PolicyId
headIdToPolicyId = fromPlutusCurrencySymbol . headIdToCurrencySymbol

headIdToCurrencySymbol :: HeadId -> CurrencySymbol
headIdToCurrencySymbol (UnsafeHeadId headId) = CurrencySymbol (toBuiltin headId)

mkHeadId :: PolicyId -> HeadId
mkHeadId = UnsafeHeadId . serialiseToRawBytes

headSeedToTxIn :: MonadFail m => HeadSeed -> m TxIn
headSeedToTxIn (UnsafeHeadSeed bytes) =
  case Aeson.decodeStrict bytes of
    Nothing -> fail $ "Failed to decode HeadSeed " <> show bytes
    Just txIn -> pure txIn

txInToHeadSeed :: TxIn -> HeadSeed
txInToHeadSeed txin = UnsafeHeadSeed $ toStrict $ Aeson.encode txin

instance SerialiseAsRawBytes HeadId where
  serialiseToRawBytes (UnsafeHeadId bytes) = bytes
  deserialiseFromRawBytes _ = Right . UnsafeHeadId

instance HasTypeProxy HeadId where
  data AsType HeadId = AsHeadId
  proxyToAsType _ = AsHeadId

instance SerialiseAsRawBytes HeadSeed where
  serialiseToRawBytes (UnsafeHeadSeed bytes) = bytes
  deserialiseFromRawBytes _ = Right . UnsafeHeadSeed

instance HasTypeProxy HeadSeed where
  data AsType HeadSeed = AsHeadSeed
  proxyToAsType _ = AsHeadSeed
