{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.Tx.Accumulator where

import Hydra.Prelude

import Accumulator (Accumulator)
import Accumulator qualified
import Bindings (getPolyCommitOverG2)
import Cardano.Crypto.EllipticCurve.BLS12_381 (Point2)
import Codec.Serialise (serialise)
import Data.Aeson.Types (Parser)
import Data.ByteString.Base16 qualified as Base16
import Data.Map qualified as Map
import Data.Text.Encoding qualified as T
import Hydra.Cardano.Api (
  CtxUTxO,
  TxIn,
  TxOut,
  toPlutusTxOut,
  toPlutusTxOutRef,
 )
import Hydra.Tx.IsTx (IsTx (..))
import PlutusTx (toData)

-- * HydraAccumulator

newtype HydraAccumulator = HydraAccumulator {unHydraAccumulator :: Accumulator}
  deriving newtype (Eq, Show)

build :: [ByteString] -> HydraAccumulator
build = HydraAccumulator . Accumulator.buildAccumulator

instance ToJSON HydraAccumulator where
  toJSON (HydraAccumulator acc) =
    toJSON
      . Map.mapKeys (T.decodeUtf8 . Base16.encode)
      . fmap (first (T.decodeUtf8 . Base16.encode))
      $ acc

instance FromJSON HydraAccumulator where
  parseJSON value =
    parseJSON value >>= \(m :: Map Text (Text, Int)) ->
      let textToBs :: Text -> Parser ByteString
          textToBs t =
            case Base16.decode (T.encodeUtf8 t) of
              Left e -> fail $ "invalid base16: " <> e
              Right bs -> pure bs
       in HydraAccumulator
            <$> ( Map.fromList
                    <$> forM
                      (Map.toList m)
                      ( \(k, (v, i)) -> do
                          k' <- textToBs k
                          v' <- textToBs v
                          pure (k', (v', i))
                      )
                )

-- * Accumulator functions

-- | Create a cryptographic accumulator from a UTxO set.
--
-- This is a pure function which will return the accumulator's root.
makeHeadAccumulator :: forall tx. IsTx tx => UTxOType tx -> IO Point2
makeHeadAccumulator u = do
  let allElements = utxoToElement <$> toPairList u
  let accumulator = Accumulator.buildAccumulator allElements
  -- NOTE: The 'crs' is a placeholder for the actual CRS
  let crs = [] :: [Point2]
  eRoot <- getPolyCommitOverG2 allElements accumulator crs
  case eRoot of
    Left err -> error $ "Failed to create accumulator root: " <> toText err
    Right root -> pure root

-- | The canonical way to serialize a UTxO element for the accumulator.
--
-- The serialization is based on the Plutus `ToData` instances for a `TxOutRef`
-- and a `TxOut`.
utxoToElement :: (TxIn, TxOut CtxUTxO) -> ByteString
utxoToElement (txIn, txOut) =
  toStrict (serialise $ toData $ toPlutusTxOutRef txIn) <> toStrict (serialise $ toData $ toPlutusTxOut txOut)
