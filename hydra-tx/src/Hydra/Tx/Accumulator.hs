module Hydra.Tx.Accumulator where

import Hydra.Prelude

import Accumulator qualified
import Bindings (getPolyCommitOverG2)
import Cardano.Api.UTxO qualified as UTxO
import Cardano.Crypto.EllipticCurve.BLS12_381 (Point2)
import Codec.Serialise (serialise)
import Hydra.Cardano.Api (
  CtxUTxO,
  TxIn,
  TxOut,
  UTxO,
  toPlutusTxOut,
  toPlutusTxOutRef,
 )
import PlutusTx (toData)

-- | Create a cryptographic accumulator from a UTxO set.
--
-- This is a pure function which will return the accumulator's root.
makeHeadAccumulator :: UTxO -> IO Point2
makeHeadAccumulator u = do
  let allElements = utxoToElement <$> UTxO.toList u
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
