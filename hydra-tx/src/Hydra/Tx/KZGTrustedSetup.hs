{-# LANGUAGE TemplateHaskell #-}

module Hydra.Tx.KZGTrustedSetup (
  g1Points,
  g2Points,
  g1BuiltinPoints,
  g2BuiltinPoints,
  maxAccumulatorSize,
  maxFanoutBatchSize,
) where

import Hydra.Prelude

import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Point1, Point2, blsUncompress)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseEither, withObject, (.:))
import Data.ByteString.Base16 qualified as Base16
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text qualified as T
import PlutusTx.Builtins (BuiltinBLS12_381_G1_Element, BuiltinBLS12_381_G2_Element, bls12_381_G1_uncompress, bls12_381_G2_uncompress, toBuiltin)

-- | Maximum accumulator element count supported by the currently embedded G1 CRS.
-- The JSON setup provides 4096 G1 monomial points [G1, τ·G1, ..., τ^4095·G1],
-- so the accumulator supports up to 4095 elements.
maxAccumulatorSize :: Int
maxAccumulatorSize = length g1Points - 1

-- | Maximum number of UTxOs that can be fanned out in a single partial fanout transaction.
-- The on-chain verification CRS uses G2 points. The EIP-4844 ceremony provides 65 G2 points,
-- so each fanout batch is limited to 64 outputs.
maxFanoutBatchSize :: Int
maxFanoutBatchSize = length g2Points - 1

-- | G1 and G2 hex strings parsed from the embedded JSON.
--
-- Source: https://github.com/ethereum/kzg-ceremony-verifier/tree/master/output_setups
-- We use @g1_monomial@ (not @g1_lagrange@): the monomial form [G1, τ·G1, ...] is required
-- for KZG polynomial commitments. The @g1_lagrange@ form in @trusted_setup.txt@ (used by
-- Ethereum clients for blob commitments) is NOT suitable for the accumulator scheme here.
embeddedSetup :: ([Text], [Text])
embeddedSetup =
  case Aeson.decodeStrict $(makeRelativeToProject "trusted_setup.json" >>= embedFile) of
    Nothing -> error "KZGTrustedSetup: failed to parse trusted_setup.json"
    Just v ->
      case parseEither (withObject "TrustedSetup" parse) v of
        Left err -> error $ "KZGTrustedSetup: " <> toText err
        Right result -> result
 where
  parse :: Aeson.Object -> Parser ([Text], [Text])
  parse obj = (,) <$> obj .: "g1_monomial" <*> obj .: "g2_monomial"

decodeHexPoint :: Text -> ByteString
decodeHexPoint t =
  case Base16.decode $ encodeUtf8 $ fromMaybe t (T.stripPrefix "0x" t) of
    Left err -> error $ "KZGTrustedSetup: invalid hex: " <> toText err
    Right bs -> bs

-- | G1 powers of tau [G1, τ·G1, ..., τ^4095·G1] from the EIP-4844 ceremony (monomial form).
-- 4096 points; used off-chain for accumulator and membership proof commitments.
g1Points :: [Point1]
g1Points = map parseG1 (fst embeddedSetup)
 where
  parseG1 :: Text -> Point1
  parseG1 hex = case blsUncompress (decodeHexPoint hex) of
    Left _ -> error "KZGTrustedSetup: invalid G1 point in trusted_setup.json"
    Right p -> p

-- | G2 powers of tau [G2, τ·G2, ..., τ^64·G2] from the EIP-4844 ceremony (monomial form).
-- 65 points; used on-chain as the verification CRS (limits batch fanout to 64 outputs).
g2Points :: [Point2]
g2Points = map parseG2 (snd embeddedSetup)
 where
  parseG2 :: Text -> Point2
  parseG2 hex = case blsUncompress (decodeHexPoint hex) of
    Left _ -> error "KZGTrustedSetup: invalid G2 point in trusted_setup.json"
    Right p -> p

-- | G1 points as Plutus built-in type, for use in off-chain accumulator commitment.
g1BuiltinPoints :: [BuiltinBLS12_381_G1_Element]
g1BuiltinPoints =
  map (bls12_381_G1_uncompress . toBuiltin . decodeHexPoint) (fst embeddedSetup)

-- | G2 points as Plutus built-in type, for use in on-chain verification CRS.
g2BuiltinPoints :: [BuiltinBLS12_381_G2_Element]
g2BuiltinPoints =
  map (bls12_381_G2_uncompress . toBuiltin . decodeHexPoint) (snd embeddedSetup)
