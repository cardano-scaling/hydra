{-# LANGUAGE TemplateHaskell #-}

-- | KZG trusted setup parameters for Hydra's accumulator-based partial fanout.
--
-- = Background
--
-- Hydra's partial fanout uses KZG polynomial commitments to prove on-chain that
-- a distributed batch of UTxOs is a genuine subset of the UTxOs committed in the
-- Closed head datum. The accumulator polynomial encodes all snapshot UTxOs as roots:
--
-- > A(X) = ∏ (X − sᵢ)   where sᵢ = hash(TxOutᵢ)
--
-- The __accumulator commitment__ is @A(τ)·G2@ — a single G2 point stored in the
-- Closed datum. The __membership proof__ for a distributed subset @S@ is the
-- quotient @Q(X) = A(X) / P_S(X)@ committed as @Q(τ)·G2@. The on-chain validator
-- verifies the pairing identity:
--
-- > e(G1, A(τ)·G2) = e(P_S(τ)·G1, Q(τ)·G2)
--
-- where @P_S(τ)·G1@ is computed on-chain via a G1 MSM using the on-chain G1 CRS.
-- G1 scalar multiplication is roughly 2× cheaper than G2, which keeps partial
-- fanout transactions within the Cardano execution budget.
--
-- = The Trusted Setup
--
-- Computing @A(τ)·G2@ and @Q(τ)·G2@ off-chain, and @P_S(τ)·G1@ on-chain, all
-- require the same secret @τ@ (powers of tau). We embed the __EIP-4844 KZG
-- trusted setup__ produced by the Ethereum KZG ceremony (2023):
--
--   * Source: <https://github.com/ethereum/kzg-ceremony-verifier/tree/master/output_setups>
--   * Ceremony: <https://ceremony.ethereum.org/>
--   * ~140,000 independent participants; secure as long as one destroyed their secret
--   * We use the @g1_monomial@ and @g2_monomial@ keys (not @g1_lagrange@, which is
--     for Ethereum blob commitments and is unsuitable here)
--
-- The setup provides:
--
--   * 4096 G1 points @[G1, τ·G1, ..., τ^4095·G1]@ — used as the on-chain
--     verification CRS (G1 MSM to evaluate @P_S(τ)·G1@)
--   * 65 G2 points  @[G2, τ·G2, ..., τ^64·G2]@   — used off-chain to build the
--     accumulator commitment and membership proofs
--
-- = Current Limitation
--
-- Because EIP-4844 only provides 65 G2 points, the accumulator polynomial can
-- have at most 64 roots, meaning a Hydra head snapshot is currently limited to
-- __64 UTxOs__. This is enforced by 'maxAccumulatorSize'.
--
-- = Migration Path
--
-- Once Hydra runs its own KZG ceremony producing, say, 4096 G2 points, we can
-- replace @trusted_setup.json@, bump 'maxAccumulatorSize' to 4095, and
-- re-publish the script registry — no on-chain validator changes required.
-- The G2 accumulator design remains identical; only the parameter size grows.
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

-- | Maximum accumulator element count supported by the currently embedded G2 CRS.
-- The JSON setup provides 65 G2 monomial points [G2, τ·G2, ..., τ^64·G2],
-- so the accumulator supports up to 64 elements.
-- Once we have our own ceremony with more G2 points, this limit will increase.
maxAccumulatorSize :: Int
maxAccumulatorSize = length g2Points - 1

-- | Maximum number of UTxOs that can be fanned out in a single partial fanout transaction.
-- The on-chain verification CRS uses G1 points. The EIP-4844 ceremony provides 4096 G1 points,
-- so each fanout batch is limited to 4095 outputs (well above execution budget limits).
maxFanoutBatchSize :: Int
maxFanoutBatchSize = length g1Points - 1

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
-- 4096 points; used on-chain as the verification CRS for membership proofs.
g1Points :: [Point1]
g1Points = map parseG1 (fst embeddedSetup)
 where
  parseG1 :: Text -> Point1
  parseG1 hex = case blsUncompress (decodeHexPoint hex) of
    Left _ -> error "KZGTrustedSetup: invalid G1 point in trusted_setup.json"
    Right p -> p

-- | G2 powers of tau [G2, τ·G2, ..., τ^64·G2] from the EIP-4844 ceremony (monomial form).
-- 65 points; used off-chain for accumulator and membership proof commitments (limits head to 64 UTxOs).
g2Points :: [Point2]
g2Points = map parseG2 (snd embeddedSetup)
 where
  parseG2 :: Text -> Point2
  parseG2 hex = case blsUncompress (decodeHexPoint hex) of
    Left _ -> error "KZGTrustedSetup: invalid G2 point in trusted_setup.json"
    Right p -> p

-- | G1 points as Plutus built-in type, for use in on-chain verification CRS.
g1BuiltinPoints :: [BuiltinBLS12_381_G1_Element]
g1BuiltinPoints =
  map (bls12_381_G1_uncompress . toBuiltin . decodeHexPoint) (fst embeddedSetup)

-- | G2 points as Plutus built-in type, for use in off-chain accumulator commitment.
g2BuiltinPoints :: [BuiltinBLS12_381_G2_Element]
g2BuiltinPoints =
  map (bls12_381_G2_uncompress . toBuiltin . decodeHexPoint) (snd embeddedSetup)
