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
-- The __accumulator commitment__ is @A(τ)·G1@ — a single G1 point (48 bytes) stored
-- in the Closed datum. The __membership proof__ for a distributed subset @S@ is the
-- quotient @Q(X) = A(X) / P_S(X)@ committed as @Q(τ)·G1@. The on-chain validator
-- verifies the pairing identity:
--
-- > e(A(τ)·G1, G2) = e(Q(τ)·G1, P_S(τ)·G2)
--
-- where @P_S(τ)·G2@ is computed on-chain via a G2 MSM using the on-chain G2 CRS.
--
-- = The Trusted Setup
--
-- Computing @A(τ)·G1@ and @Q(τ)·G1@ off-chain, and @P_S(τ)·G2@ on-chain, all
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
--   * 4096 G1 points @[G1, τ·G1, ..., τ^4095·G1]@ — used off-chain to build the
--     accumulator commitment and membership proofs
--   * 65 G2 points  @[G2, τ·G2, ..., τ^64·G2]@   — used as the on-chain
--     verification CRS (G2 MSM to evaluate @P_S(τ)·G2@)
--
-- = Accumulator size limit
--
-- The accumulator polynomial has degree n (one root per UTxO). Computing @A(τ)·G1@
-- off-chain needs n+1 G1 points. EIP-4844 provides 4096 G1 points, so a head can
-- hold up to __4095 UTxOs__. This is enforced by 'maxAccumulatorSize'.
-- The on-chain G2 CRS only needs batch-size-many points per partial fanout step,
-- so the 65 G2 points do not constrain the accumulator size.
module Hydra.Tx.KZGTrustedSetup (
  KZGSetupError (..),
  g1Points,
  g2Points,
  g1BuiltinPoints,
  g2BuiltinPoints,
  maxAccumulatorSize,
  maxFanoutBatchSize,
  fanoutChunkSize,
  fanoutOutputThreshold,
) where

import Hydra.Prelude hiding (filter, foldMap, isJust, map, (<$>), (==))

import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Point1, Point2, blsCompress, blsUncompress)
import Cardano.Crypto.Hash (SHA256)
import Cardano.Crypto.Hash.Class (HashAlgorithm (digest))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseEither, withObject, (.:))
import Data.ByteString.Base16 qualified as Base16
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text qualified as T
import PlutusTx.Builtins (BuiltinBLS12_381_G1_Element, BuiltinBLS12_381_G2_Element, bls12_381_G1_uncompress, bls12_381_G2_uncompress, toBuiltin)

-- | Errors that can occur when loading the embedded KZG trusted setup.
--
-- In practice these should never occur — the trusted setup JSON is embedded at
-- compile time and its integrity is validated by the test suite. A 'Left' here
-- would indicate binary tampering ('IntegrityCheckFailed') or a corrupted
-- build artefact.
data KZGSetupError
  = -- | SHA-256 digest of the embedded file does not match the known-good value.
    IntegrityCheckFailed
      { expectedHash :: Text
      , actualHash :: Text
      }
  | -- | The embedded bytes could not be decoded as JSON at all.
    JsonDecodeFailed
  | -- | The JSON decoded but failed the structural parser (missing keys etc.).
    JsonParseFailed {parseError :: Text}
  | -- | A G1 point hex string could not be hex-decoded or BLS-decompressed.
    InvalidG1Point {hexPoint :: Text}
  | -- | A G2 point hex string could not be hex-decoded or BLS-decompressed.
    InvalidG2Point {hexPoint :: Text}
  deriving stock (Show, Eq, Generic)

-- | Maximum accumulator element count supported by the currently embedded G1 CRS.
-- The EIP-4844 setup provides exactly 4096 G1 monomial points [G1, τ·G1, ..., τ^4095·G1],
-- so the accumulator supports up to 4095 elements (n elements need n+1 G1 points).
maxAccumulatorSize :: Int
maxAccumulatorSize = 4095

-- | Maximum number of UTxOs that can be fanned out in a single partial fanout transaction.
-- Constrained by the on-chain G2 CRS (65 points → 64-element batch) and by Cardano
-- execution budget. In practice the execution budget is the binding constraint.
maxFanoutBatchSize :: Int
maxFanoutBatchSize = 64

-- | Number of outputs distributed per partial fanout transaction.
-- Both regular and partial fanout run the same on-chain G2 MSM pairing check.
-- Empirically validated: 7 ada-only outputs consume ~86% CPU / ~39% memory,
-- leaving headroom for UTxOs with tokens or complex datums.
fanoutChunkSize :: Int
fanoutChunkSize = 7

-- | Maximum outputs in a regular (full) fanout transaction before switching
-- to partial fanout. Empirically, 10 ada-only outputs consume ~91% CPU budget;
-- above this the transaction fails. Partial fanout uses 'fanoutChunkSize' per
-- step to maintain safe headroom.
fanoutOutputThreshold :: Int
fanoutOutputThreshold = 10

-- | Expected SHA-256 of trusted_setup.json, from the EIP-4844 ceremony output.
-- Verify independently with: sha256sum hydra-tx/trusted_setup.json
-- Source: https://github.com/ethereum/kzg-ceremony-verifier/tree/master/output_setups
trustedSetupExpectedSHA256 :: Text
trustedSetupExpectedSHA256 = "9a8dcad9eaba191842f57d23d14674cbdea4b3cf7912fcc477821264dfe0c042"

-- We use @g1_monomial@ (not @g1_lagrange@): the monomial form @[G1, τ·G1, ...]@ is required
-- for KZG polynomial commitments. The @g1_lagrange@ form (used by Ethereum clients for blob
-- commitments) is NOT suitable for the accumulator scheme here.
embeddedSetup :: Either KZGSetupError ([Text], [Text])
embeddedSetup = do
  let rawBytes = $(makeRelativeToProject "trusted_setup.json" >>= embedFile)
      actualHex = decodeUtf8 $ Base16.encode $ digest (Proxy @SHA256) rawBytes
  when (actualHex /= trustedSetupExpectedSHA256) $
    Left IntegrityCheckFailed{expectedHash = trustedSetupExpectedSHA256, actualHash = actualHex}
  v <- maybe (Left JsonDecodeFailed) Right (Aeson.decodeStrict rawBytes)
  first (JsonParseFailed . toText) $
    parseEither (withObject "TrustedSetup" parse) v
 where
  parse :: Aeson.Object -> Parser ([Text], [Text])
  parse obj = fmap (,) (obj .: "g1_monomial") <*> obj .: "g2_monomial"

decodeHexPoint :: Text -> Either String ByteString
decodeHexPoint t = Base16.decode $ encodeUtf8 $ fromMaybe t (T.stripPrefix "0x" t)

-- | G1 powers of tau [G1, τ·G1, ..., τ^4095·G1] from the EIP-4844 ceremony (monomial form).
-- 4096 points; used off-chain to build accumulator commitments and membership proofs.
g1Points :: Either KZGSetupError [Point1]
g1Points = mapM parseG1 . fst =<< embeddedSetup
 where
  parseG1 :: Text -> Either KZGSetupError Point1
  parseG1 hex = do
    bs <- first (const (InvalidG1Point hex)) (decodeHexPoint hex)
    first (const (InvalidG1Point hex)) (blsUncompress bs)

-- | G2 powers of tau [G2, τ·G2, ..., τ^64·G2] from the EIP-4844 ceremony (monomial form).
-- 65 points; used as the on-chain verification CRS (G2 MSM to evaluate @P_S(τ)·G2@).
g2Points :: Either KZGSetupError [Point2]
g2Points = mapM parseG2 . snd =<< embeddedSetup
 where
  parseG2 :: Text -> Either KZGSetupError Point2
  parseG2 hex = do
    bs <- first (const (InvalidG2Point hex)) (decodeHexPoint hex)
    first (const (InvalidG2Point hex)) (blsUncompress bs)

-- | G1 points as Plutus built-in type, for use in off-chain accumulator commitment.
-- Derived from 'g1Points' by re-compressing to bytes; shares the same parsed data.
g1BuiltinPoints :: Either KZGSetupError [BuiltinBLS12_381_G1_Element]
g1BuiltinPoints = fmap (fmap (bls12_381_G1_uncompress . toBuiltin . blsCompress)) g1Points

-- | G2 points as Plutus built-in type, for use in on-chain verification CRS.
-- Derived from 'g2Points' by re-compressing to bytes; shares the same parsed data.
g2BuiltinPoints :: Either KZGSetupError [BuiltinBLS12_381_G2_Element]
g2BuiltinPoints = fmap (fmap (bls12_381_G2_uncompress . toBuiltin . blsCompress)) g2Points
