module Hydra.Tx.Accumulator.Commitment where

import Accumulator (Accumulator)
import Bindings (getPolyCommitOverG1)
import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Point1, Point2, blsCompress)
import Cardano.Crypto.Hash (Blake2b_256)
import Cardano.Crypto.Hash.Class (HashAlgorithm (digest))
import Hydra.Contract.KZGTrustedSetup qualified as KZG
import Hydra.Prelude

commit :: Int -> Accumulator -> Either Text ByteString
commit pointCount acc = bimap toText blsCompress $ getPolyCommitOverG1 [] acc (crsG1Points pointCount)

hashCommitment :: ByteString -> ByteString
hashCommitment = digest (Proxy @Blake2b_256)

fromKZGSetup :: Either KZG.KZGSetupError a -> a
fromKZGSetup = either (\e -> error $ "KZG trusted setup invariant violated: " <> show e) id

crsG1Points :: Int -> [Point1]
crsG1Points n = take n $ fromKZGSetup KZG.g1Points

-- | Returns the first @n@ G2 powers of tau from the Ethereum 32768-point trusted setup.
-- Used as the on-chain CRS for verifying membership proofs:
-- @[G2, τ·G2, ..., τ^(n-1)·G2]@.
crsG2Points :: Int -> [Point2]
crsG2Points n = take n $ fromKZGSetup KZG.g2Points
