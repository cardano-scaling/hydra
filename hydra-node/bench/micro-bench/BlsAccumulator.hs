module BlsAccumulator where

import Accumulator (Accumulator, Element, buildAccumulator)
import Bindings (getPolyCommitOverG1, getPolyCommitOverG2)
import Cardano.Crypto.EllipticCurve.BLS12_381 (Point1, Point2, blsGenerator, blsMult, blsSerialize)
import Field qualified as F
import Hydra.Cardano.Api (CtxUTxO, TxOut, toPlutusTxOut)
import Hydra.Prelude
import PlutusLedgerApi.V3 qualified as Ledger
import PlutusTx (ToData (toBuiltinData))
import PlutusTx.Builtins qualified as Builtins
import Test.Hydra.Tx.Gen (genTxOut)
import Test.QuickCheck (generate, shuffle)

newtype NFPoint1 = NFPPoint1 Point1
  deriving (Generic)

instance NFData NFPoint1 where
  rnf :: NFPoint1 -> ()
  rnf (NFPPoint1 p) = blsSerialize p `seq` ()

newtype NFPoint2 = NFPPoint2 Point2
  deriving (Generic)
instance NFData NFPoint2 where
  rnf :: NFPoint2 -> ()
  rnf (NFPPoint2 p) = blsSerialize p `seq` ()

tau :: F.Scalar
tau = F.Scalar 22_435_875_175_126_190_499_447_740_508_185_965_837_690_552_500_527_637_822_603_658_699_938_581_184_511

generateTxOuts :: Int -> IO [Ledger.TxOut]
generateTxOuts n =
  replicateM n (generate genTxOut :: IO (TxOut CtxUTxO))
    <&> fmap (fromMaybe (error "Unreachable") . toPlutusTxOut)

generateSetup :: Int -> Int -> IO (Accumulator, [Element], [NFPoint1], [NFPoint2])
generateSetup ledgerSize fanoutSize = do
  ledgerTxOuts <- generateTxOuts ledgerSize
  shuffledTxOuts <- generate $ shuffle ledgerTxOuts

  let accumulator = buildAccumulator $ map (Builtins.fromBuiltin . Builtins.serialiseData . toBuiltinData) ledgerTxOuts
  let fanOutTxOuts = take fanoutSize shuffledTxOuts
  let fanOutElements = map (Builtins.fromBuiltin . Builtins.serialiseData . toBuiltinData) fanOutTxOuts

  let crsG1 = generateCrsG1 ledgerSize
  let crsG2 = generateCrsG2 ledgerSize

  return (accumulator, fanOutElements, crsG1, crsG2)

generateCrsG1 :: Integral a => a -> [NFPoint1]
generateCrsG1 ledgerSize =
  let powerOfTauField = map (F.powModScalar tau) [1 .. fromIntegral ledgerSize]
      powerOfTauInt = map F.unScalar powerOfTauField
      g1 = blsGenerator :: Point1
   in map (NFPPoint1 . blsMult g1) powerOfTauInt

generateCrsG2 :: Integral a => a -> [NFPoint2]
generateCrsG2 ledgerSize =
  let powerOfTauField = map (F.powModScalar tau) [1 .. fromIntegral ledgerSize]
      powerOfTauInt = map F.unScalar powerOfTauField
      g2 = blsGenerator :: Point2
   in map (NFPPoint2 . blsMult g2) powerOfTauInt

membershipBenchmarkG1 :: [Element] -> Accumulator -> [NFPoint1] -> IO (Either String NFPoint1)
membershipBenchmarkG1 elements accum crsG1 =
  getPolyCommitOverG1 elements accum (map (\(NFPPoint1 p) -> p) crsG1) >>= \case
    Left err -> return $ Left err
    Right p1 -> return $ Right (NFPPoint1 p1)

membershipBenchmarkG2 :: [Element] -> Accumulator -> [NFPoint2] -> IO (Either String NFPoint2)
membershipBenchmarkG2 elements accum crsG2 =
  getPolyCommitOverG2 elements accum (map (\(NFPPoint2 p) -> p) crsG2) >>= \case
    Left err -> return $ Left err
    Right p2 -> return $ Right (NFPPoint2 p2)
