module Main where

import Hydra.Prelude

import BlsAccumulator
import Criterion (bench, bgroup, nf, nfIO, whnf)
import Criterion.Main (defaultMain)
import Data.Aeson (Value (String), object, (.=))
import Data.Aeson qualified as Aeson
import Data.List qualified as List
import Hydra.API.ClientInput (ClientInput (NewTx))
import Hydra.Cardano.Api (
  UTxO,
  serialiseToCBOR,
 )
import Hydra.Chain.ChainState (ChainSlot (ChainSlot))
import Hydra.Ledger (Ledger (applyTransactions), ValidationError)
import Hydra.Ledger.Cardano (Tx, cardanoLedger, genFixedSizeSequenceOfSimplePaymentTransactions)
import Test.Hydra.Node.Fixture (defaultGlobals, defaultLedgerEnv)
import Test.QuickCheck (generate)

main :: IO ()
main = do
  -- Use this env var to run benchmarks for more transactions:
  --
  -- > N_TXNS=1 cabal bench micro --benchmark-options '--json output.json +RTS -T'
  --
  -- You can then find the `peakMbAllocated` field in the resulting json file.
  -- (Sorry, criterion makes it very hard to find this with a simple jq
  -- command.)
  --
  nTxns <- fromMaybe 1 . (>>= readMaybe) <$> lookupEnv "N_TXNS"
  (utxo, tx) <- prepareTx nTxns
  let jsonNewTx = (Aeson.encode . NewTx) tx
      toNewTx :: ByteString -> Value
      toNewTx bs = object ["tag" .= ("NewTx" :: Text), "transaction" .= String (decodeUtf8 bs)]
      cborNewTx = (Aeson.encode . toNewTx . serialiseToCBOR) tx

  ledgerSize <- fromMaybe 10_000 . (>>= readMaybe) <$> lookupEnv "LEDGER_SIZE"
  fanoutSize <- fromMaybe 50 . (>>= readMaybe) <$> lookupEnv "FANOUT_SIZE"
  (accum, fanOutElements, crsG1, crsG2) <- generateSetup ledgerSize fanoutSize
  defaultMain
    [ bgroup
        "Cardano Ledger"
        [ bench "Apply Tx" $ whnf benchApplyTxs (utxo, tx)
        , bench "Serialize NewTx (JSON)" $ nf (Aeson.encode . NewTx) tx
        , bench "Serialize NewTx (CBOR)" $ nf serialiseToCBOR tx
        , bench "Deserialize NewTx (JSON)" $ whnf (Aeson.decode @(ClientInput Tx)) jsonNewTx
        , bench "Deserialize NewTx (CBOR-in-JSON)" $ whnf (Aeson.decode @(ClientInput Tx)) cborNewTx
        ]
    , bgroup
        "BLS Accumulator"
        [ -- TODO: Create a more correct normal form benchmark for CRS generation
          -- as it is now, it uses length to avoid laziness.
          bench "Generate CRS G1" $ nf generateCrsG1 ledgerSize
        , bench "Generate CRS G2" $ nf generateCrsG2 ledgerSize
        , bench "Membership Benchmark G1" $ nfIO (membershipBenchmarkG1 fanOutElements accum crsG1)
        , bench "Membership Benchmark G2" $ nfIO (membershipBenchmarkG2 fanOutElements accum crsG2)
        ]
    ]

prepareTx :: Int -> IO (UTxO, Tx)
prepareTx n =
  second List.head <$> generate (genFixedSizeSequenceOfSimplePaymentTransactions n)

benchApplyTxs :: (UTxO, Tx) -> Either (Tx, ValidationError) UTxO
benchApplyTxs (utxo, tx) = applyTransactions defaultLedger (ChainSlot 1) utxo [tx]

defaultLedger :: Ledger Tx
defaultLedger = cardanoLedger defaultGlobals defaultLedgerEnv
