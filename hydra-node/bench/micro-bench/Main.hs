module Main where

import "hydra-prelude" Hydra.Prelude

import "QuickCheck" Test.QuickCheck (generate)
import "aeson" Data.Aeson (Value (String), object, (.=))
import "aeson" Data.Aeson qualified as Aeson
import "base" Data.List qualified as List
import "criterion" Criterion (bench, bgroup, nf, whnf)
import "criterion" Criterion.Main (defaultMain)
import "hydra-cardano-api" Hydra.Cardano.Api (
  UTxO,
  serialiseToCBOR,
 )
import "hydra-node" Hydra.API.ClientInput (ClientInput (NewTx))
import "hydra-node" Hydra.Ledger (Ledger (applyTransactions), ValidationError)
import "hydra-node" Test.Hydra.Ledger.Cardano (genFixedSizeSequenceOfSimplePaymentTransactions)
import "hydra-node" Test.Hydra.Node.Fixture (defaultGlobals, defaultLedgerEnv)
import "hydra-tx" Hydra.Chain.ChainState (ChainSlot (ChainSlot))
import "hydra-tx" Hydra.Ledger.Cardano (Tx, cardanoLedger)

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
  defaultMain
    [ bgroup
        "Cardano Ledger"
        [ bench "Apply Tx" $ whnf benchApplyTxs (utxo, tx)
        , bench "Serialize NewTx (JSON)" $ nf (Aeson.encode . NewTx) tx
        , bench "Serialize NewTx (CBOR)" $ nf serialiseToCBOR tx
        , bench "Deserialize NewTx (JSON)" $ whnf (Aeson.decode @(ClientInput Tx)) jsonNewTx
        , bench "Deserialize NewTx (CBOR-in-JSON)" $ whnf (Aeson.decode @(ClientInput Tx)) cborNewTx
        ]
    ]

prepareTx :: Int -> IO (UTxO, Tx)
prepareTx n =
  second List.head <$> generate (genFixedSizeSequenceOfSimplePaymentTransactions n)

benchApplyTxs :: (UTxO, Tx) -> Either (Tx, ValidationError) UTxO
benchApplyTxs (utxo, tx) = applyTransactions defaultLedger (ChainSlot 1) utxo [tx]

defaultLedger :: Ledger Tx
defaultLedger = cardanoLedger defaultGlobals defaultLedgerEnv
