module Main where

import Hydra.Prelude

import Criterion (bench, bgroup, nf, whnf)
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
  (utxo, tx) <- prepareTx
  let jsonNewTx = (Aeson.encode . NewTx) tx
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

prepareTx :: IO (UTxO, Tx)
prepareTx =
  second List.head <$> generate (genFixedSizeSequenceOfSimplePaymentTransactions 1)

benchApplyTxs :: (UTxO, Tx) -> Either (Tx, ValidationError) UTxO
benchApplyTxs (utxo, tx) = applyTransactions defaultLedger (ChainSlot 1) utxo [tx]

defaultLedger :: Ledger Tx
defaultLedger = cardanoLedger defaultGlobals defaultLedgerEnv
