{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Hydra.Prelude

import qualified Data.Aeson as Aeson
import Criterion (bench, bgroup, whnf, nf)
import Criterion.Main (defaultMain)
import Hydra.Cardano.Api (
  UTxO, serialiseToCBOR,
 )
import Hydra.Ledger (ChainSlot (ChainSlot), Ledger (applyTransactions), ValidationError)
import Hydra.Ledger.Cardano (Tx, cardanoLedger, genFixedSizeSequenceOfSimplePaymentTransactions)
import Test.QuickCheck (generate)
import Hydra.API.ClientInput (ClientInput(NewTx))
import Data.Aeson ((.=), object, Value(String))
import qualified Data.List as List
import Hydra.Chain.Direct.Fixture (defaultGlobals, defaultLedgerEnv)

main :: IO ()
main = do
  (utxo, tx) <- prepareTx
  let jsonNewTx = (Aeson.encode . NewTx) tx
      toNewTx bs = object [ "tag" .= ("NewTx" :: Text),  "transaction" .= String (decodeUtf8 bs) ]
      cborNewTx = (Aeson.encode . toNewTx . serialiseToCBOR) tx
  defaultMain
    [ bgroup
        "Cardano Ledger"
        [ bench "Apply Tx" $ whnf benchApplyTxs (utxo, tx)
        , bench "Serialize NewTx (JSON)" $ nf (Aeson.encode . NewTx)  tx
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
