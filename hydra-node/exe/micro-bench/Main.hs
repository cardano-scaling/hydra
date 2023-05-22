{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Hydra.Prelude

import qualified Data.Aeson as Aeson
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Ledger
import qualified Cardano.Slotting.Time as Slotting
import Criterion (bench, bgroup, whnf, nf)
import Criterion.Main (defaultMain)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  ExecutionUnitPrices (ExecutionUnitPrices),
  LedgerEra,
  ProtocolParameters (..),
  UTxO, serialiseToCBOR,
 )
import Hydra.Ledger (ChainSlot (ChainSlot), Ledger (applyTransactions), ValidationError)
import Hydra.Ledger.Cardano (Tx, cardanoLedger, genFixedSizeSequenceOfSimplePaymentTransactions)
import Hydra.Ledger.Cardano.Configuration (newLedgerEnv)
import Hydra.Ledger.Cardano.Evaluate (epochInfo, pparams)
import Test.QuickCheck (generate)
import Hydra.API.ClientInput (ClientInput(NewTx))
import Data.Aeson ((.=), object, Value(String))
import qualified Data.List as List

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

-- | Default environment for the L2 ledger using the fixed L1 'pparams' with
-- zeroed fees and prices. NOTE: This is using still a constant SlotNo = 1.
defaultLedgerEnv :: Ledger.LedgerEnv LedgerEra
defaultLedgerEnv =
  newLedgerEnv pparams'
 where
  pparams' =
    pparams
      { protocolParamPrices = Just $ ExecutionUnitPrices 0 0
      , protocolParamTxFeePerByte = 0
      , protocolParamTxFeeFixed = 0
      }

defaultGlobals :: Ledger.Globals
defaultGlobals =
  Ledger.Globals
    { Ledger.epochInfo = epochInfo
    , Ledger.slotsPerKESPeriod = 20
    , Ledger.stabilityWindow = 33
    , Ledger.randomnessStabilisationWindow = 33
    , Ledger.securityParameter = 10
    , Ledger.maxKESEvo = 10
    , Ledger.quorum = 5
    , Ledger.maxMajorPV = 1_000
    , Ledger.maxLovelaceSupply = 45 * 1_000 * 1_000 * 1_000 * 1_000 * 1_000
    , Ledger.activeSlotCoeff = Ledger.mkActiveSlotCoeff . unsafeBoundRational $ 0.9
    , Ledger.networkId = Ledger.Testnet
    , Ledger.systemStart = Slotting.SystemStart $ posixSecondsToUTCTime 0
    }
 where
  unsafeBoundRational r =
    fromMaybe (error $ "Could not convert from Rational: " <> show r) $ Ledger.boundRational r
