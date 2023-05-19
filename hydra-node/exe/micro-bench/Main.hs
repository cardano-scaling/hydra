{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hydra.Prelude

import Criterion (bench, bgroup, whnf)
import Criterion.Main (defaultMain)
import Hydra.Ledger.Cardano ( Tx, cardanoLedger, genFixedSizeSequenceOfSimplePaymentTransactions )
import Hydra.Ledger (Ledger (applyTransactions), ValidationError, ChainSlot (ChainSlot))
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Ledger
import qualified Cardano.Slotting.Time as Slotting
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  ExecutionUnitPrices (ExecutionUnitPrices),
  LedgerEra,
  ProtocolParameters (..), UTxO,
 )
import Hydra.Ledger.Cardano.Configuration (newLedgerEnv)
import Hydra.Ledger.Cardano.Evaluate (epochInfo, pparams)
import Test.QuickCheck (generate)

main :: IO ()
main = do
    (utxo, txs) <- prepareTxs 10_000
    defaultMain
        [ bgroup "Cardano Ledger" [bench "Apply Tx" $ whnf benchApplyTxs (utxo, txs)]
        ]

prepareTxs :: Int -> IO (UTxO, [Tx])
prepareTxs =
  generate . genFixedSizeSequenceOfSimplePaymentTransactions


benchApplyTxs :: (UTxO, [Tx]) -> Either (Tx, ValidationError) UTxO
benchApplyTxs (utxo, txs) = applyTransactions defaultLedger (ChainSlot 1) utxo txs

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
