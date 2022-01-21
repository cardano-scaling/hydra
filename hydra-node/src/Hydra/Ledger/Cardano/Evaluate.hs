module Hydra.Ledger.Cardano.Evaluate where

import Hydra.Prelude hiding (label)

import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.PParams (PParams, PParams' (..))
import Cardano.Ledger.Alonzo.Scripts (
  CostModel,
  ExUnits (..),
  Prices (..),
  defaultCostModel,
 )
import Cardano.Ledger.Alonzo.Tools (
  BasicFailure,
  ScriptFailure,
  evaluateTransactionExecutionUnits,
 )
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr)
import Cardano.Ledger.BaseTypes (ProtVer (..), boundRational)
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (EpochSize))
import Cardano.Slotting.Time (SystemStart (SystemStart), mkSlotLength)
import Data.Array (Array, array)
import Data.Default (def)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Chain.Direct.Util (Era)
import Hydra.Ledger.Cardano (
  CardanoTx,
  LedgerCrypto,
  Utxo,
  toLedgerTx,
  toLedgerUtxo,
 )

type RedeemerReport =
  (Map RdmrPtr (Either (ScriptFailure LedgerCrypto) ExUnits))

evaluateTx ::
  CardanoTx ->
  Utxo ->
  Either (BasicFailure LedgerCrypto) RedeemerReport
evaluateTx tx utxo =
  runIdentity $
    evaluateTransactionExecutionUnits
      pparams
      (toLedgerTx tx)
      (toLedgerUtxo utxo)
      epochInfo
      systemStart
      costModels

pparams :: PParams Era
pparams =
  def
    { _costmdls = Map.singleton PlutusV1 $ fromJust $ defaultCostModel PlutusV1
    , _maxValSize = 1000000000
    , _maxTxExUnits = ExUnits 12_500_000 10_000_000_000
    , _maxBlockExUnits = ExUnits 10000000000 10000000000
    , _protocolVersion = ProtVer 5 0
    , _prices =
        Prices
          { prMem = fromJust $ boundRational $ 721 % 10000000
          , prSteps = fromJust $ boundRational $ 577 % 10000
          }
    }

epochInfo :: Monad m => EpochInfo m
epochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

systemStart :: SystemStart
systemStart = SystemStart $ posixSecondsToUTCTime 0

-- NOTE(SN): copied from Test.Cardano.Ledger.Alonzo.Tools as not exported
costModels :: Array Language CostModel
costModels =
  array
    (PlutusV1, PlutusV1)
    [(PlutusV1, fromJust $ defaultCostModel PlutusV1)]
