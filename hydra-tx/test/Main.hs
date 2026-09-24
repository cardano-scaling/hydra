module Main where

import Hydra.Prelude

import Hydra.Ledger.Cardano.EvaluateSpec qualified
import Hydra.Tx.AccumulatorSpec qualified
import Hydra.Tx.CardanoApiHashLengthSpec qualified
import Hydra.Tx.ContestationPeriodSpec qualified
import Hydra.Tx.Contract.ContractSpec qualified
import Hydra.Tx.DepositPeriodSpec qualified
import Hydra.Tx.HeadIdSpec qualified
import Hydra.Tx.HeadValidatorAgreementSpec qualified
import Hydra.Tx.IsTxSpec qualified
import Hydra.Tx.KZGTrustedSetupSpec qualified
import Hydra.Tx.SecretNegativeSpec qualified
import Hydra.Tx.SecretSpec qualified
import Test.Hydra.TastyMain (defaultMainHydra, testSpec)

main :: IO ()
main =
  defaultMainHydra
    "hydra-tx"
    [ testSpec "Accumulator" Hydra.Tx.AccumulatorSpec.spec
    , testSpec "CardanoApiHashLength" Hydra.Tx.CardanoApiHashLengthSpec.spec
    , testSpec "ContestationPeriod" Hydra.Tx.ContestationPeriodSpec.spec
    , testSpec "Contract" Hydra.Tx.Contract.ContractSpec.spec
    , testSpec "DepositPeriod" Hydra.Tx.DepositPeriodSpec.spec
    , testSpec "Evaluate" Hydra.Ledger.Cardano.EvaluateSpec.spec
    , testSpec "HeadId" Hydra.Tx.HeadIdSpec.spec
    , testSpec "HeadValidatorAgreement" Hydra.Tx.HeadValidatorAgreementSpec.spec
    , testSpec "IsTx" Hydra.Tx.IsTxSpec.spec
    , testSpec "KZGTrustedSetup" Hydra.Tx.KZGTrustedSetupSpec.spec
    , testSpec "Secret" Hydra.Tx.SecretSpec.spec
    , testSpec "SecretNegative" Hydra.Tx.SecretNegativeSpec.spec
    ]
