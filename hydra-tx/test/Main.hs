module Main where

import Hydra.Prelude

import Hydra.Tx.AccumulatorSpec qualified
import Hydra.Tx.ContestationPeriodSpec qualified
import Hydra.Tx.Contract.ContractSpec qualified
import Hydra.Tx.HeadIdSpec qualified
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
    , testSpec "ContestationPeriod" Hydra.Tx.ContestationPeriodSpec.spec
    , testSpec "Contract" Hydra.Tx.Contract.ContractSpec.spec
    , testSpec "HeadId" Hydra.Tx.HeadIdSpec.spec
    , testSpec "IsTx" Hydra.Tx.IsTxSpec.spec
    , testSpec "KZGTrustedSetup" Hydra.Tx.KZGTrustedSetupSpec.spec
    , testSpec "Secret" Hydra.Tx.SecretSpec.spec
    , testSpec "SecretNegative" Hydra.Tx.SecretNegativeSpec.spec
    ]
