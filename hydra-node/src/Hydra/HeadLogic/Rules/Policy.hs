module Hydra.HeadLogic.Rules.Policy where

import Hydra.HeadLogic.Program (Program, getUserValidation)
import Hydra.Prelude
import Hydra.UserValidationRules (PolicyViolation, UserValidation (..))

checkTransactionPolicy :: tx -> Program tx (Either PolicyViolation ())
checkTransactionPolicy tx = do
  mvalidation <- getUserValidation
  case mvalidation of
    Nothing -> pure (Right ())
    Just UserValidation{validateTransaction} -> pure $ validateTransaction tx
