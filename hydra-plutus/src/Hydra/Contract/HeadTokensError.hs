module Hydra.Contract.HeadTokensError (
  errorCode,
  module Hydra.Contract.HeadTokensError,
) where

import Hydra.Contract.Error (ToErrorCode (..), errorCode)
import "base" Text.Show (Show)

data HeadTokensError
  = SeedNotSpent
  | WrongNumberOfTokensMinted
  | MissingST
  | WrongNumberOfInitialOutputs
  | WrongDatum
  | MintingNotAllowed
  | NoPT
  | WrongQuantity
  | ExpectedHeadDatumType
  | ExpectedInlineDatum
  | MultipleHeadOutput
  | WrongInitialDatum
  deriving stock (Show)

instance ToErrorCode HeadTokensError where
  toErrorCode = \case
    SeedNotSpent -> "M01"
    WrongNumberOfTokensMinted -> "M02"
    MissingST -> "M03"
    WrongNumberOfInitialOutputs -> "M04"
    WrongDatum -> "M05"
    MintingNotAllowed -> "M06"
    NoPT -> "M07"
    WrongQuantity -> "M08"
    ExpectedHeadDatumType -> "M09"
    ExpectedInlineDatum -> "M10"
    MultipleHeadOutput -> "M11"
    WrongInitialDatum -> "M12"
