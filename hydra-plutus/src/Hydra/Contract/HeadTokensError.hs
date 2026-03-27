module Hydra.Contract.HeadTokensError (
  errorCode,
  module Hydra.Contract.HeadTokensError,
) where

import Hydra.Contract.Error (ToErrorCode (..), errorCode)
import Text.Show (Show)

data HeadTokensError
  = SeedNotSpent
  | WrongNumberOfTokensMinted
  | MissingST
  | MissingPTs
  | WrongDatum
  | MintingNotAllowed
  | NoPTs
  | WrongQuantity
  | ExpectedHeadDatumType
  | ExpectedInlineDatum
  | MultipleHeadOutput
  deriving stock (Show)

instance ToErrorCode HeadTokensError where
  toErrorCode = \case
    SeedNotSpent -> "M01"
    WrongNumberOfTokensMinted -> "M02"
    MissingST -> "M03"
    MissingPTs -> "M04"
    WrongDatum -> "M05"
    MintingNotAllowed -> "M06"
    NoPTs -> "M07"
    WrongQuantity -> "M08"
    ExpectedHeadDatumType -> "M09"
    ExpectedInlineDatum -> "M10"
    MultipleHeadOutput -> "M11"
