module Hydra.Contract.InitialError (
  errorCode,
  module Hydra.Contract.InitialError,
) where

import Hydra.Contract.Error (ToErrorCode (..), errorCode)
import Text.Show (Show)

data InitialError
  = STNotBurned
  | MissingOrInvalidCommitAuthor
  | LockedValueDoesNotMatch
  | MismatchCommittedTxOutInDatum
  | CouldNotFindTheCorrectCurrencySymbolInTokens
  | MultipleHeadTokensOrMoreThan1PTsFound
  | NothingCommittedButTxOutInOutputDatum
  | CommittedTxOutButNothingInOutputDatum
  | MissingDatum
  | UnexpectedInlineDatum
  | CouldNotFindDatum
  | ExpectedCommitDatumTypeGotSomethingElse
  | ExpectedSingleCommitOutput
  | WrongHeadIdInCommitDatum
  deriving (Show)

instance ToErrorCode InitialError where
  toErrorCode = \case
    STNotBurned -> "I01"
    MissingOrInvalidCommitAuthor -> "I02"
    LockedValueDoesNotMatch -> "I03"
    MismatchCommittedTxOutInDatum -> "I04"
    CouldNotFindTheCorrectCurrencySymbolInTokens -> "I05"
    MultipleHeadTokensOrMoreThan1PTsFound -> "I06"
    NothingCommittedButTxOutInOutputDatum -> "I07"
    CommittedTxOutButNothingInOutputDatum -> "I08"
    MissingDatum -> "I09"
    UnexpectedInlineDatum -> "I10"
    CouldNotFindDatum -> "I11"
    ExpectedCommitDatumTypeGotSomethingElse -> "I12"
    ExpectedSingleCommitOutput -> "I13"
    WrongHeadIdInCommitDatum -> "I14"
