module Hydra.Contract.HeadError (
  errorCode,
  module Hydra.Contract.HeadError,
) where

import Hydra.Contract.Error (ToErrorCode (..), errorCode)

data HeadError
  = InvalidHeadStateTransition
  | ChangedParameters
  | WrongStateInOutputDatum
  | HeadValueIsNotPreserved
  | SignerIsNotAParticipant
  | NoSigners
  | TooManySigners
  | ScriptNotSpendingAHeadInput
  | NoOutputDatumError
  | UnexpectedNonInlineDatum
  | NotPayingToHead
  | SignatureVerificationFailed
  | MustNotChangeVersion
  | BurntTokenNumberMismatch
  | ReimbursedOutputsDontMatch
  | STNotSpent
  | IncorrectUtxoHash
  | MissingCommits
  | NotAllValueCollected
  | IncorrectVersion
  | VersionNotIncremented
  | HasBoundedValidityCheckFailed
  | IncorrectClosedContestationDeadline
  | InfiniteUpperBound
  | InfiniteLowerBound
  | ContestersNonEmpty
  | CloseNoUpperBoundDefined
  | FailedCloseInitial
  | FailedCloseCurrent
  | FailedCloseOutdated
  | TooOldSnapshot
  | UpperBoundBeyondContestationDeadline
  | ContestNoUpperBoundDefined
  | MustNotPushDeadline
  | MustPushDeadline
  | ContesterNotIncluded
  | WrongNumberOfSigners
  | SignerAlreadyContested
  | FailedContestCurrent
  | FailedContestOutdated
  | FanoutUTxOHashMismatch
  | LowerBoundBeforeContestationDeadline
  | FanoutNoLowerBoundDefined
  | FanoutUTxOToDecommitHashMismatch
  | DepositNotSpent
  | DepositInputNotFound
  | HeadInputNotFound

instance ToErrorCode HeadError where
  toErrorCode = \case
    -- Generic
    InvalidHeadStateTransition -> "H1"
    ChangedParameters -> "H2"
    WrongStateInOutputDatum -> "H3"
    HeadValueIsNotPreserved -> "H4"
    SignerIsNotAParticipant -> "H5"
    NoSigners -> "H6"
    TooManySigners -> "H7"
    ScriptNotSpendingAHeadInput -> "H8"
    NoOutputDatumError -> "H9"
    UnexpectedNonInlineDatum -> "H10"
    NotPayingToHead -> "H11"
    SignatureVerificationFailed -> "H12"
    MustNotChangeVersion -> "H13"
    -- Abort
    BurntTokenNumberMismatch -> "H14"
    ReimbursedOutputsDontMatch -> "H15"
    -- Collect
    STNotSpent -> "H16"
    IncorrectUtxoHash -> "H17"
    MissingCommits -> "H18"
    NotAllValueCollected -> "H19"
    IncorrectVersion -> "H20"
    -- Decrement
    VersionNotIncremented -> "H21"
    -- Close
    HasBoundedValidityCheckFailed -> "H22"
    IncorrectClosedContestationDeadline -> "H23"
    InfiniteUpperBound -> "H24"
    InfiniteLowerBound -> "H25"
    ContestersNonEmpty -> "H26"
    CloseNoUpperBoundDefined -> "H27"
    FailedCloseInitial -> "H28"
    FailedCloseCurrent -> "H29"
    FailedCloseOutdated -> "H30"
    -- Contest
    TooOldSnapshot -> "H31"
    UpperBoundBeyondContestationDeadline -> "H32"
    ContestNoUpperBoundDefined -> "H33"
    MustNotPushDeadline -> "H34"
    MustPushDeadline -> "H35"
    ContesterNotIncluded -> "H36"
    WrongNumberOfSigners -> "H37"
    SignerAlreadyContested -> "H38"
    FailedContestCurrent -> "H39"
    FailedContestOutdated -> "H40"
    -- Fanout
    FanoutUTxOHashMismatch -> "H41"
    FanoutUTxOToDecommitHashMismatch -> "H42"
    LowerBoundBeforeContestationDeadline -> "H43"
    FanoutNoLowerBoundDefined -> "H44"
    DepositNotSpent -> "H45"
    DepositInputNotFound -> "H46"
    HeadInputNotFound -> "H47"
