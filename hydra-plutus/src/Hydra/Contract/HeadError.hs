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
  | FailedCloseAny
  | TooOldSnapshot
  | UpperBoundBeyondContestationDeadline
  | ContestNoUpperBoundDefined
  | MustNotPushDeadline
  | MustPushDeadline
  | ContesterNotIncluded
  | WrongNumberOfSigners
  | SignerAlreadyContested
  | FailedContestCurrent
  | FailedContestUsedDec
  | FanoutUTxOHashMismatch
  | LowerBoundBeforeContestationDeadline
  | FanoutNoLowerBoundDefined
  | FanoutUTxOToCommitHashMismatch
  | FanoutUTxOToDecommitHashMismatch
  | DepositNotSpent
  | DepositInputNotFound
  | HeadInputNotFound
  | FailedContestUnusedDec
  | FailedContestUnusedInc
  | FailedContestUsedInc
  | FailedCloseUnusedDec
  | FailedCloseUsedDec
  | FailedCloseUnusedInc
  | FailedCloseUsedInc
  | MissingCRSDatum
  | MissingCRSRefInput

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
    -- Contest
    TooOldSnapshot -> "H29"
    UpperBoundBeyondContestationDeadline -> "H30"
    ContestNoUpperBoundDefined -> "H31"
    MustNotPushDeadline -> "H32"
    MustPushDeadline -> "H33"
    ContesterNotIncluded -> "H34"
    WrongNumberOfSigners -> "H35"
    SignerAlreadyContested -> "H36"
    FailedContestCurrent -> "H37"
    FailedContestUsedDec -> "H38"
    -- Fanout
    FanoutUTxOHashMismatch -> "H39"
    FanoutUTxOToDecommitHashMismatch -> "H40"
    LowerBoundBeforeContestationDeadline -> "H41"
    FanoutNoLowerBoundDefined -> "H42"
    DepositNotSpent -> "H43"
    DepositInputNotFound -> "H44"
    HeadInputNotFound -> "H45"
    FailedCloseAny -> "H46"
    FailedContestUnusedDec -> "H47"
    FailedContestUnusedInc -> "H48"
    FailedContestUsedInc -> "H49"
    FailedCloseUnusedDec -> "H50"
    FailedCloseUsedDec -> "H51"
    FailedCloseUnusedInc -> "H52"
    FailedCloseUsedInc -> "H53"
    FanoutUTxOToCommitHashMismatch -> "H54"
    MissingCRSDatum -> "H55"
    MissingCRSRefInput -> "H56"
