module Hydra.Contract.HeadError (
  errorCode,
  module Hydra.Contract.HeadError,
) where

import Hydra.Contract.Error (ToErrorCode (..), errorCode)

data HeadError
  = InvalidHeadStateTransition
  | BurntTokenNumberMismatch
  | ReimbursedOutputsDontMatch
  | STNotSpent
  | IncorrectUtxoHash
  | ChangedParameters
  | WrongStateInOutputDatum
  | MissingCommits
  | HeadValueIsNotPreserved
  | HasBoundedValidityCheckFailed
  | ClosedWithNonInitialHash
  | IncorrectClosedContestationDeadline
  | InfiniteUpperBound
  | InfiniteLowerBound
  | ContestersNonEmpty
  | TooOldSnapshot
  | UpperBoundBeyondContestationDeadline
  | ContestNoUpperBoundDefined
  | MustNotPushDeadline
  | MustPushDeadline
  | ContesterNotIncluded
  | WrongNumberOfSigners
  | SignerAlreadyContested
  | FannedOutUtxoHashNotEqualToClosedUtxoHash
  | LowerBoundBeforeContestationDeadline
  | FanoutNoLowerBoundDefined
  | CloseNoUpperBoundDefined
  | ScriptNotSpendingAHeadInput
  | SignerIsNotAParticipant
  | NoSigners
  | TooManySigners
  | NoOutputDatumError
  | UnexpectedNonInlineDatum
  | SignatureVerificationFailed
  | NotPayingToHead
  | NotAllValueCollected
  | SnapshotNumberMismatch
  | IncorrectVersion
  | LastKnownVersionIsNotMatching
  | IncorrectRedeemerUtxoToDecommitHash

instance ToErrorCode HeadError where
  toErrorCode = \case
    InvalidHeadStateTransition -> "H01"
    BurntTokenNumberMismatch -> "H02"
    ReimbursedOutputsDontMatch -> "H03"
    STNotSpent -> "H04"
    IncorrectUtxoHash -> "H05"
    ChangedParameters -> "H06"
    WrongStateInOutputDatum -> "H07"
    MissingCommits -> "H08"
    HeadValueIsNotPreserved -> "H09"
    HasBoundedValidityCheckFailed -> "H10"
    ClosedWithNonInitialHash -> "H11"
    IncorrectClosedContestationDeadline -> "H12"
    InfiniteUpperBound -> "H13"
    InfiniteLowerBound -> "H14"
    ContestersNonEmpty -> "H15"
    TooOldSnapshot -> "H16"
    UpperBoundBeyondContestationDeadline -> "H17"
    ContestNoUpperBoundDefined -> "H18"
    MustNotPushDeadline -> "H19"
    MustPushDeadline -> "H20"
    ContesterNotIncluded -> "H21"
    WrongNumberOfSigners -> "H22"
    SignerAlreadyContested -> "H23"
    FannedOutUtxoHashNotEqualToClosedUtxoHash -> "H24"
    LowerBoundBeforeContestationDeadline -> "H25"
    FanoutNoLowerBoundDefined -> "H26"
    CloseNoUpperBoundDefined -> "H27"
    ScriptNotSpendingAHeadInput -> "H28"
    SignerIsNotAParticipant -> "H29"
    NoSigners -> "H30"
    TooManySigners -> "H31"
    NoOutputDatumError -> "H32"
    UnexpectedNonInlineDatum -> "H33"
    SignatureVerificationFailed -> "H34"
    NotPayingToHead -> "H35"
    NotAllValueCollected -> "H36"
    SnapshotNumberMismatch -> "H37"
    IncorrectVersion -> "H38"
    LastKnownVersionIsNotMatching -> "H39"
    IncorrectRedeemerUtxoToDecommitHash -> "H40"
