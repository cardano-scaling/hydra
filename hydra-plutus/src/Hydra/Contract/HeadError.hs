module Hydra.Contract.HeadError (
  errorCode,
  module Hydra.Contract.HeadError,
) where

import Hydra.Contract.Error (ToErrorCode (..), errorCode)
import Text.Show (Show)

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
  | InvalidSnapshotSignature
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
  | DatumNotFound
  | SignatureVerificationFailed
  | PartySignatureVerificationFailed
  | NotPayingToHead
  | NotAllValueCollected
  deriving (Show)

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
    -- XXX: This error code is redundant and can be removed in favor of H35.
    -- This will also make the close checks consistent with the contest's
    InvalidSnapshotSignature -> "H11"
    ClosedWithNonInitialHash -> "H12"
    IncorrectClosedContestationDeadline -> "H13"
    InfiniteUpperBound -> "H14"
    InfiniteLowerBound -> "H15"
    ContestersNonEmpty -> "H16"
    TooOldSnapshot -> "H17"
    UpperBoundBeyondContestationDeadline -> "H18"
    ContestNoUpperBoundDefined -> "H19"
    MustNotPushDeadline -> "H20"
    MustPushDeadline -> "H21"
    ContesterNotIncluded -> "H22"
    WrongNumberOfSigners -> "H23"
    SignerAlreadyContested -> "H24"
    FannedOutUtxoHashNotEqualToClosedUtxoHash -> "H25"
    LowerBoundBeforeContestationDeadline -> "H26"
    FanoutNoLowerBoundDefined -> "H27"
    CloseNoUpperBoundDefined -> "H28"
    ScriptNotSpendingAHeadInput -> "H29"
    SignerIsNotAParticipant -> "H30"
    NoSigners -> "H31"
    TooManySigners -> "H32"
    NoOutputDatumError -> "H33"
    DatumNotFound -> "H34"
    SignatureVerificationFailed -> "H35"
    PartySignatureVerificationFailed -> "H36"
    NotPayingToHead -> "H37"
    NotAllValueCollected -> "H38"
