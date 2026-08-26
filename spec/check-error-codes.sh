#!/usr/bin/env bash
# Validator error-code coverage ledger (implementation layer).
#
# Companion to check-trust-ledger.sh, one level down: that script gates the trusted
# base of the model<->reference bridge; this one gates the reject-path coverage of the
# on-chain validators themselves. The differential and mutation tests only see the
# input families somebody constructed, so a new validator check can ship silently
# uncovered - and a check nobody can trip is exactly where the next deposit-binding
# class of bug hides.
#
# For every error code a validator can raise (Hydra.Contract.HeadError's "Hxx" codes
# and deposit.ak's "Dxx" codes) this script verifies:
#   1. the constructor is actually raised somewhere - a dead code fails the build:
#      delete it instead (see the H15-H20/H40/H54 cleanup for precedent);
#   2. the Aiken codes and their Haskell mirror (Hydra.Contract.DepositError) agree;
#   3. the code appears in the ledger below with an accurate status:
#        tested          - a test ASSERTS this code, i.e. some test source applies
#                          `toErrorCode` to the constructor. That is the mutation
#                          framework's expected-error form, and the only one in use
#                          (229 occurrences; no test names a code any other way).
#        untested:<tag>  - a documented exclusion, printed as visible debt; if a test
#                          starts asserting the code this FAILS so the row is promoted
#                          to tested and the debt list stays accurate.
# A new error code therefore cannot ship without either a test asserting it or an
# explicit, reviewed exclusion here.
#
# The evidence has to be `toErrorCode <Name>` and not the bare constructor name,
# because a bare-name grep counts prose. Both directions were wrong: H43 claimed
# "tested" on the strength of a comment saying its check is *subsumed* by an earlier
# one (i.e. the opposite of coverage), and a TODO or a prop description mentioning an
# EXCLUDED code failed the check, demanding a promotion no test had earned.
#
# Exclusion tags:
#   catch-all          - a dispatcher/fallback arm, unreachable by a single mutation
#   datum-plumbing     - malformed datum shapes the mutation corpus does not construct
#   context-plumbing   - malformed script-context shapes (missing head input, missing
#                        redeemer entry, ...)
#   validity-plumbing  - missing validity-bound arms (the finite/infinite bound cases
#                        ARE tested where they guard protocol logic)
#   crs-plumbing       - missing CRS reference-input/datum arms (the content binding is
#                        tested: InvalidCRSDatum)
#   variant-gap        - a genuine coverage gap worth a test; candidates for follow-up
#   code-not-asserted  - a test does exercise the reject path, but nothing pins WHICH
#                        code it raised, so the coverage cannot be attributed here
#   unreachable        - an earlier conjunct subsumes this one, so no input can reach
#                        it; a deletion candidate, kept visible rather than claimed
#
# NB even a strict "tested" is a necessary condition, not proof the test is meaningful.
# The ledger keeps the coverage question visible and reviewed; it does not answer it.
set -euo pipefail
cd "$(dirname "$0")/.."

HEAD_ERRORS=hydra-plutus/src/Hydra/Contract/HeadError.hs
DEPOSIT_AK=hydra-plutus/validators/deposit.ak
DEPOSIT_MIRROR=hydra-plutus/src/Hydra/Contract/DepositError.hs
RAISE_DIRS=(hydra-plutus/src)
TEST_DIRS=(hydra-tx/test hydra-plutus/test hydra-node/test)

ledger=$(cat <<'LEDGER'
D01 DepositPeriodSurpassed tested
D02 DepositNoUpperBoundDefined untested:validity-plumbing
D03 DepositNoLowerBoundDefined tested
D04 DepositPeriodNotReached tested
D05 IncorrectDepositHash tested
D06 DepositHeadInputNotFound tested
D07 HeadInputRedeemerNotFound untested:context-plumbing
D08 HeadRedeemerNotIncrement tested
D09 DepositNotClaimedByHead tested
D10 MultipleDepositsRecovered tested
H1 InvalidHeadStateTransition untested:catch-all
H2 ChangedParameters tested
H3 WrongStateInOutputDatum untested:datum-plumbing
H4 HeadValueIsNotPreserved tested
H5 SignerIsNotAParticipant tested
H6 NoSigners tested
H7 TooManySigners tested
H8 ScriptNotSpendingAHeadInput untested:context-plumbing
H9 NoOutputDatumError untested:datum-plumbing
H10 UnexpectedNonInlineDatum untested:datum-plumbing
H11 NotPayingToHead tested
H12 SignatureVerificationFailed tested
H13 MustNotChangeVersion tested
H14 BurntTokenNumberMismatch tested
H21 VersionNotIncremented tested
H22 HasBoundedValidityCheckFailed tested
H23 IncorrectClosedContestationDeadline tested
H24 InfiniteUpperBound tested
H25 InfiniteLowerBound tested
H26 ContestersNonEmpty tested
H27 CloseNoUpperBoundDefined untested:validity-plumbing
H28 FailedCloseInitial tested
H29 TooOldSnapshot tested
H30 UpperBoundBeyondContestationDeadline tested
H31 ContestNoUpperBoundDefined untested:validity-plumbing
H32 MustNotPushDeadline tested
H33 MustPushDeadline tested
H34 ContesterNotIncluded tested
H35 WrongNumberOfSigners untested:variant-gap
H36 SignerAlreadyContested tested
H37 FailedContestUnused tested
H38 FailedContestUsed untested:variant-gap
H39 FanoutUTxOHashMismatch tested
H41 LowerBoundBeforeContestationDeadline tested
H42 FanoutNoLowerBoundDefined untested:validity-plumbing
H43 DepositNotSpent untested:unreachable
H44 DepositInputNotFound tested
H45 HeadInputNotFound untested:context-plumbing
H46 FailedCloseAny tested
H47 FailedCloseUnused tested
H48 FailedCloseUsed tested
H55 MissingCRSDatum untested:crs-plumbing
H56 MissingCRSRefInput untested:crs-plumbing
H57 PartialFanoutMembershipFailed tested
H58 PartialFanoutChangedParameters tested
H59 AccumulatorCommitmentHashMismatch tested
H60 FinalPartialFanoutMembershipFailed tested
H62 FinalPartialFanoutZeroOutputs tested
H63 PartialFanoutZeroOutputs tested
H64 PartialFanoutCannotBeLastBatch tested
H65 ChangedHeadAdaOverhead tested
H67 DepositDatumInvalid untested:code-not-asserted
H68 InvalidCRSDatum tested
H69 DepositNotFirstOutput tested
H70 DecrementZeroOutputs tested
H71 MustNotSpendOtherScripts tested
LEDGER
)

fail=0

# extracted (code, constructor) pairs, one "CODE NAME" per line
head_actual=$(sed -nE 's/^[[:space:]]*([A-Za-z]+) -> "(H[0-9]+)".*/\2 \1/p' "$HEAD_ERRORS" | sort)
dep_actual=$(sed -nE 's/^[[:space:]]*([A-Za-z]+) -> @"(D[0-9]+)".*/\2 \1/p' "$DEPOSIT_AK" | sort)
mirror_actual=$(sed -nE 's/^[[:space:]]*([A-Za-z]+) -> "(D[0-9]+)".*/\2 \1/p' "$DEPOSIT_MIRROR" | sort)

# 1. Aiken <-> Haskell mirror agreement.
if [ "$dep_actual" != "$mirror_actual" ]; then
  echo "check-error-codes: deposit.ak and Hydra.Contract.DepositError DISAGREE (- aiken, + mirror):"
  diff <(echo "$dep_actual") <(echo "$mirror_actual") || true
  fail=1
fi

# 2. No duplicate codes within a namespace.
dupes=$( (echo "$head_actual"; echo "$dep_actual") | cut -d' ' -f1 | sort | uniq -d)
if [ -n "$dupes" ]; then
  echo "check-error-codes: duplicate error codes: $dupes"
  fail=1
fi

# 3. Ledger drift: the extracted set must equal the ledger's (code, constructor) set.
actual=$( (echo "$head_actual"; echo "$dep_actual") | sort)
expected=$(echo "$ledger" | awk '{print $1, $2}' | sort)
if [ "$actual" != "$expected" ]; then
  echo "check-error-codes: the error-code set DRIFTED from the ledger (- ledger, + sources):"
  diff <(echo "$expected") <(echo "$actual") || true
  echo "Update the ledger in this script: a new code needs a test referencing its"
  echo "constructor (status: tested) or a reviewed exclusion (status: untested:<tag>)."
  fail=1
fi

# 4. Exclusion tags come from the documented vocabulary above: a typo'd tag would
# otherwise pass as a reviewed exclusion and drop the row out of the debt report.
KNOWN_TAGS="catch-all datum-plumbing context-plumbing validity-plumbing crs-plumbing variant-gap code-not-asserted unreachable"
while read -r code name status; do
  case "$status" in
    tested) ;;
    untested:*)
      tag=${status#untested:}
      if ! printf '%s\n' $KNOWN_TAGS | grep -qxF -- "$tag"; then
        echo "check-error-codes: $code $name has unknown exclusion tag '$tag' (known: $KNOWN_TAGS)."
        fail=1
      fi
      ;;
    *)
      echo "check-error-codes: $code $name has unrecognised status '$status' (want 'tested' or 'untested:<tag>')."
      fail=1
      ;;
  esac
done <<< "$ledger"

# 5. Every code is raised somewhere; 6. ledger statuses are accurate.
untested_report=""
while read -r code name status; do
  case "$code" in
    H*)
      if ! grep -rqw "errorCode $name" "${RAISE_DIRS[@]}"; then
        echo "check-error-codes: $code $name is never raised (no 'errorCode $name' under ${RAISE_DIRS[*]}): delete it."
        fail=1
      fi
      ;;
    D*)
      if ! grep -q "toErrorCode($name)" "$DEPOSIT_AK"; then
        echo "check-error-codes: $code $name is never raised (no 'toErrorCode($name)' in $DEPOSIT_AK): delete it."
        fail=1
      fi
      ;;
  esac
  if grep -rqE --include='*.hs' "toErrorCode[[:space:]]*\(?[[:space:]]*$name\b" "${TEST_DIRS[@]}"; then
    if [ "$status" != tested ]; then
      echo "check-error-codes: $code $name is asserted by a test but the ledger says '$status': promote it to 'tested'."
      fail=1
    fi
  else
    if [ "$status" = tested ]; then
      echo "check-error-codes: $code $name claims 'tested' but no test asserts it (no 'toErrorCode $name'): add a test or record an exclusion."
      fail=1
    fi
    untested_report+="  $code $name (${status#untested:})"$'\n'
  fi
done <<< "$ledger"

if [ "$fail" -ne 0 ]; then
  exit 1
fi

total=$(echo "$ledger" | wc -l)
untested=$(printf '%s' "$untested_report" | grep -c . || true)
echo "check-error-codes: OK: $total codes, $((total - untested)) asserted by tests, $untested documented exclusions:"
printf '%s' "$untested_report"
