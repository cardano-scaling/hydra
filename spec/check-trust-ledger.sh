#!/usr/bin/env bash
# C3.6 trust-ledger drift check.
#
# The machine-checked `spec ⇒ extracted-reference` bridge rests on a FIXED, enumerated trusted base:
#   (a) injected `Ops` mocks — the const-`true` boundaries the reference delegates (crypto / accumulator /
#       value-map conjuncts), and
#   (b) extraction-faithfulness / encoding postulates (builtin-arithmetic soundness, hash/out-ref encodings,
#       and the participant / no-mint faithfulness assumptions).
#
# This script extracts that set from the Agda sources and FAILS if it drifts from the documented ledger
# (agda-haskell-alignment.md, "Trust ledger"). So a NEW mock or postulate cannot enter the trusted base
# silently: adding one fails the build until both the EXPECTED_* lists here and the ledger are updated.
set -euo pipefail
cd "$(dirname "$0")"

BR=src/Hydra/Protocol/ReferenceBridge.agda
RR=src/Hydra/Protocol/RefReflection.agda

# (a) Ops mocks: every `<field> = λ … → true` const-true binding (ANY field in a record, first or
# not, single- OR multi-line; NOT record-pattern matches like `step =`, which don't bind `λ … → true`).
# Newlines are flattened first so a multi-line record literal cannot hide a mock from the line-based grep.
actual_mocks=$(tr '\n' ' ' < "$BR" | grep -oE '[a-zA-Z][a-zA-Z0-9]* = λ[^;{}]*→ true' | sed -E 's/ = λ.*//' | sort -u)

# (b) Postulated names, both single-line (`postulate name :`) and block (`postulate` then indented `name :`).
actual_postulates=$(awk '
  /^[ ]*--/ {next}
  /^postulate$/ {inblock=1; next}
  /^postulate[ ]+[^ ]/ {print $2; inblock=0; next}
  inblock && /^[ ]+[^ ]+ +:/ {print $1; next}
  inblock && /^[^ ]/ {inblock=0}
' "$BR" "$RR" | sort -u)

expected_mocks=$(printf '%s\n' \
  claimIncrementOK closeCryptoOK contestCryptoOK fanoutCryptoOK incCryptoOK initPlacementOK recoverHashOK \
  | sort -u)
expected_postulates=$(printf '%s\n' \
  '<ᴮ-sound' '==-sound' cidToNat mintEntryCount 'noMint→ref' 'participantSigned→ref' ptCodes refCodeOf signerCodes \
  | sort -u)

fail=0
if [ "$actual_mocks" != "$expected_mocks" ]; then
  echo "check-trust-ledger: the injected Ops-mock set DRIFTED from the documented ledger (- expected, + actual):"
  diff <(echo "$expected_mocks") <(echo "$actual_mocks") || true
  fail=1
fi
if [ "$actual_postulates" != "$expected_postulates" ]; then
  echo "check-trust-ledger: the postulate set DRIFTED from the documented ledger (- expected, + actual):"
  diff <(echo "$expected_postulates") <(echo "$actual_postulates") || true
  fail=1
fi
if [ "$fail" -ne 0 ]; then
  echo "Update spec/agda-haskell-alignment.md (the Trust ledger section) and the EXPECTED_* lists in this script."
  exit 1
fi
echo "check-trust-ledger: OK — trusted base is the 7 documented Ops mocks + 9 documented postulates."
