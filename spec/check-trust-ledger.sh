#!/usr/bin/env bash
# Trust-ledger drift check (bridge layer).
#
# The machine-checked `spec ⇒ extracted-reference` bridge rests on a FIXED, enumerated trusted base:
#   (a) the injected `Ops` boundaries: the fields of the mock records the reference delegates to
#       (crypto / accumulator / value-map conjuncts), whatever they are bound to, and
#   (b) extraction-faithfulness / encoding postulates (hash/out-ref encodings and the
#       participant / no-mint faithfulness assumptions).
#
# This script extracts that set from the Agda sources and FAILS if it drifts from the ledger below.
# So a NEW mock or postulate cannot enter the trusted base silently: adding one fails the build until
# both the EXPECTED_* lists and this ledger table are updated.
#
# Bridge-layer trust ledger (what each trusted item assumes; the HeadValidatorAgreement test covers each
# against the real validator/crypto where constructible). SCOPE: this gates ReferenceBridge/RefReflection
# only; the abstract model's own axioms (Prelude value/crypto laws, accumulator laws, §7 assumptions) are
# inventoried in the spec's "What the formalisation assumes" appendix section, not drift-checked here.
#   Ops mocks (const-true boundaries the reference delegates). The snapshot signature is the 6-tuple
#   message cid‖v‖s‖η#‖δ#‖κ# (accumulator + decommit-/commit-output-set hashes):
#     closeCryptoOK    close snapshot signature + accumulator-commitment hash (real Ed25519 in the test,
#                      incl. a tampered-δ#/κ# reject)
#     incCryptoOK      increment/decrement snapshot signature incl. the recomputed commit-set hash
#                      (increment, DepositDatumInvalid) / decommit-set hash (decrement) (real Ed25519)
#     contestCryptoOK  contest snapshot signature, η binding, contest-once (real Ed25519)
#     fanoutCryptoOK   fanout KZG membership + value conservation + the canonical-CRS datum binding
#                      (crsBindOK; real BLS pairing, InvalidCRSDatum reject in the test)
#     recoverHashOK    recover recovered-outputs serialisation hash (empty-deposit case)
#     initPlacementOK  μHead seed-spent + token placement (real validateTokensMinting)
#   Postulates (typecheck-only):
#     cidToNat, refCodeOf                          head-id / out-ref → ℕ encodings (used with cong only)
#     signerCodes, ptCodes, participantSigned→ref  signer / PT-name encodings + overlap faithfulness
#     mintEntryCount, noMint→ref                   mint-entry count encoding + noMint faithfulness
set -euo pipefail
cd "$(dirname "$0")"

BR=src/Hydra/Protocol/ReferenceBridge.agda
RR=src/Hydra/Protocol/RefReflection.agda

# (a) Injected Ops boundaries: every field of every record VALUE the bridge builds.
#
# Keyed on `= record {` (an assignment whose right-hand side is a record literal), which is what
# distinguishes the injected mocks from the record PATTERNS the `*Valid → ref` clauses destructure
# on their left-hand side. Newlines are flattened first so a multi-line record literal cannot hide a
# field from a line-based scan, and each field name is read as the first token of a `;`-separated
# component, so a field whose value itself contains `=` (say `λ x → x == y`) cannot be mistaken for
# one. Deliberately NOT keyed on the const-true spelling `= λ … → true`: it is the injection that
# widens the trusted base, and `= const true`, `= alwaysTrue` or any other spelling injects just as
# much. (Today all six happen to be written as const-true lambdas.)
actual_mocks=$(tr '\n' ' ' < "$BR" |
  grep -oE '= record[[:space:]]+\{[^}]*\}' |
  sed -E 's/^= record[[:space:]]+\{//; s/\}$//' |
  tr ';' '\n' |
  awk 'NF {print $1}' |
  sort -u)

# (b) Postulated names, single-line (`postulate name : T`) and block (`postulate` then indented
# declarations). Every name of a declaration is emitted, not just the first: `postulate` permits
# `one two : T`, which the previous per-line `print $1`/`print $2` silently dropped (both names in
# the block form, the second onwards in the single-line form) - a hole big enough to add an
# assumption through. Names may themselves contain unicode arrows (`noMint→ref`), so a declaration
# is recognised by its first token not being an operator/binder, which is what distinguishes it
# from a wrapped continuation line of the previous type signature.
actual_postulates=$(awk '
  function emit(decl) {
    sub(/:.*/, "", decl)
    n = split(decl, names, /[ \t]+/)
    if (n == 0) return
    first = names[1] != "" ? names[1] : names[2]
    if (first ~ /^([-=<>]|→|⇒|∀|\(|\{|\[)/) return   # continuation of the previous signature
    for (i = 1; i <= n; i++) if (names[i] != "") print names[i]
  }
  { line = $0; sub(/--.*/, "", line) }
  line ~ /^[ \t]*$/ { next }
  line ~ /^postulate[ \t]*$/ { inblock = 1; next }
  line ~ /^postulate[ \t]+/ { sub(/^postulate[ \t]+/, "", line); emit(line); inblock = 0; next }
  inblock && line ~ /^[ \t]+[^ \t].*:/ { emit(line); next }
  inblock && line ~ /^[^ \t]/ { inblock = 0 }
' "$BR" "$RR" | sort -u)

expected_mocks=$(printf '%s\n' \
  closeCryptoOK contestCryptoOK fanoutCryptoOK incCryptoOK initPlacementOK recoverHashOK \
  | sort -u)
expected_postulates=$(printf '%s\n' \
  cidToNat mintEntryCount 'noMint→ref' 'participantSigned→ref' ptCodes refCodeOf signerCodes \
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
  echo "Update the trust-ledger table in this script's header comment and the EXPECTED_* lists."
  exit 1
fi
echo "check-trust-ledger: OK: the bridge-layer trusted base is the 6 documented Ops mocks + 7 documented postulates."
