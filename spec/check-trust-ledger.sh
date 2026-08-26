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
# It gates two layers:
#   BRIDGE  the `spec => extracted-reference` trusted base (ReferenceBridge/RefReflection), itemised in
#           the table below.
#   MODEL   the abstract model's own axioms: every postulate of Prelude/Setup/OnChain/OffChain/Security/
#           Solvency, as a name set, plus the FULL SIGNATURES of the `Assumptions` fields the solvency
#           theorem is parameterised over. The signatures are gated and not just the names because those
#           four are where a strengthening rather than an addition does the damage: relax
#           `κ#-pair-inj` to drop its hypothesis and `hash` becomes injective on nothing, every ℍ
#           equal, and the solvency theorem vacuous, all while still typechecking.
#
# Residual limit, stated rather than papered over: the model layer gates the postulate name SET, so it
# catches a new axiom but not a strengthened type on an existing one. Their semantics live in the
# spec's "What the formalisation assumes" appendix; the four that carry the solvency argument are the
# ones gated by signature here.
#
# Bridge-layer trust ledger (what each trusted item assumes; the HeadValidatorAgreement test covers each
# against the real validator/crypto where constructible).
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
SOLVENCY=src/Hydra/Protocol/Solvency.lagda.typ
MODEL_FILES=(
  src/Hydra/Protocol/Prelude.agda
  src/Hydra/Protocol/Setup.lagda.typ
  src/Hydra/Protocol/OnChain.lagda.typ
  src/Hydra/Protocol/OffChain.lagda.typ
  src/Hydra/Protocol/Security.lagda.typ
  "$SOLVENCY"
)

# The postulate scanner, shared by both layers. Emits every declared name of every `postulate`, in the
# single-line (`postulate name : T`) and block forms, including the second and later names of a
# multi-name declaration (`one two : T`) which a per-line `print $1` silently dropped. Names may
# contain unicode arrows (`noMint→ref`), so a declaration is recognised by its first token NOT being an
# operator/binder, which is what distinguishes it from a wrapped continuation of the previous
# signature. Leading indentation is tolerated so the literate `.lagda.typ` code blocks scan too.
postulate_names() {
  awk '
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
    line ~ /^[ \t]*postulate[ \t]*$/ { inblock = 1; next }
    line ~ /^[ \t]*postulate[ \t]+/ { sub(/^[ \t]*postulate[ \t]+/, "", line); emit(line); inblock = 0; next }
    inblock && line ~ /^[ \t]+[^ \t].*:/ { emit(line); next }
    inblock && line ~ /^[^ \t]/ { inblock = 0 }
  ' "$@" | sort -u
}

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
actual_postulates=$(postulate_names "$BR" "$RR")

expected_mocks=$(printf '%s\n' \
  closeCryptoOK contestCryptoOK fanoutCryptoOK incCryptoOK initPlacementOK recoverHashOK \
  | sort -u)
expected_postulates=$(printf '%s\n' \
  cidToNat mintEntryCount 'noMint→ref' 'participantSigned→ref' ptCodes refCodeOf signerCodes \
  | sort -u)

# ── MODEL layer ──────────────────────────────────────────────────────────────────────────────────
# Every postulate of the model modules, as a name set: a new axiom cannot enter the abstract model
# silently either.
actual_model=$(postulate_names "${MODEL_FILES[@]}")
expected_model=$(printf '%s\n' \
  AccCommitment accUTxO accUTxO-∅ accVerify accVerifyExclude accVerify-self accVerify-sound \
  AccWitness adaOf adaOf-+ᵛ aggKey AggSig aggSigOf aggSound applyTxs applyTxs-compose \
  applyTxs-nil burnedCount burnedValue bytes canonicalCRS# concat crsDatumHashAt Data \
  depositDatumCommitsHash G₁ _≟ℍ_ ℍ hash headTokenCount Liveness mintedCount msVfy noCommitHash \
  nonAdaOf nonAdaOf-+ᵛ noTxId outputs outsOf PartySig PartyVerified quantityOf quantityOfᴺ \
  quantityOfᴺ-+ᵛ recoveredMatchesDeposited Script setSize setSize-pos signerKeyHash sigUnforge \
  stQty sumValue sumValue-∅ _∖ᵘ_ _∪ᵘ_ _+ᵛ_ +ᵛ-assoc +ᵛ-cancelʳ +ᵛ-identityʳ VKey εᵘ εᵛ μHead \
  | sort -u)

# The solvency theorem's trust parameters, gated by FULL SIGNATURE (see the header): these are the
# four where weakening a hypothesis, rather than adding a field, is what would quietly make the
# theorem say nothing. Comments are stripped and whitespace normalised, so reformatting is free and
# any change to a binder, a hypothesis or a conclusion is not.
actual_assumptions=$(awk '
  /^record Assumptions/ { inrec = 1; next }
  inrec && /^```/       { inrec = 0 }
  inrec {
    line = $0
    sub(/--.*/, "", line)
    gsub(/^[ \t]+|[ \t]+$/, "", line)
    if (line == "" || line == "field") next
    gsub(/[ \t]+/, " ", line)
    print line
  }
' "$SOLVENCY")
expected_assumptions=$(cat <<'ASSUMPTIONS'
κ#-pair-inj : ∀ {x y r s : ℍ} → hash (x ‖ r) ≡ hash (y ‖ s) → r ≡ s
η#-inj : ∀ {a b : AccCommitment} → hash a ≡ hash b → a ≡ b
outs#-inj : ∀ {xs ys : List Output} → hash xs ≡ hash ys → xs ≡ ys
honest-certified : ∀ {snap : Snapshot} → Certified sys snap → HonestFacts snap
ASSUMPTIONS
)

# The solvency argument's reach over the on-chain transitions. `SolventReach` enumerates the steps it
# covers, and nothing about adding a transition to OnChain forces a step to be added here: the theorem
# would just silently say less. So the correspondence is enumerated instead of assumed. A bundle is
# either consumed by a step, or listed with the reason it is out of reach.
#
#   consumed:  InitValid CloseValid ContestValid DecrementValid FanoutValid IncrementValid
#   out of reach:
#     PartialFanoutValid       the batched fan-out path: value leaves the head across several
#     FinalPartialFanoutValid  transactions, so the single-step "head value = r₀ + committed" shape
#                              does not apply and a multi-batch generalisation is future work
#     ClaimValid ClaimTxValid  νDeposit arms; they govern the deposit UTxO, not the head output whose
#     RecoverValid             value this invariant tracks (the increment step consumes their effect
#                              through IncrementValid instead)
#     BurnValid                the μHead burn arm, reached only via the fan-out family above
ONCHAIN=src/Hydra/Protocol/OnChain.lagda.typ
actual_reach=$(grep -oE "OC\.[A-Za-z]+Valid" "$SOLVENCY" | sed 's/^OC\.//' | sort -u)
expected_reach=$(printf '%s\n' \
  CloseValid ContestValid DecrementValid FanoutValid IncrementValid InitValid | sort -u)
out_of_reach=$(printf '%s\n' \
  BurnValid ClaimTxValid ClaimValid FinalPartialFanoutValid PartialFanoutValid RecoverValid | sort -u)
all_bundles=$(grep -oE "^record [A-Za-z]+Valid" "$ONCHAIN" | awk '{print $2}' | sort -u)

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
if [ "$actual_model" != "$expected_model" ]; then
  echo "check-trust-ledger: the MODEL-layer postulate set DRIFTED from the documented ledger (- expected, + actual):"
  diff <(echo "$expected_model") <(echo "$actual_model") || true
  fail=1
fi
if [ "$actual_reach" != "$expected_reach" ]; then
  echo "check-trust-ledger: the set of validity bundles the solvency steps consume DRIFTED (- expected, + actual):"
  diff <(echo "$expected_reach") <(echo "$actual_reach") || true
  fail=1
fi
if [ "$(printf '%s\n' "$expected_reach" "$out_of_reach" | sort -u)" != "$all_bundles" ]; then
  echo "check-trust-ledger: an OnChain validity bundle is neither consumed by a solvency step nor listed"
  echo "as out of reach (- accounted for, + declared in OnChain):"
  diff <(printf '%s\n' "$expected_reach" "$out_of_reach" | sort -u) <(echo "$all_bundles") || true
  fail=1
fi
if [ "$actual_assumptions" != "$expected_assumptions" ]; then
  echo "check-trust-ledger: the solvency theorem's Assumptions signatures DRIFTED (- expected, + actual):"
  diff <(echo "$expected_assumptions") <(echo "$actual_assumptions") || true
  fail=1
fi
if [ "$fail" -ne 0 ]; then
  echo "Update the trust-ledger table in this script's header comment and the expected_* lists."
  exit 1
fi
echo "check-trust-ledger: OK: bridge layer = $(echo "$expected_mocks" | wc -l) Ops mocks + $(echo "$expected_postulates" | wc -l) postulates;" \
     "model layer = $(echo "$expected_model" | wc -l) postulates + $(echo "$expected_assumptions" | wc -l) gated Assumptions signatures;" \
     "solvency reaches $(echo "$expected_reach" | wc -l) of $(echo "$all_bundles" | wc -l) validity bundles."
