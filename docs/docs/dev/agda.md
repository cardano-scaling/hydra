# Formal specification in Agda

The Hydra Head protocol specification is written in *literate Agda* rendered by
[Typst](https://typst.app): every definition, validity condition and security
proof shown in the [specification PDF](./specification.md) comes from source
files that Agda type-checks on every build. On top of that, a decidable core of
the spec is extracted to Haskell and *differentially tested* against the real
Plutus validators and the real `hydra-node` head logic. This page is an
orientation: what is proved, what is tested, what is assumed, and where the
machinery lives.

## Why literate Agda + Typst

A specification that lives next to the code but is only prose will drift. Here
the prose, the math and the machine-checked definitions come from one set of
sources ([`spec/src/Hydra/Protocol/`](https://github.com/cardano-scaling/hydra/tree/master/spec/src/Hydra/Protocol)):
`agda Main.lagda.typ` type-checks the whole document and Typst renders the same
files to the PDF. Two kinds of code fences exist: rendered ones (collected into
the PDF's Agda appendix) and bare ones, which are typechecked but never rendered - the latter
carry the bulk of the model: imports and proof plumbing, but also the whole §5 datum/redeemer,
transition-relation and validity-bundle layer and the §6 handler model. See
[`spec/README.md`](https://github.com/cardano-scaling/hydra/tree/master/spec)
for the authoring details.

## Module map

Three groups of modules, one build:

| Group | Modules | Role |
| --- | --- | --- |
| Rendered (the document) | `Introduction`, `Overview`, `Preliminaries`, `Setup`, `OnChain`, `OnChainCoverage`, `OffChain`, `Security`, `SecurityProofs`, `Solvency` | The prose. Their ```` ```agda ```` fences are collected into the PDF's Agda appendix, which is restricted to the machine-checked theorem statements (`OnChainCoverage`, `SecurityProofs`, `Security`, `OffChain`, `Solvency` - including the global solvency invariant, whose increment case consumes the deposit-identity binding); the datum/redeemer types, transition relations and validity bundles in `Preliminaries`/`Setup`/`OnChain` are typechecked but deliberately not rendered |
| Typecheck-only | `Prelude`, `ReferenceBridge`, `RefReflection`, `SolvencyCounterModel` | The abstract trust base, the bridge proofs, and the pre-fix deposit-aliasing counter-model; verified on every build, never rendered |
| Extractable | `Reference`, `OffChainReference` | Decidable checkers, self-contained over `Agda.Builtin` types so the extracted Haskell stays small |

## The trust story

Three tiers, from strongest to weakest guarantee:

1. **Proved.** The §7 security results (consistency, soundness, completeness,
   the reachability invariant, no-settlement-without-unanimity) and the
   on-chain coverage/safety theorems (non-stuckness, value conservation,
   bounded contest window) are closed Agda proofs over the abstract model.
   Multisignature unforgeability is *derived* from per-signature EUF-CMA plus
   the aggregation scheme's decomposition, not assumed monolithically.

2. **Assumed.** The model bottoms out in an enumerated trust base: ledger and
   crypto primitives (hashing, the multisignature verifier, the `Value`
   algebra), the accumulator laws (the KZG construction itself is not
   modelled), a small set of on-chain "search" postulates over the opaque
   value/key models, and the honest-behaviour premises of the security model.
   The PDF appendix section **"What the formalisation assumes"** inventories
   all of it; the reading rule is that `postulate` means assumption, everything
   else is proved.

3. **Differentially tested.** Where the abstract model meets the real code,
   the trusted base is *fixed and machine-enforced* by
   [`spec/check-trust-ledger.sh`](https://github.com/cardano-scaling/hydra/blob/master/spec/check-trust-ledger.sh),
   which fails the spec build on any drift. It gates three things:

   - the **bridge** layer: exactly 6 injected const-true mocks (crypto and
     accumulator conjuncts the tests cover against the real primitives instead)
     and 7 encoding/faithfulness postulates;
   - the **model** layer: every postulate of the model modules as a name set, so
     a new axiom cannot enter quietly either, plus the 4 `Assumptions` fields the
     solvency theorem is parameterised over by *full signature*. Signatures for
     those four because they are where a strengthening rather than an addition
     does the damage: relax `κ#-pair-inj` to drop its hypothesis and `hash`
     becomes injective on nothing, every hash equal, and the theorem vacuous,
     all while still type-checking;
   - the **escape hatches** that are not postulates at all: `TERMINATING` and
     friends, `primTrustMe`, `REWRITE`, and the `OPTIONS` flags that switch off
     termination, positivity or coverage checking. Agda's own `--safe` would be
     the obvious tool and is unavailable, because `--safe` *bans postulates* and
     this specification rests on 63 of them by design (and `--safe` is contagious
     through imports, so nothing downstream of `Prelude` could carry it). The
     enumerated list buys the same protection.

   It also gates which on-chain transitions the solvency argument reaches: every
   `*Valid` bundle must be consumed by a step or listed with the reason it is
   out of reach, so a new transition cannot narrow the theorem silently.

The bridge direction is *completeness*: `ReferenceBridge.agda` proves that a
spec-valid transaction makes the extracted checker accept, so a
reference-reject implies a spec-reject. Joined with the agreement tests'
`reference === validator`, a spec-valid transaction is accepted by the real
validator and vice versa, modulo the documented mocks, each of which the
tests exercise with real crypto (Ed25519 signatures, BLS/KZG pairings) in both
accept and reject directions.

## The extraction pipeline

`Reference.agda` and `OffChainReference.agda` are compiled by Agda's MAlonzo
backend into
[`hydra-agda/generated/`](https://github.com/cardano-scaling/hydra/tree/master/hydra-agda),
which is committed. The Agda sources fix the name of every export with
`COMPILE GHC … as …` (their "extraction surface" sections), and the
hand-written shims (`Hydra.Agda.Reference`, `Hydra.Agda.OffChainReference`)
bind those names, adding the documentation. MAlonzo's own mangled names carry a
definition-order index that any additive edit renumbers, so binding them would
make the shims need hand-editing after unrelated changes, and would let two
checkers of the same Haskell type be swapped silently.

Regeneration is manual (`hydra-agda/regenerate.sh`, inside `nix develop .#spec`)
and CI-enforced from two sides: the `hydra-agda-generated` flake check runs that
same script hermetically and fails on any diff against the committed tree, and
`hydra-agda`'s own test-suite pins each exported name to observable behaviour,
which no type-level check can do for checkers that share a type.

`regenerate.sh` also compares what it generated against the `other-modules` list
in `hydra-agda.cabal`'s library stanza, which is maintained by hand. Without that,
a regeneration that added or dropped a module would pass the drift check and
surface much later as a confusing "module not found" during the build.

## The agreement tests

Two layers bind the extracted spec to the real implementation:

- **On-chain**:
  [`Hydra.Tx.Contract.HeadValidatorAgreement`](https://github.com/cardano-scaling/hydra/blob/master/hydra-tx/test/Hydra/Tx/Contract/HeadValidatorAgreement.hs)
  (hydra-tx) runs the *real* `Head.headValidator` (and the compiled
  `deposit.ak` as UPLC) and the extracted reference on the same directly
  constructed inputs (no transactions, no mutation corpus) and asserts
  `reference === validator` across every transaction family, in both the
  accept and reject directions. The crypto the reference mocks is exercised
  for real: valid and invalid Ed25519 snapshot signatures (including the
  commit/decommit output-set hashes bound into the signed message, and the
  deposit transaction id bound into the commit digest), BLS/KZG
  membership proofs against the canonical CRS, and the canonical-CRS datum
  binding (`InvalidCRSDatum`).
  The differential and mutation layers only see the input families somebody
  constructed, so `spec/check-error-codes.sh` (CI: `checks.error-codes`) keeps
  the reject-path coverage honest: every error code a validator can raise must
  be asserted by a test (via `toErrorCode`) or carry a reviewed exclusion in
  its ledger, and dead codes fail the build. Deleting a dead constructor leaves a
  hole in the numbering, and the ledger's `RETIRED` list keeps that hole: the code
  *string* is what the mutation corpus and anything outside the repo match on, so
  a later constructor dropping into a retired number would inherit its meaning.
- **Off-chain**:
  [`Hydra.OffChainAgreementSpec`](https://github.com/cardano-scaling/hydra/blob/master/hydra-node/test/Hydra/OffChainAgreementSpec.hs)
  and `Hydra.OffChainLeaderSpec` (hydra-node) bind the extracted §6 handler
  decisions (snapshot-signing eligibility, decommit recording, deposit status,
  ack counting, contest eligibility, leader election) against the real
  `HeadLogic.update` outcomes.

## How do I ...

| Task | What to do |
| --- | --- |
| Build the PDF | `nix build .#spec`, or `./build.sh` inside `nix develop` (output: `spec/_build/hydra-spec.pdf`) |
| Type-check only | `agda src/Hydra/Protocol/Main.lagda.typ` in `spec/` |
| Change a validator condition | Update the section + `*Valid` bundle in `OnChain.lagda.typ`, mirror in `Reference.agda`, prove in `ReferenceBridge.agda`, `regenerate.sh`, extend the shim + `HeadValidatorAgreement`; see the checklist in [`spec/README.md`](https://github.com/cardano-scaling/hydra/tree/master/spec) |
| Change a head-logic handler | Update the §6 arm (+ figure) in `OffChain.lagda.typ`, mirror in `OffChainReference.agda`, bind in the hydra-node agreement tests |
| Change the datum shape | `HeadDatum` in `OnChain.lagda.typ` + `state-fields` in `diagrams.typ`; `check-refs.sh` catches constructor drift |
| Add a mock/postulate to the bridge | The build fails until `check-trust-ledger.sh`'s ledger is updated, which is the point |
| Add a postulate to the model | Same script, model layer: add the name to its list (and if it is an `Assumptions` field, its full signature) |
| Reach for `TERMINATING`/`primTrustMe` | Don't; the same script rejects them by name. If a definition genuinely needs one, that is a design discussion, not a pragma |
| Add a validator error code | Pick an unused number (`RETIRED` ones are burned), assert it from a test via `toErrorCode`, or record a reviewed exclusion in `check-error-codes.sh` |

CI gates: `checks.spec` (Agda typecheck, reference/diagram lints, trust-ledger
drift check, PDF render) and `checks.hydra-agda-generated` (extraction
freshness). The agreement tests run in the ordinary package test suites.

## Pointers

- The PDF appendix: *"Reading the Agda (for Haskell programmers)"* (a
  Haskell-to-Agda glossary) and *"What the formalisation assumes"* (the full
  trust-base inventory).
- [`spec/README.md`](https://github.com/cardano-scaling/hydra/tree/master/spec)
  covers building, authoring, and the keep-in-sync checklist.
- [`spec/check-trust-ledger.sh`](https://github.com/cardano-scaling/hydra/blob/master/spec/check-trust-ledger.sh)
  is the enumerated bridge trust ledger.
- The three agreement test modules named above, whose headers document the
  per-conjunct coverage.
