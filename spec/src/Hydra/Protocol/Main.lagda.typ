#import "/template.typ": *
#import "/macros.typ": *

#show: body => hydra-spec(
  title: "Hydra HeadV2 Specification: Coordinated Head protocol",
  subtitle: "DRAFT",
  authors: (
    (name: "Sebastian Nagel", email: "sebastian.nagel@iohk.io"),
    (name: "Sasha Bogicevic", email: "sasha.bogicevic@iohk.io"),
    (name: "Franco Testagrossa", email: "franco.testagrossa@iohk.io"),
    (name: "Daniel Firth", email: "daniel.firth@iohk.io"),
    (name: "Noon van der Silk", email: "noon.vandersilk@iohk.io"),
    (name: "Veronika Romashkina", email: "veronika.romashkina@iohk.io"),
  ),
  body,
)

```
module Hydra.Protocol.Main where

import Hydra.Protocol.Prelude
import Hydra.Protocol.Introduction
import Hydra.Protocol.Overview
import Hydra.Protocol.Preliminaries
import Hydra.Protocol.Setup
import Hydra.Protocol.OnChain
import Hydra.Protocol.OffChain
import Hydra.Protocol.Security
-- The machine-checked §7 proof terms (rendered: the statements appear under §7 "Machine-checked
-- results", included below after Security; the proof bodies are typechecked but hidden).
import Hydra.Protocol.SecurityProofs
-- Extractable decidable reference checker + the bridge proving it reflects the on-chain
-- validity bundles (Tier 2 differential-testing; not rendered in the document).
import Hydra.Protocol.Reference
import Hydra.Protocol.ReferenceBridge
-- Extractable decidable reference for the OFF-CHAIN HeadLogic figure (Tier 2 differential, off-chain
-- side; typecheck-only here, extracted via regenerate.sh, not rendered in the document).
import Hydra.Protocol.OffChainReference
-- On-chain coverage / non-stuckness + safety invariants (rendered: included below after OnChain).
import Hydra.Protocol.OnChainCoverage
```

#include "Introduction.lagda.typ"
#include "Overview.lagda.typ"
#include "Preliminaries.lagda.typ"
#include "Setup.lagda.typ"
#include "OnChain.lagda.typ"
#include "OnChainCoverage.lagda.typ"
#include "OffChain.lagda.typ"
#include "Security.lagda.typ"
#include "SecurityProofs.lagda.typ"

#pagebreak()

= Agda formalisation <agda-appendix>

The specification above is _literate Agda_: every definition, validity bundle, and proof is
machine-checked by Agda (`agda Main.lagda.typ`) as part of building this document. To keep the body
readable, the rendered Agda is collected here rather than shown inline; each block appears under the
section it supports, in document order, and the body links here in place. The appendix is
deliberately restricted to the _machine-checked theorem statements_ (the coverage, safety,
value-conservation and security results of @sec:onchain-theorems, @sec:offchain-theorems and
@sec:security-theorems) together with the property types they inhabit. Everything else — the
datum/redeemer types, the transition relations, the validity bundles, the helper definitions, every
`postulate`, and all proof bodies — is typechecked as part of the build but not rendered, as are the
typecheck-only modules `Prelude`, `Reference`, `ReferenceBridge`, `RefReflection` and
`OffChainReference`. @sec:assumption-inventory lists what the formalisation takes on trust.

== Reading the Agda (for Haskell programmers) <sec:reading-agda>

The definitions, transition rules and security proofs in this document are real
Agda code, type-checked as part of the build (`nix build .#spec`); the prose and
math render alongside them. If you know Haskell but are new to Agda, this short
glossary maps the Agda idioms you will meet to their Haskell intuition.

#dparagraph[Assumptions vs. proofs.] The single most important thing to know: a
block introduced by the Agda keyword `postulate` is an _assumption_ (an axiom the
spec takes on trust, such as the ledger semantics or the cryptographic
unforgeability of signatures), _not_ something proved. Everything else, given by a
defining equation, is a definition or a type-checked proof. Postulate blocks are
not rendered in this document (only theorem statements are); the full inventory
lives in @sec:assumption-inventory. Postulates are also not the only assumptions:
some enter as module _hypotheses_ or constructor _premises_ (the honest-behaviour
premises of `signHonest` in @sec:security, the `ContestBound` hypotheses of
@sec:onchain-theorems, per-instance hypotheses like `ηEq`).
@sec:assumption-inventory collects all of these in one place; auditing the
sources means searching for `postulate` _and_ reading the flagged premises.

#dparagraph[Types, values and proofs.] Agda is dependently typed: types may mention
values. `Set` is the type of (small) types, Haskell's kind `Type`. A function type
`(x : A) → B x` is a _dependent_ function whose result type may depend on the
argument; `∀ {x} → B` is the same with `x` an _implicit_ argument Agda infers, like
an inferred `forall`. Propositions are types and a proof is a value of that type, so
a function `P → Q` is read both as "a function" and as "P implies Q".

#dparagraph[Logic and data.] `_×_` is a pair, read as logical _and_; `_⊎_` is
`Either`, read as _or_ (constructors `inj₁`/`inj₂`); `⊥` is the empty type
(`False`; `⊥-elim` is "from a contradiction, anything"); `⊤` is unit. `Σ[ x ∈ A ] B`
is a dependent pair (a value `x : A` together with a `B`, often read "there exists
an `x` such that B"); Agda `record`s are sugar for nested `Σ`.

#dparagraph[Equality.] `_≡_` is _propositional_ equality: `a ≡ b` is the type of
_proofs_ that `a` and `b` are the same value, distinct from a `Bool`-valued test
`a == b`. `refl` proves `a ≡ a`; `sym`/`trans`/`cong` are symmetry, transitivity and
congruence; `subst P eq px` rewrites a proof `px : P a` along `eq : a ≡ b` into a
`P b`. Where the spec runs a `Bool` equality and needs to turn it into `≡`, the
bridge lemma is named `==-sound` (proved in the typecheck-only `RefReflection`
module, so it does not appear in this appendix).

#dparagraph[Pattern matching.] Definitions are equations over constructors, as in
Haskell. `with e` adds `e` as an extra argument to split on, refining the goal by
what `e` turned out to be. An _absurd pattern_ `()` discharges a case that cannot
occur because its type has no constructor (for example, a membership proof in the
empty list), with no right-hand side.

#dparagraph[Recurring types.] `ℕ` the naturals; `List`/`_∷_`/`[]`, `Maybe`/`just`/
`nothing` as in Haskell; `Fin n` the naturals below `n` (a bounded index); `Vec A n`
a length-`n` list; `ℙ A` a finite set and `_⇀_` a finite map (from the set-theory
library); `_∈ˡ_` list membership and `_⊆ˡ_` list inclusion. Most unicode names are
ordinary identifiers, defined in the typecheck-only `Prelude` module; @sec:prel is
the rendered counterpart for the mathematical notation.

#dparagraph[Relations as transitions.] The on-chain state machine is an
inductively-defined relation `_⟶⟨_⟩_` (a datum steps to a datum under a redeemer),
and the security model uses a step relation `_⟶ˢ_` with its reflexive-transitive
closure `Reachable`. A value of such a type is a _proof_ that a particular step (or
run) is allowed; the proofs in @sec:security are inductions over these.

#dparagraph[Validity bundles.] A validator's requirements are written as a `record`
(e.g. `CloseValid`) with one named field per checkable condition (`step`, `deadlineOK`,
`valuePreserved`, …) - like a Haskell record of proofs. The bundle is inhabited exactly for
valid transactions, and a proof reads a condition by its field name (`CloseValid.deadlineOK b`)
rather than by tuple position. (The lowercase `closeValid ctx d d' ct` is the predicate that returns
this record for well-shaped datums, and is empty otherwise.)

#dparagraph[Extraction.] The decidable checker in `Reference.agda` is compiled to
Haskell by Agda's GHC backend (MAlonzo) and run in the `hydra-tx` test suite as a
second oracle against the real Plutus validator (see @sec:security).

== What the formalisation assumes <sec:assumption-inventory>

The trust base, in five families:

- *Ledger and crypto primitives* (the typecheck-only `Prelude`, postulates):
  `hash`, `bytes`/`concat`, the multisignature verifier `msVfy`, the `Value`
  algebra (`_+ᵛ_`/`_≤ᵛ_`/`εᵛ` with commutative-monoid and order laws) and its
  projections (`adaOf`, `nonAdaOf`, `quantityOf`, `stQty`, `headTokenCount`);
  the off-chain ledger `applyTxs` with its nil and compositionality laws (§6).
- *Accumulator laws* (@sec:on-chain): `accUTxO`/`accVerify`/`accVerifyExclude`
  with the specifying laws `accUTxO-∅`, `accVerify-sound`/`-complete`/`-self`
  and `setSize`; the KZG construction itself is not modelled.
- *On-chain search postulates* (@sec:on-chain): `burnedValue`, `burnedCount`,
  `mintedCount`, `μHead`, `signerKeyHash` - witnesses over the opaque value and
  key-set models - plus the context lookups the `Context` model does not expose:
  `depositCommitsHashOf` (the increment's recomputed commit-set hash, from the
  claimed deposit's datum) and `crsDatumHashAt`/`canonicalCRS#` (the CRS
  reference-input datum hash and the canonical trusted-setup constant the
  fanout bundles bind it to).
- *Security-model assumptions* (@sec:security): per-signature EUF-CMA
  (`sigUnforge`) plus the aggregation scheme's decomposition (`aggSound`), from
  which `ms-unforgeable` is _derived_; `aggKey`/`aggSigOf`/`PartyVerified` and
  the glue `outsOf`; the honest-behaviour premises of the `signHonest`
  constructor; and per-instance hypotheses supplied at each use (the `ηEq`
  accumulator-commitment hypothesis of `reflects`, the `ξ ≡ aggSigOf` hypothesis
  of the `*-certified` family). The `ContestBound` module hypotheses of
  @sec:onchain-theorems are of the same kind.
- *Bridge layer* (typecheck-only `ReferenceBridge`/`RefReflection`): 6 injected
  const-true mocks (crypto/accumulator/hash conjuncts the differential covers
  against the real validator) and 7 encoding/faithfulness postulates, enumerated
  and drift-checked by `spec/check-trust-ledger.sh` - the build fails if this
  set changes without the ledger being updated.

Everything else is a definition or a machine-checked proof.

#agda-appendix-mode.update(true)

#context {
  let blocks = query(<agda-src>)
  let cur = none
  for b in blocks {
    if b.value.secnum != cur {
      heading(level: 2, b.value.sec)
      cur = b.value.secnum
    }
    raw(b.value.src, lang: "agda", block: true)
  }
}

#bibliography("/short.bib", style: "springer-basic")
