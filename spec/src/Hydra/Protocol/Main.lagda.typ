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
-- The machine-checked §7 proof terms (typecheck-only; the rendered §7 shows the model + statements).
import Hydra.Protocol.SecurityProofs
-- Extractable decidable reference checker + the bridge proving it reflects the on-chain
-- validity bundles (Tier 2 differential-testing; not rendered in the document).
import Hydra.Protocol.Reference
import Hydra.Protocol.ReferenceBridge
-- On-chain coverage / non-stuckness: a reachability inductive over datums + the obligation that the
-- terminal fanout bundle is inhabited for the reachable empty head (typecheck-only; the dual of the
-- soundness corpus, ruling out an over-strict finalize conjunct like `FanoutValid.outputsPositive`).
import Hydra.Protocol.OnChainCoverage
```

#include "Introduction.typ"
#include "Overview.typ"
#include "Preliminaries.typ"
#include "Setup.typ"
#include "OnChain.typ"
#include "OffChain.typ"
#include "Security.typ"

#pagebreak()

= Agda formalisation <agda-appendix>

The specification above is _literate Agda_: every definition, validity bundle, and proof is
machine-checked by Agda (`agda Main.lagda.typ`) as part of building this document. To keep the body
readable, the rendered Agda is collected here rather than shown inline; each block appears under the
section it supports, in document order, and the body links here in place. This appendix is the
rendered form of the same typechecked source, not a copy of it.

== Reading the Agda (for Haskell programmers) <sec:reading-agda>

The definitions, transition rules and security proofs in this document are real
Agda code, type-checked as part of the build (`nix build .#spec`); the prose and
math render alongside them. If you know Haskell but are new to Agda, this short
glossary maps the Agda idioms you will meet to their Haskell intuition.

#dparagraph[Assumptions vs. proofs.] The single most important thing to know: a
block introduced by the Agda keyword `postulate` is an _assumption_ (an axiom the
spec takes on trust, such as the ledger semantics or the cryptographic
unforgeability of signatures), _not_ something proved. Everything else, given by a
defining equation, is a definition or a type-checked proof. To audit exactly what
the specification assumes, search the sources for `postulate`. In the rendered PDF a
`postulate` block looks like any other code, so this keyword is the thing to watch
for.

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
bridge lemma is named `==-sound`.

#dparagraph[Pattern matching.] Definitions are equations over constructors, as in
Haskell. `with e` adds `e` as an extra argument to split on, refining the goal by
what `e` turned out to be. An _absurd pattern_ `()` discharges a case that cannot
occur because its type has no constructor (for example, a membership proof in the
empty list), with no right-hand side.

#dparagraph[Recurring types.] `ℕ` the naturals; `List`/`_∷_`/`[]`, `Maybe`/`just`/
`nothing` as in Haskell; `Fin n` the naturals below `n` (a bounded index); `Vec A n`
a length-`n` list; `ℙ A` a finite set and `_⇀_` a finite map (from the set-theory
library); `_∈ˡ_` list membership and `_⊆ˡ_` list inclusion. Most unicode names are
ordinary identifiers; search @sec:prel for their definitions.

#dparagraph[Relations as transitions.] The on-chain state machine is an
inductively-defined relation `_⟶⟨_⟩_` (a datum steps to a datum under a redeemer),
and the security model uses a step relation `_⟶ˢ_` with its reflexive-transitive
closure `Reachable`. A value of such a type is a _proof_ that a particular step (or
run) is allowed; the proofs in @sec:security are inductions over these.

#dparagraph[Validity bundles.] A validator's requirements are written as a `record`
(e.g. `CloseValid`) with one named field per checkable condition (`step`, `deadlineOK`,
`valuePreserved`, …) - like a Haskell record of proofs. The bundle is inhabited exactly for
valid transactions, and a proof reads a condition by its field name (`b .deadlineOK`) rather
than by tuple position. (The lowercase `closeValid ctx d d' ct` is the predicate that returns
this record for well-shaped datums, and is empty otherwise.)

#dparagraph[Extraction.] The decidable checker in `Reference.agda` is compiled to
Haskell by Agda's GHC backend (MAlonzo) and run in the `hydra-tx` test suite as a
second oracle against the real Plutus validator (see @sec:security).


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
