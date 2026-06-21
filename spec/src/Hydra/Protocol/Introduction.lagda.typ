```
module Hydra.Protocol.Introduction where
```

#import "/template.typ": *
#import "/macros.typ": *

= Introduction

This document specifies the 'Coordinated Hydra Head' protocol to be implemented
as the first version of Hydra Head on Cardano - *Hydra HeadV1*. The
protocol is derived from variants described in the original paper
@hydrahead20, but was further simplified to make a first implementation on
Cardano possible.

Note that the format and scope of this document is (currently) also inspired by
the paper and hence does not include a definition of the networking protocols or
concrete message formats. #todo[Add: network specification (message formats)] It
is structured similarly, but focuses on a single variant, and avoids
indirections and unnecessary generalizations. The document is kept in sync with
the reference implementation available on Github~@hydra-repo.

First, a high-level overview of the protocol and how it differs from legacy
variants of the Head protocol is given in @sec:overview. Relevant
definitions and notations are introduced in @sec:prel, while
@sec:setup describes protocol setup and assumptions. Then, the
actual on-chain transactions of the protocol are defined in
@sec:on-chain, before the off-chain protocol part specifies
behavior of Hydra parties off-chain and ties the knot with on-chain transactions
in @sec:offchain. At last, @sec:security gives the
security definition, properties and proofs for the Coordinated Head protocol.

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
the specification assumes, search the sources for `postulate`; the trusted base is
also summarised in `security-formalisation-plan.md`. In the rendered PDF a
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

#dparagraph[Validity bundles.] A validator's requirements are written as a big `_×_`
of conjuncts (`(step) × (deadline ok) × (value preserved) × …`); each conjunct is one
checkable condition, and the bundle type is inhabited exactly for valid transactions.

#dparagraph[Extraction.] The decidable checker in `Reference.agda` is compiled to
Haskell by Agda's GHC backend (MAlonzo) and run in the `hydra-tx` test suite as a
second oracle against the real Plutus validator (see @sec:security and
`agda-haskell-alignment.md`).
