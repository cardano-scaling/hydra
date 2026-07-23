
#import "/template.typ": *
#import "/macros.typ": *

== Machine-checked on-chain properties <sec:onchain-theorems>

The per-transaction conditions of the preceding sections are _soundness_
statements: they are only ever consumed in the direction "accepted
$arrow.r.double$ safe". None of them says that a validity bundle is
_inhabited_ for the states that reach it. That direction matters: an
over-strict condition (one stronger than the validator needs) only shrinks
the accept set, so it can never falsify a soundness theorem, yet it can
strand a reachable head forever (an empty head that cannot be finalised
leaves its $n+1$ state tokens unburnable). This section states the
machine-checked properties that close that gap, together with the safety
invariants and value-conservation theorems of the on-chain state machine.
All of them are proved in the Agda module `OnChainCoverage`
(@agda-appendix shows the statements; the proof terms are typechecked as
part of this document's build but not rendered). None of this is temporal
liveness (fairness, message delivery, eventual confirmation are out of
scope): every statement is an atemporal existence or invariance statement
over the transition relation of @sec:on-chain.

Two reachability notions ground the statements: `Reachable` is datum-shape
reachability over the bare step relation, and `Reachableᵛ` closes under
steps that additionally carry their validity bundles, so it is exactly the
set of states a validating chain can occupy. The contest step of
`Reachableᵛ` is stated on concrete datums so the bundle's contester is tied
to the datum's newly-appended one.





=== Non-stuckness (coverage)

#theorem(name: "Empty head is finalisable")[
  The machine reaches an empty `Closed` head (init opens empty, close-initial
  keeps it empty), and that reachable state admits a valid $m = 0$ full
  fan-out: given the context facts (past the deadline, the $n+1$ tokens
  burned, value conserved, the canonical CRS reference resolved), the
  terminal `Fanout` bundle is inhabited, with the $0$-output membership
  witness derived from the accumulator laws
  (@agda-appendix: `fanout-empty-inhabited`, `finalize-reachable-empty`).
] <thm:empty-finalisable>

An `outputsPositive : 0 < m` conjunct on `FanoutValid` would make these
lemmas fail to compile (the empty head forces $m = 0$), so the build itself
rejects introducing that over-strict guard, which is exactly the
defect class this direction exists to catch.





#theorem(name: "Reachable heads can finalise")[
  Any reachable `Closed` head committing to a known UTxO set $V$
  ($eta = accUTxO(V)$) admits a valid full fan-out of $V$, provided the
  transaction actually distributes $V$; the membership witness is derived
  from the accumulator laws, so only the genuinely contextual antecedents
  (deadline, burn, value conservation, pays-out-$V$, the canonical CRS
  reference resolved) remain. Likewise, a reachable `FanoutProgress` is
  never stuck: its remaining accumulator is provably non-empty
  (`progress-nonEmpty`), which _derives_ the final batch's $0 < m$
  requirement rather than assuming it
  (@agda-appendix: `fanout-coverage`, `progress-finalizable`).
] <thm:coverage>







=== Safety invariants of reachable states

The coverage statements above feed the bundle conditions to a constructor,
so they are agnostic to the conditions' definitions. The invariants below
instead _consume_ the structural premises of the transition relation across
a reachable run: corrupting a premise breaks the corresponding proof at
compile time.

#invariant(name: "No double contest, no resurrection")[
  The contester list of any reachable head is duplicate-free (each contest
  step's freshness premise $keyHash in.not "contesters"$ is exactly what
  discharges the induction), and a head that has fanned out is terminal: no
  transition leaves `Final` (@agda-appendix:
  `reachable-contesters-distinct`, `final-is-terminal`).
] <inv:contest-final>





=== Value conservation (no theft)

#theorem(name: "Every transaction accounts for the head value")[
  A fan-out (full or final-partial) fully accounts for the head's input
  value on the ada axis: input ada equals distributed ada plus burned ada
  plus the carried overhead. Close and contest preserve the head value
  exactly on both the ada and non-ada axes, and an intermediate partial
  fan-out splits it exactly between the continuing head output and the
  distributed batch. Each statement consumes its bundle's value conjunct
  through the additivity of the value projections
  (@agda-appendix: `fanout-conserves-ada`,
  `finalPartialFanout-conserves-ada`, `close-preserves-value`,
  `contest-preserves-value`, `partialFanout-conserves-ada`).
] <thm:value-conservation>











#theorem(name: "No output fabrication at fan-out")[
  The outputs a fan-out transaction actually pays (the anchored
  `distributedOuts ctx m`, the first $m$ transaction outputs) are a subset
  of the accumulator-committed set $V$: consuming the membership conjunct
  together with the accumulator soundness law, a fan-out cannot distribute
  an output the head did not commit to
  (@agda-appendix: `fanout-distributes-committed`,
  `finalPartialFanout-distributes-committed`).
] <thm:no-fabrication>





A validly-initialised head is moreover a reachable state, tying the
$muHead$ init conditions (`versionZero`, `etaEmpty`) to the reachability
all of the above is stated over (@agda-appendix: `init-reachable`).



=== The contest game is bounded

The security section's completeness theorem (@sec:security) assumes "the
latest multi-signed snapshot wins the close/contest game"; the on-chain
half of that game is bounded by two machine-checked facts. First, the
contestation deadline of any validly-reachable `Closed` head is at most the
close-time deadline plus one contestation period per recorded contester:
each contest extends the deadline by at most $T$, consuming the close and
contest deadline equations across the whole run.



Second, the contesters are drawn from the head's $n$ participation-token
names, so there are at most $n$ of them. The participant-set facts are
taken as _module hypotheses_ rather than global postulates on purpose:
"a unit quantity of asset $(cid, keyHash)$ in a head-output value names one
of the head's init-minted participation tokens" is true on-chain by
once-only minting (the $muHead$ seed is spent, so policy-$cid$ tokens
beyond the $n+1$ minted at init can never exist), but it is not true of the
raw value model, where any map literal can carry any asset. Scoping the
fact as a hypothesis keeps the theory honest: the theorems hold in any
model or run where the values at the head are ledger-produced.

#theorem(name: "The contest window is bounded")[
  Under the once-only-mint hypotheses, every recorded contester of a
  validly-reachable `Closed` head is a participant, so (with
  no-double-contest) at most $n$ contests can ever be recorded, and the
  contestation deadline is at most $(t_"hi" + T) + n dot T$ for the close
  transaction's upper validity bound $t_"hi"$: the deadline moves at most
  $n$ times, and fan-out is enabled by close plus $n+1$ contestation
  periods (@agda-appendix: `contesters-are-participants`,
  `contesters-bounded`, `contest-window-bounded`). This is the atemporal
  safety half of the contest game; no fairness or liveness is assumed.
] <thm:contest-window>







=== The bridge to the extracted reference checker <sec:bridge-flagship>

The `spec ⇒ extracted-reference` half of the differential chain (the Scope note
of @sec:security) is proved per conjunct in the typecheck-only `ReferenceBridge`
module. Its flagship composition lemma is re-stated here so the rendered
document carries one representative: a single `closeValid` bundle (shown for the
`closeInitial` case) discharges the extracted close checker, the value
preservation checker, the no-mint checker and the shared participant-signature
checker at once, on the same inputs the `hydra-tx` `HeadValidatorAgreement`
suite feeds the real validator. The other transaction families compose
identically; their statements live in `ReferenceBridge`, and the injected mocks
and encoding postulates the bridge rests on are the drift-checked ledger of
@sec:assumption-inventory.


