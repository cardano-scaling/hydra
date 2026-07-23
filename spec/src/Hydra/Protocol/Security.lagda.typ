
#import "/template.typ": *
#import "/macros.typ": *

= Security <sec:security>

#todo[Add security experiment]
Adversaries:

/ Active Adversary.: An _active adversary_ $adv$ has full control
  over the protocol, i.e., he is fully unrestricted in the above#todo[above this section there is no security game] security game.

/ Network Adversary.: A _network adversary_ $adv_emptyset$ does not corrupt
  any head parties, eventually delivers all sent network messages
  (i.e., does not drop any messages), and does not cause the $hpClose$ event.
  Apart from this restriction, the adversary can act arbitrarily in the above experiment.

Random variables:

- $That_i$: the set of transactions $tx$ for which party $party_i$,
  _while uncorrupted_, output $(hpSeen, tx)$;

- $Tbar_i$: the set of transactions $tx$ for which party $party_i$,
  _while uncorrupted_, output $(hpConf, tx)$;

- $Snapbar_i$: latest snapshot $(s, U)$ that party
  $party_i$ performed _while uncorrupted_: output $(hpSnap, (s, U))$;

- $Hcont$: the set of (at the time) uncorrupted parties who produced
  $xi$ upon close/contest request and $xi$ was applied to
  correct~$eta$; and

- $honest$: the set of parties that remain uncorrupted.


Security conditions / events:

- #propName[Consistency (Head)]: In presence of an active adversary, the
  following condition holds at any point in time:
  For all $i, j$,
  $Uinit compose (Tbar_i union Tbar_j) != bot$, i.e., no two
  uncorrupted parties see conflicting transactions confirmed.

- #propName[Oblivious Liveness (Head)]:
  Consider any protocol execution in presence of a network adversary wherein
  the head does not get closed for a sufficiently long period of time, and consider
  an honest party $p_i$ who enters transaction $tx$ by executing $(hpNew, tx)$ _each time after having finished a snapshot_.

  Then the following eventually holds:
  $tx in inter.big_(i in [n]) Tbar_i or
  forall i: Uinit compose (Tbar_i union {tx}) = bot$,
  i.e., every party will observe the transaction confirmed or every party
  will observe the transaction in conflict with their confirmed transactions.#footnote[
    In particular, _liveness_ expresses that the protocol makes progress
    under reasonable network conditions if no head parties get corrupted.
  ]

- #propName[Soundness (Chain)]: In presence of an active adversary,
  the following condition is satisfied:
  $exists Ttilde subset.eq inter.big_(i in honest) That_i : Ufinal
  = Uinit compose Ttilde != bot$, i.e., the final UTxO set results
  from applying a set of transactions to $U_0$ that have been seen by
  all honest parties (whereas each such transaction applies conforming to the ledger rules).

- #propName[Completeness (Chain)]: In presence of an active adversary,
  the following condition holds: For $Ttilde$ as above,
  $union.big_(p_i in Hcont) Tbar_i subset.eq Ttilde$, i.e., all
  transactions seen as confirmed by an honest party at the end of the
  protocol are considered.

Note that the original version of the coordinated head satisfies a stronger version of liveness which is important for the 'user experience' in the protocol:

- #propName[Liveness (Head)]:
  Consider any protocol execution in presence of a network adversary wherein
  the head does not get closed for a sufficiently long period of time, and consider
  an honest party $p_i$ who enters transaction $tx$ by executing $(hpNew, tx)$.

  Then the following eventually holds:
  $tx in inter.big_(i in [n]) Tbar_i or
  forall i: Uinit compose (Tbar_i union {tx}) = bot$,
  i.e., every party will observe the transaction confirmed or every party
  will observe the transaction in conflict with their confirmed transactions.#footnote[
    In particular, _liveness_ expresses that the protocol makes progress
    under reasonable network conditions if no head parties get corrupted.
  ]


== Proofs

The security properties are stated over the protocol model below. Three of the four are
_machine-checked_ in Agda - #propName[Consistency] (`consistency`), #propName[Soundness]
(`soundness`) and #propName[Completeness] (`completeness`) - with the safety content _derived_
from a signature model (below):

- individual party signatures,
- a snapshot _confirmable_ only once _every_ party signed it (the coordinated
  head's full multisignature), and
- honest parties signing only _applicable_ snapshots, at most one per number,
  each extending the signer's own confirmed snapshot.

From these the Agda machine-checks that every honest party's confirmed snapshot is
applicable to $Uinit$ (so confirmed sets never conflict), that two confirmations of the same snapshot
number coincide, and that confirmed snapshots nest by number (`confirmed-nest`).

`confirm` checks the @sec:multisig aggregate multisignature (`msVfy`); `msgOf` is the snapshot's
own serialised content (`snapMsg` of its cid, version, number and η-hash, the @sec:offchain message
cid‖v‖s‖η\#), so the verified message depends
only on the snapshot's identifying fields rather than being a free token. The binding of a verifying
signature to a snapshot is formally carried by `ms-unforgeable`. These are theorems about every
#emph[currently]-honest party's confirmed snapshot: the random variables $That_i$/$Tbar_i$ are scoped
to a party _while uncorrupted_; corruption only shrinks the honest set, and the theorems do not
constrain a once-honest-now-corrupt party's confirmed set.

The safety perimeter — the assumptions the proofs rest on — is:

+ the ledger semantics (`applyTxs`);
+ per-signature _unforgeability_ (`sigUnforge`, EUF-CMA) plus the aggregation
  scheme's n-of-n decomposition (`aggSound`), from which the aggregate-level
  `ms-unforgeable` is _derived_;
+ the honest-signing discipline of `signHonest`, part derived and part assumed:
  the numbering guard (sign exactly the number one above the signer's own
  confirmed snapshot, hence at most once per number) is _derived_ from the
  fired `reqSn-sign` handler premise together with the no-in-flight
  precondition and the invariant's `signNumBound`, while chain-extension,
  applicability-of-the-delta and only-seen enter as _premises_ of the
  `signHonest` constructor - explicit honest-behaviour assumptions from the
  protocol flow (@fig:off-chain-prot); and
+ for the on-chain bridge only, that the finalized datum's stored accumulator
  commits to the off-chain final UTxO (the `ηEq` hypothesis of `reflects`,
  supplied per finalization), irreducible because νHead authenticates η via
  the multisignature, not by recomputing it.

The verified aggregate is _system-relative_: `AggVerified sys snap` checks the
aggregate `aggSigOf sys snap` built from the signatures the system recorded (`sigs sys`).
This keeps the confirmation layer non-vacuous - `AggVerified sys snap` is false where
the signatures are absent, yet satisfiable where every party signed, so a model with
genuine confirmations exists. #propName[Liveness] is not yet _stated_: its type is left
abstract pending a deferred temporal/fairness layer, so nothing about it is assumed to hold. The
prose lemmas further below give the informal arguments these proofs mirror.

#dparagraph[Scope (what these proofs do and do not cover).] To avoid over-reading the word "unified":
these proofs and the on-chain validity bundles of @sec:on-chain (`closeValid`, `incrementValid`, …)
are two formalizations with _three deliberate meeting points_:

+ the datum-field accessors (the security model reads the on-chain datum
  through `OC.snapNum`/`OC.ηOf`/`OC.accUTxO`);
+ the signing message (`snapMsg` is _defined_ as the same
  `cid ‖ v ‖ s ‖ η# ‖ δ# ‖ κ#` concatenation the bundles' `snapshotSigOK`
  verifies, so the two formalizations meet definitionally at the message); and
+ the certificate corollaries of @sec:security-theorems (`sig-certifies` and
  the `*-certified` family consume the bundles' `sigOK` conjuncts, together
  with the close/contest `etaOK` binding and the deposit side's
  before-deadline check).

The `finalize` step still admits _any_ datum with a
matching snapshot number, so no reachability theorem consumes a bundle's value-conservation,
deadline or contester checks; those are instead cross-checked against the real Plutus
validator by the extracted differential oracle (the `Reference`/`ReferenceBridge` modules),
not by these theorems. Two further honesty notes:

- _non-vacuity_ (that some confirmation is reachable) is a meta-level
  model-existence argument, not machine-checked, because `msVfy` is an
  abstract postulate so no closed term proves `AggVerified`;
- the `ηEq` accumulator-commitment is supplied by the finalizer, not enforced
  by the model, so `Reflects` is conditional on the finalizer having posted
  the η it signed.

The
νDeposit validator (`deposit.ak`) and the off-chain handlers are likewise not part of any machine-checked
theorem here; their decidable conjuncts are covered by the extracted differential layer instead (the
Claim arm fully - `claimTxValid→ref` and the claim agreement - the Recover arm modulo its
serialisation-hash mock, and the handler guards by the `hydra-node` agreement tests).

The confirmed-snapshot ordering that the safety argument relies on is machine-checked, not a
free-standing predicate: `agree` (L1: two honest-certified snapshots of the same number coincide) and
`cert-nest` (L2: honest-certified snapshots nest by number), both proved over `Reachable` in
@agda-appendix and consumed by the theorems below.

The properties above quantify over whole multi-party executions in the presence of an
adversary, so they are stated over an explicit execution model:

- a ledger-application operation `applyTxs`;
- a global $sans("System")$ state recording each party's signatures;
- a single-step relation $sans("_⟶ˢ_")$ - an honest party signs an
  _applicable_ snapshot, a corrupt party signs arbitrarily, a party confirms
  a snapshot whose aggregate multisignature verifies, the adversary corrupts
  a party; and
- the $sans("Reachable")$ closure from an initial system.

A snapshot is $sans("Certified")$ once every party signed it, so
unforgeability is immediate: a certified snapshot carries the confirmer's own
honest signature. The machine-checked invariant then derives:

+ every honest party's confirmed snapshot is applicable to $Uinit$, from the
  honest "sign only applicable" guard;
+ two certified snapshots of the same number are equal, from the honest "one
  signature per number" guard; and
+ confirmed snapshots nest by number (`confirmed-nest`), from the honest
  "extend my own confirmed snapshot" guard plus a gap induction using the
  previous item.

`confirm` checks the @sec:multisig aggregate multisignature
(`AggVerified`/`msVfy`). Beyond the ledger `applyTxs` and the scheme's
unforgeability (per-signature `sigUnforge` + the `aggSound` decomposition,
from which `ms-unforgeable` is derived), the safety argument relies on the
honest-behaviour _premises_ of `signHonest` - chain-extension, applicability
of the delta, only-seen (see the modelling note below; the numbering guard,
by contrast, is derived from the fired handler) - which make the confirmed
chain linear and monotone, and, for the on-chain side, on the finalization
bridge's accumulator-commitment hypothesis.

The off-chain⇒on-chain link is constructed by `reflects`, from a `finalize`
step: the conflict-freedom and snapshot-number conjuncts of `Reflects` are
derived, leaving the stored accumulator's commitment to the off-chain UTxO as
the single assumed conjunct, supplied per finalization as the explicit
hypothesis `ηEq` (a hypothesis rather than a global axiom, since `finalize`
admits any matching-number datum). #propName[Liveness] additionally needs a
temporal/fairness layer (deferred).

This section states the model and the property statements; the machine-checked results are
rendered in @sec:security-theorems, and their _proof terms_ (the `invariant` induction and its
L1/L2/L3 corollaries, the `consistency`/`soundness`/`completeness` derivations, the
once-honest-then-corrupt extension and the `reflects` bridge) live in the companion literate
module #raw("Hydra.Protocol.SecurityProofs"), typechecked by the build (imported by `Main`)
with the proof bodies not rendered, so the properties remain machine-verified.

#dparagraph[Modelling note (honest signing discipline: derived vs. assumed).]
The `signHonest` move follows the off-chain handler model: an honest party signs by firing the
`reqSn-sign` handler (OffChain `_handles_↝_`) with no snapshot in flight ($hats = bars$). The four
honest-signing guards divide as follows.

The numbering guard is _derived_: the fired handler's premise $s = bars + 1$
(with $hats = bars$) makes the signed snapshot exactly one above the signer's
_own_ confirmed snapshot, and since signing advances $hats$, the invariant
`signNumBound` bounds every prior signature strictly below the new number, so
at-most-one-signature-per-number (`sigDedup`) is proved rather than assumed.

The other three guards are _premises_ of the `signHonest` constructor -
assumptions about honest behaviour, not derived from the off-chain handlers:

- the snapshot's transactions extend the signer's own confirmed snapshot by a
  delta (chain-extension);
- the delta applies on top of that confirmed snapshot (applicability); and
- the delta has been observed (only-seen).

From these premises the invariant derives the whole-snapshot facts:
applicability to $Uinit$ by ledger compositionality (`applyTxs-compose`) from
the party's confirmed-applicability invariant, and only-seen for the whole
snapshot from the `sigSeen` invariant. The @sec:offchain prose specifies this
regime operationally (round-robin snapshot leader, $s = hats + 1$, the $hpRS$
'wait' guards); the derived numbering guard and the honest-behaviour premises
together make the confirmed chain linear (`agree`) and monotone
(`confirmed-nest`).

=== The system model

A party's confirmed transactions and number are read off its local state; the
global `System` records each party's local state, honesty flag, recorded
signatures and seen sets; and `Certified` is the n-of-n signing predicate the
proofs reason with.




=== The signing message and unforgeability

The signing message is the @sec:offchain serialisation, defined as the same concatenation
the on-chain signature conjuncts verify; aggregate unforgeability is derived
from per-signature EUF-CMA plus the aggregation scheme's decomposition
(@agda-appendix).





=== The step relation

The single-step relation captures honest signing (firing the `reqSn-sign`
handler), corrupt signing, confirmation against a verifying aggregate,
corruption, finalization, observation, and lifted local off-chain steps.


=== Initial systems, reachability and the invariant

An initial system has no signatures and genesis confirmed snapshots; `Reachable`
closes the step relation from an initial system; and `Inv` is the eight-field
invariant carried through every reachable system, proved by the `invariant`
induction of @sec:security-theorems (@agda-appendix).




=== The property statements

The properties above are stated as types; their proofs are the machine-checked
results of @sec:security-theorems (@agda-appendix).







#dparagraph[Consistency.]

#lemma(name: [Consistency])[
  The coordinated head protocol satisfies the #propName[Consistency] property.
] <lem:consistency>
#proof[
  Observe that $Tbar_i union Tbar_j subset.eq That_i$ since no
  transaction can be confirmed without every honest party signing off
  on it. Since parties do not sign conflicting transactions
  (see $hpRS$, 'wait'), we have
  $Uinit applytx Tbar_i != bot$,
  $Uinit applytx Tbar_j != bot$, and
  $Uinit applytx That_i != bot$. Thus, since $Tbar_i union Tbar_j subset.eq That_i$
  it follows that
  $Uinit applytx (Tbar_i union Tbar_j) != bot$

  _Machine-checked as `consistency` (@sec:security-theorems), *derived* from the signature model: each honest
  party's confirmed set is applicable (`conf-applicable`: a confirmed snapshot is certified, so it
  carries that party's own signature, and honest parties sign only applicable snapshots), and the two
  confirmed sets nest (`confirmed-nest`, derived via `cert-nest` from the honest extend-your-own-confirmed
  guard + agreement). The only safety assumptions are the ledger and the @sec:multisig multisignature's
  unforgeability (`ms-unforgeable`). The statement also covers parties corrupted after
  confirming (`consistency-uncorrupted`): since `confirm` requires a multisignature regardless of the
  confirmer's honesty, every confirmed snapshot is certified (`confCert-all`), so any party's confirmed
  set — including a once-honest party's, the one an on-chain close could be built against — stays
  consistent with every other's, given at least one honest party._
]

#dparagraph[Oblivious Liveness.]
For all lemmas towards oblivious liveness, we assume the presence of a network adversary, and that the head does not get closed for a sufficiently
long period of time.
We call this the _liveness condition_.

#lemma[
  Under the liveness condition, any snapshot issued as $(hpRS, s, T)$ will eventually be confirmed
  in the sense that every party holds a valid mulisignature on it.
] <lem:reqconf>
#proof[
  Consider a party $p_i$ receiving message $(hpRS, s, T)$. We demonstrate that $p_i$ executes
  the code past the 'wait' instruction of the $hpRS$ routine.

  - Passing the 'require' guard:
  Note that the snapshot leader sends the request only if $hats = bars$, and for $s = hats + 1$.
  Thus, $hats_i = hats$ since $p_i$ has already signed the snapshot for $hats$. The 'require'
  guard is thus satisfied for $p_i$.

  - Passing the 'wait' guard:
  Since the snapshot leader sees $hats = bars$, also $p_i$ will eventually see $hats_i = bars_i$. Furthermore, since all leaders are honest, it holds that $hatmU applytx mT_("res") != bot$ by construction.

  This implies that every party will eventually sign and acknowledge the newly created snapshot.
  Finally, the 'require' and 'wait' guards of the $hpAS$ code will be passed by every party
  since an $hpAS$ for snapshot number $s$ can only be received for $s in {hats, hats + 1}$
  as an acknowledgement can only be received for the current snapshot being worked on by $p_i$
  or a snapshot that is one step ahead — implying that everybody will hold a valid multisignature
  on the snapshot in consideration.
]

#lemma(name: [Eternal snapshot confirmation])[
  Under the liveness condition, as long as new transactions are issued, for any $k > 0$, every party eventually confirms
  a snapshot with sequence number $s = k$.
] <lem:eternal>
#proof[
  By @lem:reqconf, any requested snapshot eventually gets confirmed, implying
  that the next leader observes $hats = bars$ and thus, in turn, issues a new snapshot.
  Thus, for any $k$, a snapshot is eventually confirmed.
]

#lemma(name: [Oblivious Liveness])[
  The coordinated head protocol satisfies the #propName[Oblivious Liveness] property.
] <lem:liveness>
#proof[
  Consider the first point in time where a transaction $tx$ enters the system by some party $p_i$
  issuing $(hpNew, tx)$, and consider the next point in time
  $t$ when $p_i$ issues a snapshot.

  By @lem:eternal, this snapshot will eventually be issued and confirmed by all parties.

  #v(0.5em)

  Let $hatmT$ be the transactions to be considered by $p_i$'s snapshot: $hatmL = barmU applytx hatmT$
  where $barmU$ is the snapshot prior to $p_i$'s. Since $p_i$ issues
  $(hpRT, tx)$ after each snapshot, we have that, either,
  - $tx in hatmT$, in which case $tx in inter.big_(i in [n]) Tbar_i$ after everybody has completed this snapshot, or,
  - $tx in.not hatmT$, in which case $hatmL applytx tx = bot$ ($tx$ is still in the wait queue of $(hpRT, tx)$. After everybody has completed this snapshot, it thus holds that $forall i: Uinit applytx Tbar_i = hatmL$, and thus, that
    $forall i: Uinit applytx (Tbar_i union {tx}) = bot$.
  In both cases, the lemma follows.
]

#dparagraph[Soundness and completeness.]

#lemma(name: [Soundness])[
  The basic head protocol satisfies the #propName[Soundness] property.
] <lem:soundness>

#proof[
  Let $T$ be the set of transactions such that $Ufinal = U_0 applytx T$.
  Since $Ufinal$ is multi-signed, it holds that $T subset.eq That_i$
  ($T$ is _seen_) by every honest party in the head.
  Furthermore, since honest signatures are only issued for valid transaction,
  $Ufinal != bot$ (i.e., $Ufinal$ is a valid state), and soundness
  follows.

  _Machine-checked as #raw("soundness") in @sec:security-theorems ($Ufinal = U_0 applytx tilde(T) != bot$ with
  $tilde(T)$ the certified finalized snapshot). The $!= bot$ is *derived*: a
  certified snapshot carries an honest party's signature, and honest parties sign only applicable
  snapshots (`cert-applicable`). The $tilde(T) subset.eq inter.big_(i in honest) That_i$ conjunct
  is machine-checked too: an honest party signs only transactions it has observed (the `signHonest`
  seen guard), so a certified snapshot's transactions lie in every honest party's seen set
  (`sigSeen-inv`), proved as the second conjunct of #raw("soundness")._
]


#lemma(name: [Completeness])[
  The basic head protocol satisfies the #propName[Completeness]
  property.
] <lem:completeness>
#proof[
  Consider all parties $p_i in Hcont$. Since the close/contest process
  finally accepts the latest multi-signed snapshot, it holds that
  $Ufinal . s >= max_(p_i in Hcont) (bars_i)$, and thus that
  $union.big_(p_i in Hcont) Tbar_i subset.eq inter.big_(p_i in honest) That_i$,
  and completeness follows.

  _Machine-checked as #raw("completeness") in @sec:security-theorems: each honest party's $Tbar_i subset.eq tilde(T)$
  (the finalized snapshot itself) whenever $bars_i <= s_f$, *derived* from `cert-nest` (L2),
  `confCert-of` (the party's confirmed snapshot is genesis-or-certified) and `ms-unforgeable` (the
  finalized snapshot is certified). The $bars_i <= s_f$ premise is the "latest multi-signed snapshot
  wins" fact of the close/contest process; our `finalize` admits any certified snapshot, so it is a
  per-party premise rather than derived. The sibling honest-to-honest nesting is `confirmed-nest`._
]
