
#import "/template.typ": *
#import "/macros.typ": *

== Machine-checked results <sec:security-theorems>

The properties of the preceding section are not only stated over the execution
model; they are proved. This section renders the machine-checked results: each
statement below is the actual Agda type of a theorem in the module
`SecurityProofs`, shown here and in @agda-appendix, while the proof terms — and
the supporting lemmas and corollaries the prose cites by name — are typechecked
as part of this document's build but not rendered. All results are
inductions over the `Reachable` closure of the step relation `_⟶ˢ_`
(@sec:security); none of them is temporal (liveness remains out of scope).
The trust base is exactly the one enumerated in the Proofs preamble: the ledger
postulates (`applyTxs` and its laws), per-signature unforgeability plus the
aggregation scheme's decomposition (from which `ms-unforgeable` is derived),
the honest-behaviour premises of `signHonest`, and, where a result consumes an
on-chain validity bundle, the per-instance signature-trust hypotheses noted
with that result.


The workhorse is a single induction: every reachable system satisfies the
eight-field invariant `Inv` of @sec:security (@agda-appendix: `invariant`).
The safety content is derived from the structure of the steps rather than
assumed: `confApp` (L3) is discharged at `confirm` from `sigApp` (a certified
snapshot carries the honest confirmer's own signature, and honest signatures
are only on applicable snapshots), and `sigChain` records, for every honest
signature, an extending certified-or-genesis predecessor (from the
`signHonest` premises together with `confCert`), which is what the nesting
lemma below consumes. Corruption only ever shrinks the honest set, and `sigs`
only grows, so certification facts are carried forward across steps. All of
the L1/L2/L3 consequences used by the theorems of this section are projections
of this one invariant.







Agreement (L1) is the projection of the invariant's one-signature-per-number
field: two certified snapshots of the same number are equal, witnessed by any
honest party, who by `Certified` signed both (@agda-appendix: `agree`).










Certified snapshots nest by number (L2). The proof is an induction on the gap
between the two numbers: at gap zero the snapshots are equal by `agree`;
otherwise the higher snapshot has, by the invariant's `sigChain`, an extending
certified-or-genesis predecessor one number below it, so the induction
recurses to the predecessor and composes with the extension, the genesis case
being impossible for a certified (hence positive-numbered) snapshot
(@agda-appendix: `cert-nest`).



The @sec:security nesting obligation follows: two honest parties' confirmed snapshots
nest by number, since an honest party's confirmed snapshot is the genesis
(whose transaction list is empty, hence trivially contained) or is certified,
in which case `cert-nest` applies (@agda-appendix: `confirmed-nest`).



=== Consistency

#theorem(name: "Consistency")[
  In any reachable system, the confirmed transaction sets of two honest
  parties nest (one contains the other) and each is applicable to $Uinit$, so
  their union never fails to apply; the union form is machine-checked as a
  single transaction set containing both that is applicable to $Uinit$
  (`consistency-union`), making the paper's
  $Uinit compose (Tbar_i union Tbar_j) != bot$ literal. The property moreover
  extends to once-honest-then-corrupt parties: _every_ party's confirmed
  snapshot is genesis-or-certified unconditionally (`confCert-all`), because
  `confirm` demands a verifying aggregate multisignature regardless of the
  confirmer's honesty, so _any_ two parties' confirmed sets, including one a
  party confirmed before (or even adopted after) being corrupted, nest and are
  applicable, given at least one honest witness (`consistency-uncorrupted`)
  (@agda-appendix).
] <thm:consistency>



The union form: there is a single transaction set `T` containing both honest
confirmed sets (their union, i.e. the inclusion-larger of the two, since they
nest) that is applicable to $Uinit$.



The @sec:security random variables $Tbar_i$ scope a party's confirmed set to a party
_while uncorrupted_, and an on-chain close could be built against the
confirmed snapshot of a party corrupted after confirming. The extension rests
on the fact that certification is unconditional: the only step that changes a
party's confirmed snapshot is `confirm`, which requires a verifying aggregate
multisignature (hence, by unforgeability, a certificate) whatever the
confirmer's honesty flag. This is in fact stronger than the literal @sec:security
scoping, since it also covers any snapshot a corrupt party adopts after
corruption.








=== Soundness and completeness

#theorem(name: "Soundness")[
  In any reachable system with at least one honest party, if a snapshot's
  aggregate multisignature verifies, then applying its transactions to
  $Uinit$ succeeds (the final UTxO set exists and is conflict-free) and those
  transactions have been seen by _every_ honest party
  ($tilde(T) subset.eq inter.big_(j in honest) That_j$) (@agda-appendix:
  `soundness`).
] <thm:soundness>

Both conjuncts are derived: `ms-unforgeable` makes the verified snapshot
certified, an honest signer signs only applicable snapshots
(`cert-applicable`, giving conflict-freedom), and only transactions it has
seen (the `sigSeen` component of the invariant, giving the intersection-seen
subset).



#theorem(name: "Completeness")[
  Every honest party's confirmed transactions are contained in the finalized
  snapshot (the snapshot whose aggregate multisignature verifies), whenever
  that party's confirmed number is at most the finalized snapshot's number
  (@agda-appendix: `completeness`).
] <thm:completeness>

The finalized snapshot is certified (`ms-unforgeable`); the party's confirmed
snapshot is genesis (trivially contained) or certified (`confCert-of`), and
two certified snapshots nest by number (`cert-nest`), using the party itself
as the honest witness. The `confirmedNo ≤ number` premise is the "latest
multi-signed snapshot wins the close/contest game" fact of @sec:close-tx/@sec:contest-tx: the real
close/contest process always settles on the latest multi-signed snapshot,
whereas the model's `finalize` admits _any_ certified snapshot, so the fact
enters as a per-party premise rather than being derived.



=== The on-chain reflection bridge

#theorem(name: "On-chain settlement reflects the off-chain state")[
  A finalization against a snapshot whose aggregate multisignature verifies
  yields `Reflects`: the off-chain final UTxO exists (from
  @thm:soundness), the on-chain snapshot number matches (the `finalize`
  witness), and the stored accumulator commits to that UTxO. Conversely, when
  the datum reflects a finalized snapshot, the outputs a valid on-chain
  fan-out actually distributes are a subset of that off-chain final UTxO's
  outputs (`reflect-sound`, `reflect-fanout-⊆`) (@agda-appendix).
] <thm:reflection>

Two of the three `Reflects` conjuncts are derived; the accumulator commitment
is supplied as the explicit per-finalization hypothesis `ηEq`, the irreducible
signature-trust assumption: $nuHead$ authenticates $eta$ via the
multisignature over $cid || v || s || eta^(\#) || delta^(\#) || kappa^(\#)$, not by recomputing
$accUTxO(U)$. It is a hypothesis rather than a global postulate on purpose,
since `finalize` admits any datum with a matching snapshot number, a global
axiom would have no model, and the finalizer discharges `ηEq` from the $eta$
it actually committed.





The fan-out half consumes the bundle's membership conjunct through the
accumulator soundness law: the outputs the transaction pays (the anchored
`distributedOuts`) are a subset of the outputs of the off-chain final UTxO,
tying the transaction's own outputs, not a free set parameter, to the
Soundness UTxO.



=== No settlement without unanimity

#theorem(name: "No settlement without unanimity")[
  Every @sec:increment-tx–@sec:contest-tx validity bundle carries a `sigOK` conjunct bottoming out in
  `snapshotSigOK` ($msVfy$ over
  $cid || v || s || eta^(\#) || delta^(\#) || kappa^(\#)$), and the model's
  signing message `snapMsg` is _defined_ as that same concatenation. Once the
  snapshot's identifying fields match the signed ones and the redeemer's
  aggregate signature is the system-recorded one (the per-instance
  signature-trust hypothesis `ξEq`, the same pattern as the reflection
  bridge's `ηEq`), the on-chain conjunct is exactly `AggVerified`, and
  unforgeability certifies: every party signed. Hence no increment, decrement,
  close or contest settles without a unanimous certificate (`sig-certifies`
  and the per-transaction `*-certified` corollaries); the certified snapshot's
  accumulator hash is the hash of the accumulator actually stored in the
  produced datum (`close-η-reflected`, `contest-η-reflected`); and a valid
  deposit-claim transaction is both unanimously certified and posted before
  the deposit's recover deadline (`claimTx-certified`) (@agda-appendix).
] <thm:unanimity>

These corollaries consume the `sigOK` fields of the on-chain bundles, so the
off-chain certificate layer and the on-chain validity bundles meet at the
signature conjuncts, not only at datum accessors. The field-matching
equalities are dischargeable by `refl` for the snapshot actually signed;
`ξEq` is per-instance for the same reason `ηEq` is.



The per-transaction corollaries (`increment-certified`, `decrement-certified`,
`close-certified`, `contest-certified`) each consume their bundle's `sigOK`
field through a one-line application of `sig-certifies` (typechecked, not
rendered); the `closeUsed`/`contestUsed`/`closeAny` redeemer variants follow
identically (their `sigOK` reduces to `snapshotSigOK` at version $v - 1$ or
$v$).









On top of the certificate, close and contest cannot verify a signature over
one accumulator while storing another: the `etaOK` conjunct binds the
redeemer's $eta^(\#)$ to the hash of the accumulator stored in the produced
datum (the two-line corollaries `close-η-reflected` / `contest-η-reflected`,
typechecked but not rendered).





A valid deposit claim (the joint `ClaimTxValid` encoding, $nuHead$ increment
and $nuDeposit$ claim both passing) is unanimously certified _and_ posted
before the deposit's recover deadline: a deposit can be absorbed into the head
neither without every party's signature nor after it has become recoverable.



=== Handler invariants across adversarial executions

#invariant(name: "No commit/decommit overlap, version discipline (system level)")[
  Throughout any reachable adversarial execution, for every party: a pending
  commit ($tx_alpha$) and a pending decommit ($tx_omega$) are never both in
  flight (`noBothInFlightˢ`), and the party's seen open-state version is its
  confirmed snapshot's version or exactly one above (`versionDisciplineˢ`).
  Both are seeded by the `Initial` predicate and preserved by every `_⟶ˢ_`
  step, reusing the local handler-model preservation lemmas of
  @sec:offchain-theorems on the `offChain` step's embedded handler witness
  (@agda-appendix).
] <inv:system-handler>

This lifts the `require` disciplines of @fig:off-chain-prot from the single-party handler
model to the multi-party adversarial system: the deposit/decommit exclusivity
and the version discipline are properties of every reachable execution, not
runtime assertions an honest node merely hopes to maintain.




