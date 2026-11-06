# Crem/Program review

The two concrete findings from the review are repaired. The follow-up also changes
the decommit owner to represent local intent and settlement authority separately.
The [state-transition analysis](state-transitions.md) explains the design and its
remaining limits; [validation](validation.md) records the checks.

## Findings and disposition

| Finding | Disposition |
| --- | --- |
| 1. A confirmed conflicting transaction leaves an orphaned decommit reservation. | Fixed at confirmation/adoption. Reconstruct the unreserved ledger, retain valid dependencies, and release a superseded reservation with `DecommitInvalid`. |
| 2. Confirmation evidence is not bound to the Head's committee. | Fixed. Evidence retains the ordered verification keys; the owner checks Head, committee and candidate bytes. Adoption checks Head and committee. |
| 3. Owner phases describe storage shape and exclude too few protocol mistakes. | Improved for decommits: `NoDecommit`, `DecommitPending`, `AwaitingSettlement`. Direct replacement of an authorised decrement by a local reservation is absent from the topology and fails to typecheck. The snapshot storage graph remains broad; its limits are explicit. |
| Trusted replay can manufacture unverified commands for live staging. | The event adapter is private. Public `replayEvent` imports retained facts without returning a command to a live Program. Recovery remains trusted. |
| Confirmation refusal is an empty event list that callers can overlook. | Refused confirmation, adoption and decommit reservation reject staging. An earlier staged signature is discarded. Request selection still deliberately uses event presence through `SendSnapshot`. |
| A node with a decommit certificate but no local `ReqDec` admits another decommit. | Found while refining the phases. Client and network admission now check the certificate; the owner also refuses replacement. |

The [missing carried transaction](state-transitions.md#unresolved-a-certificate-without-its-carried-transaction)
is an additional unresolved finding from the phase analysis. ReqSn can overtake
ReqDec; the next leader then emits a request omitting the authorised decommit,
which its own validator rejects. The example uses the normal dispatcher and real
signatures. It needs a separate repair to candidate/request retention and proposal
admission; the two requested fixes do not resolve it.

The conflict is an inherited protocol defect, reproduced with real signatures.
The empty-committee case is an in-process API misuse: production AckSn already
supplied the correct keys. No network signature bypass was demonstrated.

The [historical conflict trace](../prior-spike/diagrams.md#historical-conflicting-decommit-reservation)
and [corrected trace](../head-logic-diagrams.md#conflicting-decommit-released-at-confirmation)
show the same schedule before and after the repair. The automated tests vary the
remaining initial deliveries in 24 ways, compare independently corrected replay,
round-trip serialized state, and admit a subsequent valid decommit. Separate
cases preserve dependencies and restore unspent inputs of multi-input reservations.

## Correctness limits

Compatibility with the frozen implementation caught migration mistakes; it did
not establish protocol correctness. The conflict passed the old implementation's
outcome and replay comparisons. The earlier invariant—reserved inputs are absent
from local UTxO—also held after a competing transaction consumed those inputs.
Disposition after confirmation needs a different invariant.

The fixes do not establish that all HeadLogic bugs are gone. Remaining cases are
listed in [recovery limits](checks.md#recovery-limits): retry expiry can strand
work, an invalid partial acknowledgement blocks replacement, and partial
acknowledgements still record a signature twice. The user chose to defer the two
recovery-policy changes. Checkpoint imports remain trusted and do not repair all
historically inconsistent state.

The merged [capacity fix](capacity.md#merged-fix-and-regression-tests) and its
upstream tests remain in this representation. It was reported upstream in
PR #2927, not discovered independently by Crem.

## Human maintenance

Read one rule (`ReqTx` or `AckSn`), then `Workflow`, `Coordinated`, and its two
owners. Rules retain their decisions, primitive queries and effects. Owners
retain state updates. Rendering stays in the development library. Names should
identify a protocol prerequisite or owned fact, not an incidental program counter.

There is still a cost in phase GADTs, singleton declarations, imports and
intermediate result phases. Crem's `ActionResult` chooses the destination outside
the effect, so effectful ledger results need a phase in which to branch. Do not
add stages merely to make graphs longer. The new decommit distinction earns its
place by changing admission and confirmation behavior and excluding a replacement
edge. The snapshot storage graph does not yet offer the same protection.

`stage` and recovery share transition definitions. That removes a source of
reducer drift, but several copies remain deliberately:

- Frozen handlers, selectors and reducers in the test library preserve migration
  behavior. They are large; keep a removal milestone after review and retain the
  compact counterexamples and independent properties.
- ReqSn checks received proposals independently of leader selection. Combining
  them must not let an untrusted leader bypass validation.
- Client admission, network admission, the ledger and on-chain validators have
  different trust boundaries. Their checks are not automatically redundant.
- Agda is intentionally independent. Its bounded projections must name their
  exclusions rather than imply a proof of the complete implementation.

## Type safety and security

The new owner boundary checks the Head and ordered committee that produced
verification evidence. Candidate bytes must match on confirmation. A refused
change rejects the Program, preserving the existing discard-on-rejection and
prefix-on-wait semantics. This strengthens future-caller protection; it does not
change the signature scheme.

The remaining assumptions matter:

- `Program` capability indices restrict which primitives are available. They do
  not prove that arbitrary before/after UTxOs came from ledger validation.
- Candidate signing bytes are cached in publicly constructible state. Decoding
  rebuilds them, but an in-process caller can still construct inconsistent state.
- Replay trusts retained events and checkpoints. `Program.change` can still emit
  raw events; the whole codebase is not forced through opaque owner commands.
- Crem permits self-transitions. An edge proof alone cannot prevent replacing an
  operation with another identity within the same phase.
- Traces omit signing keys, but can contain transaction/application data.

Authenticated membership, ReqSn ledger validation, aggregate verification and
capacity checks remain. Fault injection stays in the development library. The
investigation demonstrates coordination and API defects, not theft or an invalid
production confirmation.

## Crem changes worth discussing

The installed library is crem 0.1.1.0. Three improvements would help this design:

1. Let an effect return an existential destination together with its allowed-edge
   proof. This could remove mechanical result phases while retaining typed
   transitions. See [BaseMachine](https://github.com/marcosh/crem/blob/main/src/Crem/BaseMachine.hs).
2. Separate execution from the singleton/rendering dictionaries required by
   `StateMachine.Basic`, or supply a small declaration helper. See
   [StateMachine](https://github.com/marcosh/crem/blob/main/src/Crem/StateMachine.hs).
3. Provide an explicit state inspection hook. The current `NoThunks` instance
   accepts `Basic _` without examining the base machine; it cannot certify the
   contents of retained continuations. Hydra should still own its persistence
   format and protocol evidence.

An owner-issued candidate handle tied to a specific round is the next Hydra-side
refinement worth testing. It must share the existing rules and keep recovery
initialization distinct from live admission. It is not implemented here.

## Runtime cost

The recorded cluster runs before these repairs lose 3.54–7.82% TPS for small Heads
and allocate 12.25–14.58% more per confirmed transaction. Larger Heads show a smaller
relative cost; burst results are noisy. These numbers do not support a general
sub-5% claim and have not been remeasured for the new confirmation checks.
[Measurements](measurements.md) separates measured results from the additional
work introduced by the repairs. Profile allocations and runtime reconstruction
before accepting this cost in production.
