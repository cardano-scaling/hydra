# Protocol checks and recovery limits

Crem constrains phase changes and ownership. Program exposes the decisions and
primitive queries inside those phases. Payload invariants and protocol progress
need independent checks in addition to those types.

## Enforced requirements

| Requirement | Code and evidence |
| --- | --- |
| A locally applied transaction retains its validated registry payload. Duplicate receipt is a no-op before ledger application. | [ReqTx](../../hydra-node/src/Hydra/HeadLogic/Rules/ReqTx.hs); the witness-stripped duplicate regression in [HeadLogicSpec](../../hydra-node/test/Hydra/HeadLogicSpec.hs). |
| Snapshot rebase preserves pending decommit spending-input reservations. | [Transaction owner](../../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Transactions.hs), [CoordinatedAudit](../../hydra-node/testlib/Test/Hydra/HeadLogic/CoordinatedAudit.hs), and [CoordinatedSpec](../../hydra-node/test/Hydra/HeadLogic/CoordinatedSpec.hs). |
| A decommit request waits for its required inputs in the confirmed base. A newly ready decommit can start its own round. | [Shared selection](../../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotRequest.hs); SimpleTx and real Cardano checks in [IncrementalWorkflowSpec](../../hydra-node/test/Hydra/HeadLogic/IncrementalWorkflowSpec.hs). |
| A live snapshot request has checked capacity and matches the owner's head, version, number, and idle phase. | [Capacity contract](capacity.md) and [CapacitySpec](../../hydra-node/test/Hydra/HeadLogic/CapacitySpec.hs). |
| Live confirmation requires evidence bound to the Head, ordered committee and current candidate's signed bytes. Adoption checks the Head and committee too. Failed final verification discards the staged signature. | [SnapshotVerification](../../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotVerification.hs), [snapshot owner](../../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Snapshots.hs), and [CoordinatedSpec](../../hydra-node/test/Hydra/HeadLogic/CoordinatedSpec.hs). |
| Repeated settlement observations preserve a newer reservation; rollback preserves L2 state and controls reposting through retained observation slots. | [State owners](../head-logic-crem.md#settlement-and-rollback), [Settlement](../../hydra-node/testlib/Test/Hydra/HeadLogic/Settlement.hs), and owner/recovery tests. |

Cardano dependency availability includes spending, reference, and collateral
inputs. Reservations remove spending inputs. Neither arithmetic check replaces
ReqSn's validation of scripts, witnesses, or time.

The explorer independently checks confirmed signatures, agreement at equal
version/number, dependency order, registry payload equality, and local UTxO
reconstruction. Its capacity scenario also checks both fanout alternatives.
These checks use the real verifier and a SimpleTx reference calculation rather
than the production state reducer.

Confirmation also releases conflicting local decommits, restoring their remaining
inputs. A valid retained dependency keeps its reservation. A current-version
certificate blocks a replacement even without a local `ReqDec`. The
[state-transition analysis](state-transitions.md) explains the missing Crem edge,
the corrected 24-delivery trace and bounded Agda coverage.

## Recovery limits

**ReqSn can overtake ReqDec and lose the carried transaction.** Confirmation
retains the decommit outputs but may lack the transaction needed by the next
request. The next leader can then issue a request that ReqSn rejects, retaining a
request marker until settlement advances the version. The
[phase analysis and executable example](state-transitions.md#unresolved-a-certificate-without-its-carried-transaction)
record this unresolved case.

**Retry expiry can strand work.** A leader can exhaust a dependent transaction's
retry budget before receiving its parent. Other parties may retain the child
while no message remains to wake that leader. Re-submitting the child wakes it.
The [saved replay](traces/expired-dependency-v2.json) exercises this behavior.
Finite retries provide no general liveness guarantee.

**An invalid partial acknowledgement blocks replacement.** Partial signatures
are retained before individual verification. Final aggregate verification rejects
an invalid set, but a corrected signature from an already recorded party gets
`SnapshotAlreadySigned`. Closing is the protocol exit; automatic replacement is
not provided. Partial collection also records `PartySignedSnapshot` twice; map
aggregation makes the repeated event idempotent.

**Replay trusts retained events.** It does not repeat admission or cryptographic
checks, and restoring a checkpoint does not repair inconsistent contents.
Live request and confirmation evidence are intentionally absent from persisted
event encodings.

**Confirmation evidence trusts the caller's key selection.** The opaque token
binds signed bytes to the candidate, but not the verification keys to the Head's
parties. Current rules supply the correct keys. The
[API witness](adversarial-review.md#verification-evidence-has-an-incomplete-context)
shows why this remains an in-process trust boundary.

The retained retry and acknowledgement policies are recorded beside [ReqTx](../../hydra-node/src/Hydra/HeadLogic/Rules/ReqTx.hs)
and [AckSn](../../hydra-node/src/Hydra/HeadLogic/Rules/AckSn.hs). Changing them needs
explicit recovery, resource, and scheduling rules.

## Formal coverage

The independent [Agda reference](../../spec/src/Hydra/Protocol/OffChainReference.agda)
checks selected terminal outcomes, event/effect order, input admission, phase
projections, decommit readiness and reservation, settlement versions, and bounded
capacity selection/pruning. Each projection states its exclusions; it does not
prove the complete Haskell node.

In particular, the bounded snapshot-round model excludes incremental actions,
pruning payloads, collisions, capacity overflow, and scheduling. Separate bounded
references cover parts of those domains. The small-set capacity model excludes
decommits; the settlement projection excludes payload identity. Real ledger and
cryptographic implementation, chain validity, crash durability, and general
distributed progress remain outside these checks.

See [validation](validation.md) for test locations and commands, and
[tracing and exploration](tools.md) for executable examples.
