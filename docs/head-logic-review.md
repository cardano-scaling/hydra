# Head logic: Crem and Program

The node handles every input through a crem workflow whose actions are `Program`s.
Crem describes the phases and the data available at each phase. Program describes
the decisions, ledger and signature queries, staged events, and effects. One
interpreter runs those instructions. Separate crem machines apply the retained
events to `NodeState`.

Network message formats, persisted event encodings, and checkpoint formats are
unchanged. [Protocol checks and recovery limits](head-logic/checks.md) describe
the guarantees enforced by the rules and the recovery policies they retain.

## Read the design

- [Programs inside crem](head-logic-crem-workflows.md): input execution, rule modules, terminal semantics, and tests.
- [Snapshot and transaction owners](head-logic-coordinated.md): shared live/replay transitions, typed component ports, and decommit reservations.
- [Choosing the state transitions](head-logic/state-transitions.md): local intent, certificate authority, legitimate adoption paths, and what the topology excludes.
- [Retained-event machines](head-logic-crem.md): lifecycle, deposits, snapshot coordination, and replay.
- [Diagrams](head-logic-diagrams.md): current phase graphs and executions, with a complete source index.
- [Tracing and exploration](head-logic/tools.md): runnable examples, primitive replay, and bounded searches.
- [Snapshot capacity](head-logic/capacity.md): selection policy, checked proposals, and capacity/progress tests.
- [Performance](head-logic/measurements.md): complete rounds, event application, and three-node throughput.
- [Validation](head-logic/validation.md): test coverage, verification results, and commands.
- [Adversarial review](head-logic/adversarial-review.md): findings, their repairs, remaining maintenance costs, and security limits.
- [Local Crem API experiment](head-logic/crem-api-sketch.md): the library changes used throughout the workflows and state owners, exact comparisons, and a reproducible patch.

## Code to start with

Read these in order:

1. [`HeadLogic.update` and `updateProgram`](../hydra-node/src/Hydra/HeadLogic.hs) connect the node to the workflow and interpreter.
2. [`Rules/Input.hs`](../hydra-node/src/Hydra/HeadLogic/Rules/Input.hs) checks client input size and synchronisation before routing by head state.
3. [`Rules/ReqTx.hs`](../hydra-node/src/Hydra/HeadLogic/Rules/ReqTx.hs) is a complete rule. Its actions use ordinary monadic Haskell; a ledger reply selects the next phase and the data it carries.
4. [`Workflow.hs`](../hydra-node/src/Hydra/HeadLogic/Workflow.hs) drives crem until completion. [`Program.hs`](../hydra-node/src/Hydra/HeadLogic/Program.hs) defines the instructions; [`Interpreter.hs`](../hydra-node/src/Hydra/HeadLogic/Interpreter.hs) supplies primitives and buffers events and effects.
5. [`Machine/Coordinated.hs`](../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated.hs) composes the snapshot and transaction owners. ReqTx, snapshot, settlement and side-load rules stage their changes through these same transitions.
6. [`Machine.hs`](../hydra-node/src/Hydra/HeadLogic/Machine.hs) connects deposit ownership, lifecycle and clock; the open lifecycle state contains the snapshot/transaction composition. Recovery uses these machines without re-running input rules or cryptography.

For request selection, read [`SnapshotRequest`](../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotRequest.hs),
[`SendSnapshot`](../hydra-node/src/Hydra/HeadLogic/Rules/SendSnapshot.hs), and
[`CapacitySpec`](../hydra-node/test/Hydra/HeadLogic/CapacitySpec.hs) together: selection,
owner acceptance, and an independent check of the resulting request.

For a larger rule, read [`ReqSn.hs`](../hydra-node/src/Hydra/HeadLogic/Rules/ReqSn.hs)
and [`AckSn.hs`](../hydra-node/src/Hydra/HeadLogic/Rules/AckSn.hs). For chain interaction,
read [`Settlement.hs`](../hydra-node/src/Hydra/HeadLogic/Rules/Settlement.hs) and
[`Repost.hs`](../hydra-node/src/Hydra/HeadLogic/Rules/Repost.hs).

Rendering lives in the development library's
[`Mermaid.hs`](../hydra-node/testlib/Test/Hydra/HeadLogic/Mermaid.hs).
The production rules have no rendering code, debugger configuration, or fault
injection switch. Signing keys belong to interpreter configuration, outside the
public rule environment and traces.

The [ReqSn-before-ReqDec regression](head-logic/state-transitions.md#fixed-a-certificate-without-its-carried-transaction)
shows a bug exposed by the ownership analysis, the repair, and a component trace.
It also explains why this supports using Crem without establishing that Crem is
necessary.

## What the evidence establishes

The compatibility tests compare complete ordered outcomes, retained state, serialized
events and checkpoints, and recorded primitive replay with frozen implementations.
The existing protocol, simulation and persistence tests still exercise the node.
Agda independently checks the bounded decisions described in the
[specification](../spec/src/Hydra/Protocol/OffChain.lagda.typ); it does not prove the
Haskell implementation or distributed progress.

The [measurements](head-logic/measurements.md)
cover the complete input and state-owner composition, retained-event application,
and three-node throughput, including allocation costs as well as elapsed time. The
design adds allocations and reduces TPS in the small controlled workload; this
cost belongs in the merge decision.

Before merging, profile the allocation cost and repeat the measurements under
wider workloads. The retained recovery policies need separate protocol decisions.

Archived investigations are in [prior-spike](prior-spike/README.md), including
the speculative [Agda generation proposal](prior-spike/agda-generated.md).
