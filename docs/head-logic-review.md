# Head logic: Crem and Program

The node handles every input through a crem workflow whose actions are `Program`s.
Crem describes the phases and the data available at each phase. Program describes
the decisions, ledger and signature queries, staged events, and effects. One
interpreter runs those instructions. Separate crem machines apply the retained
events to `NodeState`.

This is the implemented design on this branch. Network messages, persisted event
encodings, and checkpoint formats are unchanged by the conversion. The conversion and decommit fixes are separate commits; deferred recovery
policies are recorded in the [bug ledger](docs/dev/architecture/head-logic-experiments/bugs.md).

## Read the design

- [Programs inside crem](head-logic-crem-workflows.md): input execution, rule modules, terminal semantics, and tests.
- [Snapshot and transaction owners](head-logic-coordinated.md): shared live/replay transitions, typed component ports and the decommit fixes.
- [Retained-event machines](head-logic-crem.md): lifecycle, deposits, snapshot coordination, and replay.
- [All diagrams](head-logic-diagrams.md): every `.mmd` file rendered with an explanation.
- [Experiments](docs/dev/architecture/head-logic-experiments/README.md): comparisons, counterexamples, measurements, resolved replay, and earlier proposals.
- [Generating head logic from Agda](docs/dev/architecture/head-logic-experiments/agda-generated.md): a speculative extension, with no generated production rules in this spike.

## Code to start with

Read these in order:

1. [`HeadLogic.update` and `updateProgram`](../hydra-node/src/Hydra/HeadLogic.hs) connect the node to the workflow and interpreter.
2. [`Rules/Input.hs`](../hydra-node/src/Hydra/HeadLogic/Rules/Input.hs) checks client input size and synchronisation before routing by head state.
3. [`Rules/ReqTx.hs`](../hydra-node/src/Hydra/HeadLogic/Rules/ReqTx.hs) is a complete rule. Its actions use ordinary monadic Haskell; the phase types carry the ledger result between steps.
4. [`Workflow.hs`](../hydra-node/src/Hydra/HeadLogic/Workflow.hs) drives crem until completion. [`Program.hs`](../hydra-node/src/Hydra/HeadLogic/Program.hs) defines the instructions; [`Interpreter.hs`](../hydra-node/src/Hydra/HeadLogic/Interpreter.hs) supplies primitives and buffers events and effects.
5. [`Machine/Coordinated.hs`](../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated.hs) composes the snapshot and transaction owners. ReqTx, snapshot, settlement and side-load rules stage their changes through these same transitions.
6. [`Machine.hs`](../hydra-node/src/Hydra/HeadLogic/Machine.hs) connects deposit ownership, lifecycle and clock; the open lifecycle state contains the snapshot/transaction composition. Recovery uses these machines without re-running input rules or cryptography.

For a larger rule, read [`ReqSn.hs`](../hydra-node/src/Hydra/HeadLogic/Rules/ReqSn.hs)
and [`AckSn.hs`](../hydra-node/src/Hydra/HeadLogic/Rules/AckSn.hs). For chain interaction,
read [`Settlement.hs`](../hydra-node/src/Hydra/HeadLogic/Rules/Settlement.hs) and
[`Repost.hs`](../hydra-node/src/Hydra/HeadLogic/Rules/Repost.hs).

Rendering lives in the development library's
[`Mermaid.hs`](../hydra-node/testlib/Test/Hydra/HeadLogic/Mermaid.hs).
The production rules have no rendering code, debugger configuration, or fault
injection switch. Signing keys belong to interpreter configuration, outside the
public rule environment and traces.

## What the evidence establishes

The migration tests compare complete ordered outcomes, retained state, serialized
events and checkpoints, and recorded primitive replay with frozen implementations.
The existing protocol, simulation and persistence tests still exercise the node.
Agda independently checks the bounded decisions described in the
[specification](../spec/src/Hydra/Protocol/OffChain.lagda.typ); it does not prove the
Haskell implementation or distributed progress.

The [measurements](docs/dev/architecture/head-logic-experiments/measurements.md)
separate instruction interpretation, crem workflows, event aggregation, and
whole-node throughput. Earlier cluster results predate the crem conversion;
they are not a TPS measurement of the final design.

Before a merge, decide which frozen migration fixtures to keep and review the
remaining recovery policies separately. Wider workload and deployment testing
should follow the controlled measurements here.
